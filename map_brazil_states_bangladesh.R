# =============================================================================
#  Which Brazilian states buy the most from Bangladesh?
#  Proportional-bubble map of Brazil's 2025 imports from Bangladesh by state,
#  coloured by each state's top product group. Data: Comex Stat (MDIC/SECEX).
#  Compiled by Elish Sharmin. Views are personal.
#
#  STEP 1 - Download the data from Comex Stat (https://comexstat.mdic.gov.br)
#    Geral > Importação
#      Período: 2025 (Jan-Dez), Ano a ano
#      Filtros: País = Bangladesh
#      Detalhamentos (in this order): UF do Produto, SH2 (Capítulo)
#      Valores: Valor FOB (US$)
#    Click "Consultar", then download as CSV (or Excel). Save it as
#    data/comexstat_bangladesh_2025.csv inside this project.
#
#  STEP 2 - In the R Console:
#    install.packages(c("data.table", "readxl", "sf", "geobr",
#                       "ggplot2", "ggrepel", "ragg", "systemfonts", "scales"))  # once
#    setwd("path/to/bangladesh-brazil-trade-figures")   # the project folder
#    source("R/map_brazil_states_bangladesh.R")
#
#  Output (in output/): map_brazil_states_bangladesh.png (300 dpi) and .pdf,
#          bangladesh_imports_by_state_2025.csv (the numbers behind the map)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(ggrepel); library(scales)
})

# ---- 0. Settings -------------------------------------------------------------
dir.create("output", showWarnings = FALSE)
INPUT     <- "data/comexstat_bangladesh_2025.csv"   # your Comex Stat file (.csv or .xlsx)
PARTNER   <- "Bangladesh"
YEAR      <- 2025
MAP_FILE  <- NULL     # leave NULL to download state borders with geobr;
                      # or give a local .geojson/.gpkg of Brazilian states

# Product groups by HS chapter (edit to suit)
group_of <- function(ch) {
  ch <- suppressWarnings(as.integer(ch))
  fcase(ch == 61,                   "Knitted apparel",
        ch == 62,                   "Woven apparel",
        ch %in% c(50:60, 63, 65),   "Jute, textiles & made-ups",
        ch %in% c(41:43, 64),       "Leather & footwear",
        default =                   "Other products")
}
group_cols <- c("Knitted apparel"           = "#2563EB",   # bright blue
                "Woven apparel"             = "#F97316",   # bright orange
                "Jute, textiles & made-ups" = "#16A34A",   # bright green
                "Leather & footwear"        = "#DB2777",   # bright pink
                "Other products"            = "#8B5CF6")   # bright violet

# ---- 1. Read the Comex Stat file --------------------------------------------
read_comex <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, "\nCheck getwd() and INPUT.")
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    x <- as.data.table(readxl::read_excel(path, col_types = "text"))
  } else {
    x <- tryCatch(fread(path, colClasses = "character", encoding = "UTF-8"),
                  error = function(e) fread(path, colClasses = "character", encoding = "Latin-1"))
  }
  x
}
raw <- read_comex(INPUT)

norm <- function(s) {
  s <- iconv(s, to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]", "", tolower(s))
}
find_col <- function(opts, required = TRUE) {
  n <- norm(names(raw))
  for (o in norm(opts)) { h <- which(n == o); if (length(h)) return(names(raw)[h[1]]) }
  for (o in norm(opts)) { h <- grep(o, n, fixed = TRUE); if (length(h)) return(names(raw)[h[1]]) }
  if (required) stop("Column not found (looked for: ", paste(opts, collapse = ", "),
                     ")\nColumns in file: ", paste(names(raw), collapse = " | "))
  NA_character_
}
c_uf   <- find_col(c("UF do Produto", "UF", "Estado", "state"))
# SH2: pick the column whose values look like chapter codes (e.g. "61", "061", "61 - Apparel")
sh2_candidates <- names(raw)[grepl("sh2|chapter|capitulo|cap", norm(names(raw)))]
code_share <- vapply(sh2_candidates, function(cn)
  mean(grepl("^\\s*[0-9]{1,4}(\\b|\\s|-|$)", raw[[cn]]), na.rm = TRUE), 0)
c_sh2  <- if (length(code_share) && max(code_share) > 0.5) names(which.max(code_share)) else NA_character_
c_desc <- setdiff(sh2_candidates, c_sh2)[1]               # description column, if any
if (is.na(c_sh2) && is.na(c_desc))
  stop("No HS chapter column found. Columns in file: ", paste(names(raw), collapse = " | "))
message("HS chapter column used: ", ifelse(is.na(c_sh2), paste0(c_desc, " (descriptions)"), c_sh2))
c_val  <- find_col(c("Valor FOB (US$)", "Valor FOB US", "VL_FOB", "FOB", "value"))
c_year <- find_col(c("Ano", "CO_ANO", "year"), required = FALSE)
c_pais <- find_col(c("Pais", "País", "CO_PAIS", "country"), required = FALSE)

clean_sh2 <- function(v) {                    # "61", "061", "6109", "61 - Apparel" -> "61"
  digits <- regmatches(v, regexpr("[0-9]+", v))
  out <- rep(NA_character_, length(v)); out[grepl("[0-9]", v)] <- digits
  out <- ifelse(nchar(out) == 3 & substr(out, 1, 1) == "0", substr(out, 2, 3), out)
  out <- ifelse(nchar(out) >= 4, substr(out, 1, 2), out)
  sprintf("%02d", suppressWarnings(as.integer(out)))
}
desc_to_sh2 <- function(v) {                  # fallback: classify from description text
  t <- tolower(iconv(v, to = "ASCII//TRANSLIT"))
  fcase(grepl("apparel|vestuario", t) & grepl("not knit|nao de malha|exceto de malha", t), "62",
        grepl("apparel|vestuario", t), "61",
        grepl("jute|juta|textile|textil|cotton|algodao|fibre|fiber|fibra", t), "53",
        grepl("footwear|calcado|leather|couro", t), "64",
        default = "99")
}
to_num <- function(v) {                       # handles "1.234.567" and "1,234,567"
  v <- gsub("[^0-9,.-]", "", v)
  v <- ifelse(grepl("\\.[0-9]{3}(\\.|$)", v) & !grepl(",[0-9]{1,2}$", v), gsub("\\.", "", v), v)
  as.numeric(gsub(",", "", v))
}
d <- data.table(
  uf    = trimws(raw[[c_uf]]),
  sh2   = if (!is.na(c_sh2)) clean_sh2(raw[[c_sh2]]) else desc_to_sh2(raw[[c_desc]]),
  value = to_num(raw[[c_val]]),
  year  = if (is.na(c_year)) as.character(YEAR) else raw[[c_year]],
  pais  = if (is.na(c_pais)) PARTNER else raw[[c_pais]]
)
d <- d[grepl(as.character(YEAR), year) & grepl(PARTNER, pais, ignore.case = TRUE)]
d <- d[!is.na(value) & value > 0 & !is.na(sh2) & sh2 != "NA"]
if (!nrow(d)) stop("No usable rows after filtering for ", PARTNER, " ", YEAR, ".")

# ---- 2. State codes (handles names or two-letter codes) ----------------------
uf_tab <- data.table(
  abbrev = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
             "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
  name   = c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
             "Espírito Santo","Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul",
             "Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro",
             "Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina",
             "São Paulo","Sergipe","Tocantins"))
uf_tab[, key := norm(name)]
d[, key := norm(uf)]
d[nchar(uf) == 2, key := norm(uf_tab$name[match(toupper(uf), uf_tab$abbrev)])]
d <- merge(d, uf_tab[, .(key, abbrev, name)], by = "key", all.x = TRUE)
if (d[is.na(abbrev), .N]) message("Rows without a recognised state (e.g. 'Não Declarada') dropped: US$",
                                  round(d[is.na(abbrev), sum(value)] / 1e6, 2), " m")
d <- d[!is.na(abbrev)]

# ---- 3. Totals and top product group per state -------------------------------
d[, group := group_of(sh2)]
by_grp <- d[, .(value = sum(value)), by = .(abbrev, name, group)]
top    <- by_grp[order(-value)][, .SD[1], by = abbrev][, .(abbrev, top_group = group,
                                                         top_share = value)]
st     <- d[, .(value = sum(value)), by = .(abbrev, name)]
st     <- merge(st, top, by = "abbrev")
st[, top_share := round(100 * top_share / value)]
st[, share_of_total := round(100 * value / sum(value), 1)]
setorder(st, -value)
print(st)
fwrite(st, sprintf("output/bangladesh_imports_by_state_%d.csv", YEAR))

# ---- 4. Map ------------------------------------------------------------------
GEOJSON_URL <- "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/brazil-states.geojson"
if (is.null(MAP_FILE) && !requireNamespace("geobr", quietly = TRUE)) {
  MAP_FILE <- "data/brazil-states.geojson"                 # geobr not available: use open map file
  if (!file.exists(MAP_FILE)) {
    message("geobr not installed - downloading Brazil state borders (3 MB)...")
    download.file(GEOJSON_URL, MAP_FILE, mode = "wb", quiet = TRUE)
  }
  message("Using map file: ", normalizePath(MAP_FILE))
}
if (is.null(MAP_FILE)) {
  states <- geobr::read_state(code_state = "all", year = 2020, showProgress = FALSE)
  states <- states[, c("abbrev_state", "name_state", "geom")]
  names(states)[1:2] <- c("abbrev", "name_map")
} else {
  states <- st_read(MAP_FILE, quiet = TRUE)
  nm <- intersect(c("abbrev_state", "sigla", "SIGLA", "UF", "abbrev"), names(states))
  if (length(nm)) { states$abbrev <- states[[nm[1]]] } else {
    nn <- intersect(c("name_state", "name", "nome", "NOME"), names(states))[1]
    states$abbrev <- uf_tab$abbrev[match(norm(states[[nn]]), uf_tab$key)]
  }
}
states <- st_transform(states, 4674)
sf_use_s2(FALSE)
pts <- suppressWarnings(st_point_on_surface(states))
xy  <- cbind(st_drop_geometry(pts)["abbrev"], st_coordinates(pts))
st  <- merge(st, as.data.table(xy), by = "abbrev", all.x = TRUE)

pick_font <- function(cands) {
  fams <- unique(systemfonts::system_fonts()$family)
  hit <- cands[cands %in% fams]; if (length(hit)) hit[1] else "sans"
}
FONT  <- pick_font(c("Inter", "Source Sans 3", "Segoe UI", "Lato", "Arial"))
TITLE <- pick_font(c("Merriweather", "Georgia", "Cambria", "Source Serif 4", "serif"))

fmt_usd <- function(x) ifelse(x >= 1e9, paste0("$", round(x / 1e9, 1), "B"),
                       ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
                                        paste0("$", round(x / 1e3), "K")))
# ---- 4b. Bubbles that do not overlap (simple force layout, in degrees) ------
R_MAX <- 3.8                                     # radius of the largest bubble (degrees)
st[, r := R_MAX * sqrt(value / max(value))]
st[, r := pmax(r, 0.25)]                         # keep tiny states visible
st[, `:=`(X0 = X, Y0 = Y)]                       # state anchor point
GAP <- 0.25
for (it in 1:800) {
  for (i in seq_len(nrow(st) - 1)) for (j in (i + 1):nrow(st)) {
    dx <- st$X[j] - st$X[i]; dy <- st$Y[j] - st$Y[i]; dd <- sqrt(dx^2 + dy^2) + 1e-9
    ov <- st$r[i] + st$r[j] + GAP - dd
    if (ov > 0) {                                # push apart; smaller bubble moves more
      wi <- st$r[j]^2 / (st$r[i]^2 + st$r[j]^2); wj <- 1 - wi
      st$X[i] <- st$X[i] - dx / dd * ov * wi; st$Y[i] <- st$Y[i] - dy / dd * ov * wi
      st$X[j] <- st$X[j] + dx / dd * ov * wj; st$Y[j] <- st$Y[j] + dy / dd * ov * wj
    }
  }
  st[, `:=`(X = X + (X0 - X) * 0.01, Y = Y + (Y0 - Y) * 0.01)]   # gentle pull home
}
st[, moved := sqrt((X - X0)^2 + (Y - Y0)^2) > r * 0.6]
circles <- st_buffer(st_as_sf(st[, .(abbrev, X, Y, r)], coords = c("X", "Y"), crs = 4674),
                     dist = st$r, nQuadSegs = 40)
circles <- merge(circles, st[, .(abbrev, group_f = factor(top_group, levels = names(group_cols)))],
                 by = "abbrev")

# colour mode: by product group, or by value if groups could not be detected
COLOUR_BY_VALUE <- uniqueN(st$top_group) == 1 && st$top_group[1] == "Other products"
if (COLOUR_BY_VALUE) {
  message("\nNOTE: no product groups detected (all 'Other'). Colouring by import value instead.",
          "\nTo fix the groups, paste the output of  names(raw); head(raw, 3)  to Claude.")
  circles <- merge(circles, st[, .(abbrev, value)], by = "abbrev")
}

# labels: inside the bubble when the name fits, otherwise just below it
st[, label := paste0(name, "\n", fmt_usd(value))]
st[, name_wrap := vapply(name, function(n) paste(strwrap(n, width = 10), collapse = "\n"), "")]
st[, n_lines := lengths(regmatches(name_wrap, gregexpr("\n", name_wrap))) + 2]
st[, max_chars := vapply(strsplit(name_wrap, "\n"), function(x) max(nchar(x)), 1L)]
st[, inside := (2 * r >= 0.30 * pmax(max_chars, 6) + 0.4) & (2 * r >= 0.85 * n_lines + 0.3)]
st[, label_in := paste0(name_wrap, "\n", fmt_usd(value))]
st[, size_in := fifelse(r >= 2.5, 3.0, 2.5)]
big   <- st[inside == TRUE]
DEG_MM <- 4.0                                    # ~mm per degree at this figure size
st[, lab_out := fifelse(inside, "", label)]      # every bubble is an obstacle for labels
# Hand-placed nudges (degrees) for labels squeezed by the big SP/SC bubbles
st[, nx := fifelse(abbrev == "PR", -4.5, 0)]
st[, ny := fifelse(abbrev == "PR",  0.3, 0)]
total <- sum(st$value)

p <- ggplot() +
  geom_sf(data = states, fill = "#ECE8E1", colour = "white", linewidth = 0.35) +
  geom_segment(data = st[moved == TRUE], aes(x = X0, y = Y0, xend = X, yend = Y),
               colour = "#9AA5B1", linewidth = 0.35) +
  geom_point(data = st[moved == TRUE], aes(X0, Y0), size = 0.9, colour = "#7B8794") +
  { if (COLOUR_BY_VALUE)
      geom_sf(data = circles, aes(fill = value), colour = "white", linewidth = 0.7)
    else
      geom_sf(data = circles, aes(fill = group_f), colour = "white", linewidth = 0.7) } +
  geom_text(data = big, aes(X, Y, label = label_in, size = size_in), family = FONT,
            fontface = "bold", colour = "white", lineheight = 0.9) +
  scale_size_identity() +
  geom_text_repel(data = st, aes(X, Y, label = lab_out, point.size = 2 * r * DEG_MM),
                  family = FONT, size = 2.7, colour = "#1F2933", lineheight = 0.9,
                  min.segment.length = 0.4, segment.colour = "#9AA5B1", segment.size = 0.3,
                  box.padding = 0.25, point.padding = 0.1, force = 2, force_pull = 1,
                  max.overlaps = Inf, seed = 7,
                  nudge_x = st$nx, nudge_y = st$ny) +
  scale_continuous_identity(aesthetics = "point.size") +
  { if (COLOUR_BY_VALUE)
      scale_fill_gradientn(colours = c("#FACC15", "#F97316", "#E11D48", "#7C3AED"),
                           trans = "log10", labels = fmt_usd, name = "Imports (US$)",
                           guide = guide_colourbar(barheight = unit(3.2, "cm"), barwidth = unit(0.45, "cm")))
    else
      scale_fill_manual(values = group_cols, drop = TRUE, name = NULL,
                        guide = guide_legend(override.aes = list(colour = NA))) } +
  coord_sf(xlim = c(-75, -30.5), ylim = c(-35, 5.5), expand = FALSE, datum = NA) +
  labs(
    title    = "Which Brazilian states buy\nthe most from Bangladesh?",
    subtitle = sprintf("Imports from Bangladesh by state, %d (total %s). Bubble size = import value;\n%s",
                       YEAR, fmt_usd(total),
                       if (COLOUR_BY_VALUE) "colour = import value" else "colour = state's top product group"),
    caption  = "Source: Comex Stat (MDIC/SECEX), FOB values by state (UF do Produto).\nCompiled by Elish Sharmin. Views are personal."
  ) +
  theme_void(base_family = FONT) +
  theme(
    plot.title    = element_text(family = TITLE, face = "bold", size = 22, colour = "#2E4A72",
                                 lineheight = 1.0, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, colour = "#52606D", lineheight = 1.15, margin = margin(b = 4)),
    plot.caption  = element_text(size = 7.5, colour = "#7B8794", hjust = 0, lineheight = 1.15,
                                 margin = margin(t = 6)),
    plot.title.position = "plot", plot.caption.position = "plot",
    legend.position = c(0.02, 0.18), legend.justification = c(0, 0),
    legend.text = element_text(size = 9, colour = "#1F2933"),
    legend.title = element_text(size = 9, face = "bold", colour = "#1F2933"),
    legend.background = element_rect(fill = "white", colour = "#D9D3C9", linewidth = 0.4),
    legend.margin = margin(6, 8, 6, 6),
    plot.background = element_rect(fill = "#FAF6EF", colour = NA),
    plot.margin = margin(20, 20, 14, 20)
  )

out <- "output/map_brazil_states_bangladesh"
ragg::agg_png(paste0(out, ".png"), width = 8, height = 8.6, units = "in", res = 300,
              background = "#FAF6EF")
print(p); invisible(dev.off())
ggsave(paste0(out, ".pdf"), p, width = 8, height = 8.6, device = grDevices::cairo_pdf,
       bg = "#FAF6EF")
message("Saved ", out, ".png/.pdf and bangladesh_imports_by_state_", YEAR, ".csv")
