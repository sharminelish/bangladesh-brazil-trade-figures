# =============================================================================
#  South Asian exporters vs other suppliers: import-weighted MFN tariff faced
#  in Brazil and Argentina. Compiled by Elish Sharmin. Views are personal.
#
#  Packages: install.packages(c("ggplot2", "ragg", "systemfonts", "scales"))
#  Run from the project folder:  source("R/fig_tariff_comparison.R")
#  Output (in output/): fig_tariff_comparison.png (300 dpi) and .pdf
#
#  Data: author's estimates. Each supplier's imports by HS chapter (UN Comtrade,
#  2025; Argentina-Vietnam 2024) weighted by the importer's 2025 MFN simple
#  averages per WTO product group (WTO Tariff Profiles). "All suppliers" = the
#  importer's overall trade-weighted MFN average reported by the WTO (2025).
#  India excludes India-MERCOSUR PTA preferences. Germany shown as the largest
#  EU supplier. Trade remedies (e.g. anti-dumping duties) are not included.
# =============================================================================

library(ggplot2)
library(scales)

PALETTE <- "ocean"       # "ocean", "earth" or "contrast"

palettes <- list(
  ocean    = list(sa = "#2563EB", sa_aggr = "#0B1F3A", other = "#A5B4C4", world = "#C2410C"),
  earth    = list(sa = "#4D7C0F", sa_aggr = "#3F2A14", other = "#C8BFAF", world = "#6D28D9"),
  contrast = list(sa = "#F59E0B", sa_aggr = "#1E1B4B", other = "#C4C4CC", world = "#BE123C"))
pal <- palettes[[PALETTE]]

pick_font <- function(cands) {
  fams <- unique(systemfonts::system_fonts()$family)
  hit <- cands[cands %in% fams]; if (length(hit)) hit[1] else "sans"
}
FONT <- pick_font(c("Inter", "Source Sans 3", "IBM Plex Sans", "Segoe UI", "Lato", "Arial"))

# ---- Data (%) ----------------------------------------------------------------
d <- data.frame(
  market   = rep(c("Brazil", "Argentina"), each = 9),
  supplier = rep(c("Bangladesh", "South Asia excl. India", "Pakistan", "Sri Lanka",
                   "Vietnam", "China", "Germany (EU)", "All suppliers", "India"), 2),
  tariff   = c(33.2, 28.0, 22.2, 22.6, 13.3, 11.8,  9.8,  9.4,  8.2,    # Brazil
               32.6, 25.4, 24.6, 17.4, 14.7, 12.8, 10.5, 12.3, 10.3)    # Argentina
)
d$type <- ifelse(d$supplier %in% c("Bangladesh", "Pakistan", "Sri Lanka", "India"), "South Asia",
          ifelse(d$supplier == "South Asia excl. India", "South Asia aggregate",
          ifelse(d$supplier == "All suppliers", "World", "Other supplier")))
d$fill <- c("South Asia" = pal$sa, "South Asia aggregate" = pal$sa_aggr,
            "Other supplier" = pal$other, "World" = pal$world)[d$type]
d$market <- factor(d$market, levels = c("Brazil", "Argentina"))

# order suppliers by their Brazil tariff (highest at top)
ord <- d[d$market == "Brazil", ]
ord <- ord$supplier[order(ord$tariff)]
d$supplier <- factor(d$supplier, levels = ord)
d$face <- ifelse(d$type %in% c("South Asia aggregate", "World"), "bold", "plain")
avg <- data.frame(market = factor(c("Brazil", "Argentina"), levels = levels(d$market)),
                  x = c(9.4, 12.3))

# ---- Plot --------------------------------------------------------------------
p <- ggplot(d, aes(x = tariff, y = supplier)) +
  geom_vline(data = avg, aes(xintercept = x), colour = pal$world, linewidth = 0.5,
             linetype = "dashed") +
  geom_col(aes(fill = fill), width = 0.68) +
  scale_fill_identity() +
  geom_text(aes(x = tariff - 0.5, label = sprintf("%.1f%%", tariff), fontface = face,
                colour = ifelse(type == "Other supplier", "#1F2933", "white")),
            hjust = 1, family = FONT, size = 3.3) +
  scale_colour_identity() +
  facet_wrap(~ market, nrow = 1) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0, 35, 10),
                     labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Clothing exporters pay up to three times the tariff faced by other major suppliers",
    subtitle = "Estimated import-weighted MFN tariff by supplier, Brazil and Argentina, 2025. Dashed line: average for all suppliers",
    x = NULL, y = NULL,
    caption  = paste0(
      "Author's estimate: each supplier's imports by HS chapter (UN Comtrade; Argentina–Vietnam 2024) weighted by the importer's 2025 MFN averages\n",
      "per WTO product group (WTO Tariff Profiles). 'All suppliers' = WTO trade-weighted MFN average. India excludes PTA preferences; Germany shown\n",
      "as the largest EU supplier. Anti-dumping and other trade-remedy duties are not included. Compiled by Elish Sharmin. Views are personal.")
  ) +
  theme_minimal(base_family = FONT, base_size = 11) +
  theme(
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = 14, colour = "#0B1F3A", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9.8, colour = "#52606D", margin = margin(b = 12)),
    plot.caption  = element_text(size = 7.3, colour = "#7B8794", hjust = 0, lineheight = 1.15,
                                 margin = margin(t = 12)),
    strip.text    = element_text(face = "bold", size = 11.5, colour = "#1F2933", hjust = 0),
    axis.text.y   = element_text(size = 10, colour = "#1F2933"),
    axis.text.x   = element_text(size = 8.5, colour = "#7B8794"),
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#EEF1F4", linewidth = 0.4),
    panel.spacing.x = unit(1.4, "lines"),
    plot.margin = margin(18, 20, 14, 14),
    plot.background = element_rect(fill = "white", colour = NA)
  )

dir.create("output", showWarnings = FALSE)
ragg::agg_png("output/fig_tariff_comparison.png", width = 10, height = 5.8, units = "in", res = 300)
print(p); invisible(dev.off())
ggsave("output/fig_tariff_comparison.pdf", p, width = 10, height = 5.8, device = grDevices::cairo_pdf)
message("Saved output/fig_tariff_comparison.png and .pdf | font: ", FONT)
