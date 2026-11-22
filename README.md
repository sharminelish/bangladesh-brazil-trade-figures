# Bangladesh–Brazil trade figures (R)

Reproducible R code for two data visualisations on Bangladesh's trade with Brazil and MERCOSUR.

Compiled by **Elish Sharmin**. Views are personal.

| Figure | Script | Output |
|---|---|---|
| Which Brazilian states buy the most from Bangladesh? (bubble map, 2025) | `R/map_brazil_states_bangladesh.R` | `output/map_brazil_states_bangladesh.png` / `.pdf` |
| Clothing exporters pay up to three times the tariff faced by other major suppliers (Brazil and Argentina, 2025) | `R/fig_tariff_comparison.R` | `output/fig_tariff_comparison.png` / `.pdf` |

## Project structure

```
bangladesh-brazil-trade-figures/
├── R/
│   ├── map_brazil_states_bangladesh.R   # state bubble map (Comex Stat)
│   └── fig_tariff_comparison.R          # import-weighted MFN tariff comparison
├── data/                                # put the Comex Stat download here (not tracked)
│   └── README.md
├── output/                              # figures are written here (not tracked)
├── .gitignore
├── LICENSE
└── README.md
```

## Requirements

R 4.3 or later. Install the packages once:

```r
install.packages(c("data.table", "readxl", "sf", "ggplot2", "ggrepel",
                   "ragg", "systemfonts", "scales"))
# optional: install.packages("geobr")  # otherwise the map script downloads an open
#                                        # Brazil states GeoJSON automatically
```

## How to run

Open R (or RStudio) in the project folder, then:

```r
setwd("path/to/bangladesh-brazil-trade-figures")

# 1. Tariff comparison (data are embedded in the script)
source("R/fig_tariff_comparison.R")

# 2. State map (needs the Comex Stat file in data/ first; see data/README.md)
source("R/map_brazil_states_bangladesh.R")
```

## Data and methods

**State map.** Comex Stat (MDIC/SECEX), imports from Bangladesh in 2025, FOB US$, by *UF do Produto* (the importer's state) and HS chapter. Each state is coloured by its largest product group:

- HS 61: knitted apparel
- HS 62: woven apparel
- HS 50–60, 63, 65: jute, textiles and made-ups
- HS 41–43, 64: leather and footwear
- Everything else: other products

Bubbles are placed with a small force layout so they do not overlap. State-level figures show where the importer is registered. That reflects state import-tax incentives and port access as well as final consumption.

**Tariff comparison.** These are author's estimates. Each supplier's imports by HS chapter (UN Comtrade, 2025; Argentina–Vietnam 2024) are weighted by the importer's 2025 MFN simple average for each WTO product group (WTO Tariff Profiles). "All suppliers" is the importer's overall trade-weighted MFN average reported by the WTO. India's figure excludes India–MERCOSUR PTA preferences. Trade remedies such as anti-dumping duties are not included.

## Sources

- Comex Stat, Ministry of Development, Industry, Trade and Services (MDIC): https://comexstat.mdic.gov.br
- UN Comtrade: https://comtradeplus.un.org
- WTO Tariff Profiles: https://www.wto.org/english/res_e/statis_e/tariff_profiles_list_e.htm
- Brazil state borders: Code for Germany, *click_that_hood* (brazil-states.geojson), or the `geobr` package (IPEA)

## Licence

Code released under the MIT Licence (see `LICENSE`). Data remain subject to their original providers' terms.
