#===================================================================
# Goal: map of treated vs. control municipalities - FARC exposure (DiD)
#       Choropleth of Colombian municipalities (DANE MGN 2021) shaded by
#       treatment status. Produces two maps:
#         (1) Treated vs. control  (presence: attacks_pc > 0)
#         (2) Three-arm intensity  (unexposed / low / high)
#       Join key: DIVIPOLA 5-digit code (MPIO_CDPMP in the MGN geojson <-> cod_mpio).
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(sf)

#===================================================================
# Setup (paths + shared functions)
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. INPUTS  -------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Municipal geometry: DANE Marco Geoestadístico Nacional (MGN) 2021, municipal
# layer, at 1. Data/Municipality/mpio.geojson. It has 1,121 municipalities and
# the 5-digit DIVIPOLA code in the field MPIO_CDPMP.
geom_path <- file.path(data_mun, "mpio.geojson")

# Frozen treatment cross-section (one row per municipality), saved by
# 06_treatment_identification.R. Must contain: cod_mpio, treat_presence, intensity.
xs <- readRDS(file.path(data_final, "exposure_crosssection.rds"))
length(unique(xs$cod_mpio))

#===================================================================
# 1. GEOMETRY 
#===================================================================
muni <- st_read(geom_path, quiet = TRUE) |>
  mutate(cod_mpio = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"))

#===================================================================
# 2. JOIN TREATMENT 
#===================================================================
# Standardize the key on your side too (guards against lost leading zeros).

xs <- xs |> mutate(cod_mpio = str_pad(as.character(cod_mpio), 5, pad = "0"))

map_df <- muni |>
  right_join(st_drop_geometry(xs) |>
              select(cod_mpio, treat_presence, intensity),
            by = "cod_mpio") |>
  mutate(
    # municipalities not matched (e.g. the 2 dropped for missing population or not considering in the analysis) 
    treat_presence = factor(ifelse(is.na(treat_presence), 0L, treat_presence),
                            levels = c(0, 1), labels = c("Control", "Treated")),
    intensity = factor(ifelse(is.na(as.character(intensity)), "unexposed",
                              as.character(intensity)),
                       levels = c("unexposed", "low", "high"))
  )

n_treat <- sum(map_df$treat_presence == "Treated")
n_ctrl  <- sum(map_df$treat_presence == "Control")

#===================================================================
# 3. MAP 1 — TREATED vs. CONTROL (FARC presence) 
#===================================================================

m1 <- ggplot(map_df) +
  geom_sf(aes(fill = treat_presence), colour = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = c("Control" = "#e1e0d9", "Treated" = farc_palette[["FARC-exposed"]]),
    name = NULL,
    labels = c(paste0("Control (", n_ctrl, ")"),
               paste0("Treated (", n_treat, ")"))
  ) +
  labs(title = "FARC exposure by municipality, 2011–2014",
       subtitle = "Treated = at least one FARC-attributed attack per 10,000 inhabitants",
       caption = "Source: SIEVCAC conflict registry; DANE MGN 2021 municipal boundaries.") +
  theme_void(base_size = 12) +
  theme(legend.position = c(0.15, 0.15),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle = element_text(colour = "#52514e", size = rel(0.9)),
        plot.caption = element_text(colour = "#898781", size = rel(0.7), hjust = 0),
        legend.text = element_text(colour = "#52514e"))

ggsave(file.path(results_img, "map_treated_control.png"), m1, width = 8, height = 10,
       dpi = 300, bg = "white")

#===================================================================
# 4. MAP 2 — THREE-ARM INTENSITY  (High and Low exposure)
#===================================================================

m2 <- ggplot(map_df) +
  geom_sf(aes(fill = intensity), colour = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = c("unexposed" = "#e1e0d9", "low" = farc_palette[["Low"]],
               "high" = farc_palette[["High"]]),
    name = "FARC intensity",
    labels = c("Unexposed", "Low (≤ median)", "High (> median)")
  ) +
  labs(title = "FARC exposure intensity by municipality, 2011–2014",
       subtitle = "Low/high split at the median of attacks per capita among exposed municipalities",
       caption = "Source: SIEVCAC conflict registry; DANE MGN 2021 municipal boundaries.") +
  theme_void(base_size = 12) +
  theme(legend.position = c(0.18, 0.18),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle = element_text(colour = "#52514e", size = rel(0.9)),
        plot.caption = element_text(colour = "#898781", size = rel(0.7), hjust = 0),
        legend.title = element_text(face = "bold", colour = "#0b0b0b"),
        legend.text = element_text(colour = "#52514e"))

ggsave(file.path(results_img, "map_intensity_three_arm.png"), m2, width = 8, height = 10,
       dpi = 300, bg = "white")

# Quick check that the join was complete with identified municipalities 1108
cat("Municipalities in map:", nrow(map_df),
    "| matched to treatment:", sum(!is.na(map_df$cod_mpio %in% xs$cod_mpio)), "\n")
cat("Treated:", n_treat, "| Control:", n_ctrl, "\n")
