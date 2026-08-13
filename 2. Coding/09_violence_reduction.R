#===================================================================
# Goal: Violence-reduction table and figure (first-stage validation)
#       - Table analogous to Table 1 in Prem, Vargas & Namen (2023):
#         average cases/victims by violence type, pre- vs. post-
#         ceasefire, for FARC-exposed vs. non-exposed municipalities,
#         with a DID test of whether the decline differs by group.
#       - Companion figure: total violence events over time, by
#         exposure status.
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: July 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(scales)
library(fixest)

#===================================================================
# Setup (paths + shared functions)
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Data
df <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))

# cease (year >= 2014) is already built in 06_treatment_identification.R and
# matches the Pre (2011-2014) / Post (2015-2023) split used for the exposure
# window throughout this project.

#===================================================================
# PART A. TABLE — cases and victims by violence type, Pre/Post, by group
#===================================================================

# All 11 SIEVCAC violence categories built in 02_preprocess_violence.R.
# n_eventos_XX_1 = FARC-attributed; _0 = all other perpetrators
# (including unidentified, per process_data_violence()). Summing 1+0 gives
# total violence of that type regardless of perpetrator, matching how
# Table 1 in Prem, Vargas & Namen (2023) is constructed.
violence_cats <- c(
  WA = "War actions",       DP = "Damage to settlements",
  AS = "Selective murders",  AT = "Terrorist attacks",
  DB = "Damage to property", DF = "Forced disappearance",
  MI = "Antipersonnel mines",MA = "Massacres",
  RU = "Recruitment",        SE = "Kidnappings",
  VS = "Sexual violence"
)

did_pvalue <- function(outcome_var, data = df) {
  fml <- as.formula(sprintf("%s ~ treat_presence*cease", outcome_var))
  m <- feols(fml, data = data, cluster = ~cod_mpio)
  pvalue(m)["treat_presence:cease"]
}

build_row <- function(code, label, metric, suffix) {
  var1 <- paste0(suffix, "_", code, "_1")
  var0 <- paste0(suffix, "_", code, "_0")
  tmp  <- df[[var1]] + df[[var0]]

  means <- tibble(treat_presence = df$treat_presence, cease = df$cease, value = tmp) |>
    group_by(treat_presence, cease) |>
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

  g0_pre  <- means$mean_value[means$treat_presence == 0 & means$cease == 0]
  g0_post <- means$mean_value[means$treat_presence == 0 & means$cease == 1]
  g1_pre  <- means$mean_value[means$treat_presence == 1 & means$cease == 0]
  g1_post <- means$mean_value[means$treat_presence == 1 & means$cease == 1]

  df_tmp <- df |> mutate(.tmp_outcome = tmp)
  p_val  <- did_pvalue(".tmp_outcome", data = df_tmp)

  # NA (not NaN/Inf) when the pre-period mean is 0 -- a handful of rare
  # violence types (e.g. terrorist attacks, damage to settlements) were
  # essentially never recorded in unexposed municipalities pre-ceasefire,
  # so a % change is undefined rather than a real (near-)-100% decline.
  safe_pct_change <- function(pre, post) if (pre == 0) NA_real_ else (post - pre) / pre

  tibble(
    category        = label,
    metric          = metric,
    nofarc_pre      = g0_pre,
    nofarc_post     = g0_post,
    nofarc_change   = safe_pct_change(g0_pre, g0_post),
    farc_pre        = g1_pre,
    farc_post       = g1_post,
    farc_change     = safe_pct_change(g1_pre, g1_post),
    p_value_diff    = p_val
  )
}

table_cases <- imap_dfr(violence_cats, ~build_row(.y, .x, "Cases", "n_eventos"))
#table_victims <- imap_dfr(violence_cats, ~build_row(.y, .x, "Victims", "victims"))

cat("\n=== Violence reduction, by FARC exposure (2011-2014 vs. 2015-2023) ===\n")
print(as.data.frame(table_cases), digits = 3)

saveRDS(table_cases, paste0(data_final, "/violence_reduction_table.rds"))

#===================================================================
# PART B. FIGURE — total violence events over time, by exposure status
#===================================================================

event_cols_1 <- paste0("n_eventos_", names(violence_cats), "_1")
event_cols_0 <- paste0("n_eventos_", names(violence_cats), "_0")

df_trend <- df |>
  mutate(total_all_events = rowSums(across(all_of(event_cols_1)), na.rm = TRUE) +
                             rowSums(across(all_of(event_cols_0)), na.rm = TRUE)) |>
  group_by(year, treat_presence) |>
  summarise(mean_events = mean(total_all_events, na.rm = TRUE), .groups = "drop")

p_violence_trend <- df_trend |>
  mutate(treat_presence = factor(treat_presence, levels = c(0, 1),
                                  labels = c("Unexposed", "FARC-exposed"))) |>
  ggplot(aes(year, mean_events, colour = treat_presence)) +
  scale_x_continuous(
    breaks = seq(2010, 2023, by = 2),
    limits = c(2010, 2023)
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  add_ceasefire_line(2014.5) +
  scale_colour_manual(values = farc_palette) +
  labs(colour = NULL, x = NULL, y = "Mean violent events per municipality",
       title = "Violence trends before and after the FARC ceasefire",
       subtitle = "All 11 SIEVCAC violence categories, any perpetrator, municipality-year panel 2011–2023") +
  theme_capstone()

ggsave(paste0(results_img, "/violence_reduction_trend.png"), p_violence_trend,
       width = 8, height = 5, dpi = 300)
