#===================================================================
# Goal: ROBUSTNESS section — does the heavy-exposure result survive?
#       Three main-text checks, all on the three-arm design that carries
#       the result:
#         (1)   stricter threshold: "high" = top QUARTILE (p75) of exposed
#               municipalities, instead of the top half (median split)
#         (2)   comparison group restricted to non-FARC-violence munis
#         (3)   differential (Wooldridge) pre-trends
#       Plus one APPENDIX check: re-including the 13 main cities dropped
#       in 06. Plus a consolidated table + forest plot of the
#       heavy-exposure ATT across every variation.
#
# Author: Juliana Aragon
# LSE MY498 Capstone - Applied Social Data Science
# Date: July 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(scales)
library(fixest)

#===================================================================
# Setup (paths + shared functions: build_exposure, farc_palette,
#        theme_capstone, add_ceasefire_line)
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Data
#   df       : primary analysis panel (13 main cities already dropped in 06)
#   base_all : panel BEFORE the city drop, used only by check (ii)
df       <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))
base_all <- readRDS(paste0(data_final, "/final_panel_2010_2023.rds"))

#===================================================================
# Global parameters
outcome  <- "pro_he"
ref_year <- 2013
dept     <- if ("cod_dep" %in% names(df)) "cod_dep" else "departamento"

# The FARC exposure window (pre-ceasefire).
exp_wind_cease <- 2010:2014
year_cease     <- 2014
main_cities <- c("11001","05001","76001","08001","13001","68001",
                 "17001","52001","66001","54001","73001","23001","50001")

#===================================================================
# Shared fitters / extractors
#===================================================================
# Static (pooled) ATT with the project's FE / weights / clustering.
fit_static <- function(rhs, data = df) {
  fml <- as.formula(sprintf("%s ~ %s | cod_mpio + %s^year", outcome, rhs, dept))
  feols(fml, data = data, weights = ~w_takers, cluster = ~cod_mpio)
}

# Event-study fitter (single treated series, ref = ref_year).
fit_es <- function(rhs_var, data = df) {
  fml <- as.formula(sprintf("%s ~ i(year, %s, ref = %d) | cod_mpio + %s^year",
                            outcome, rhs_var, ref_year, dept))
  feols(fml, data = data, weights = ~w_takers, cluster = ~cod_mpio)
}

# One coefficient -> tidy row, in percentage points of HE access.
grab_pp <- function(model, term, label) {
  ct <- as.data.frame(coeftable(model))
  tibble(spec     = label,
         att_pp   = 100 * ct[term, "Estimate"],
         se_pp    = 100 * ct[term, "Std. Error"],
         p_value  = ct[term, "Pr(>|t|)"],
         n_obs    = nobs(model))
}

# Joint pre-trends Wald test over 2010-2012 (H0: all pre coefs = 0).
pre_years <- 2010:(ref_year - 1)                 # 2010, 2011, 2012
pre_pat   <- paste(pre_years, collapse = "|")
wald_pre  <- function(model, var, label) {
  w <- fixest::wald(model, paste0("year::(", pre_pat, "):", var))
  tibble(spec = label, F_stat = as.numeric(w["stat"]),
         df1 = as.numeric(w["df1"]), df2 = as.numeric(w["df2"]),
         p_value = as.numeric(w["p"]))
}

# Baseline heavy-exposure benchmarks (from the main-results panel) so every
# robustness estimate below is read against the number it is stress-testing.
m_base_pres   <- fit_static("did_presence")                 # binary (headline, null)
m_base_arms   <- fit_static("did_low + did_high_u")         # three-arm (low & high vs. unexposed)
base_presence <- grab_pp(m_base_pres, "did_presence", "Baseline: FARC presence (binary)")
base_low      <- grab_pp(m_base_arms, "did_low",      "Baseline: low vs. unexposed")
base_high_u   <- grab_pp(m_base_arms, "did_high_u",   "Baseline: high vs. unexposed")

cat("\n=== Baseline benchmarks (three-arm decomposition + binary) ===\n")
print(as.data.frame(bind_rows(base_presence, base_low, base_high_u)))

#===================================================================
# CHECK 1.  STRICTER THRESHOLD — three-arm, "high" = top quartile (p75)
#   Baseline splits exposed municipalities low/high at the MEDIAN of the
#   per-capita attack rate. Here we move the cut to the 75th percentile among
#   exposed, so "high" is the top quartile of exposed municipalities, and
#   re-estimate the three-arm high-vs-unexposed ATT.
#===================================================================

# Defining the new threshold for high exposure definition
pos_ex <- df |> 
  distinct(cod_mpio, attacks_pc) |> 
  filter(attacks_pc > 0) |> 
  pull(attacks_pc)
q75    <- quantile(pos_ex, 0.75, names = FALSE)

# Treatment measure 
df_q <- df |>
  mutate(intensity75 = factor(case_when(attacks_pc == 0   ~ "unexposed",
                                        attacks_pc <= q75 ~ "low",
                                        TRUE              ~ "high"),
                              levels = c("unexposed", "low", "high")),
         did_low75  = cease * as.integer(intensity75 == "low"),  
         did_high75 = cease * as.integer(intensity75 == "high"))

# Number of high exposure municipalities 
n_high75 <- df_q |> 
  filter(intensity75 == "high") |> 
  distinct(cod_mpio) |> nrow()

# Fit the model - three-arm approach 
m_thr    <- fit_static("did_low75 + did_high75", data = df_q)
check_i  <- grab_pp(m_thr, "did_high75", "(1) Three-arm, high = top quartile (p75)")

# clean high-vs-unexposed pre-trends under the stricter threshold
df_q_hu  <- df_q |> 
  filter(intensity75 %in% c("unexposed", "high")) |>
  mutate(th75 = as.integer(intensity75 == "high"))
pt_i     <- wald_pre(fit_es("th75", data = df_q_hu), "th75", "(1) High(p75) vs. unexposed")

# Results for parallel trend test - Wald test
cat(sprintf("\n(1) Top-quartile threshold: q75 = %.3f attacks/10k | high identified off %d municipalities\n",
            q75, n_high75))
print(as.data.frame(check_i)); print(as.data.frame(pt_i))

#============================================================================
# CHECK (2)  NON-FARC COMPARISON GROUP
#   Restrict controls to municipalities ALSO touched by armed violence, just
#   not FARC (>=1 non-FARC-attributed event over 2010-2014). This drops the
#   never-conflict-affected controls, so the contrast is FARC-exposed vs.
#   otherwise-conflict-affected — ruling out a generic "conflict zone"
#   post-ceasefire dynamic (Prem, Vargas & Namen 2023, Sec. V.D.4).
#   Uses total_nofarc_events (02_preprocess_violence.R; same 6-cat basket).
#============================================================================

# Identification of municipalities exposed to No-FARC violence 
nonfarc_exposure <- df |>
  filter(year %in% exp_wind_cease) |>
  group_by(cod_mpio) |>
  summarise(nofarc_tot = sum(total_nofarc_events, na.rm = TRUE), .groups = "drop") |>
  mutate(nofarc_exposed = as.integer(nofarc_tot > 0))

df_conflict <- df |>
  left_join(nonfarc_exposure, by = "cod_mpio") |>
  filter(treat_presence == 1 | nofarc_exposed == 1)

cat(sprintf("\n(2) Comparison-group restriction: %d municipalities (%d FARC-exposed, %d non-FARC-only controls)\n",
            length(unique(df_conflict$cod_mpio)),
            length(unique(df_conflict$cod_mpio[df_conflict$treat_presence == 1])),
            length(unique(df_conflict$cod_mpio[df_conflict$treat_presence == 0]))))  #(399 FARC-exposed, 219 non-FARC-only controls)

# Fitting the model 
m_cg_pres <- fit_static("did_presence",         data = df_conflict)  #Binary treatment
m_cg_arms <- fit_static("did_low + did_high_u", data = df_conflict)  #Three-arm 

# clean high-vs-unexposed pre-trends on the restricted sample
df_cg_hu  <- df_conflict |> 
  filter(intensity %in% c("unexposed", "high")) |>
  mutate(thc = as.integer(intensity == "high"))

m_cg_es   <- fit_es("thc", data = df_cg_hu)

check_ii <- bind_rows(
  grab_pp(m_cg_pres, "did_presence", "(2) Non-FARC controls: FARC presence (binary)"),
  grab_pp(m_cg_arms, "did_high_u",   "(2) Non-FARC controls: high vs. unexposed")
)

# Parallel trends Wald test
pt_ii <- wald_pre(m_cg_es, "thc", "(2) Non-FARC controls: high vs. unexposed")

cat("\n=== (2) Non-FARC comparison group ===\n")
print(as.data.frame(check_ii)); print(as.data.frame(pt_ii))

#===================================================================
# CHECK (3)  DIFFERENTIAL (WOOLDRIDGE) PRE-TRENDS
#   Relax the common-trend assumption: let municipalities with different
#   exposure histories follow different LINEAR trajectories, by interacting
#   pre-ceasefire exposure (farc_logint = log attacks per 10,000) with a
#   linear cohort trend [@wooldridge2025two]. cod_mpio FE absorbs the
#   exposure main effect; dept^year FE absorbs the common year effect; the
#   interaction is the exposure-specific linear trend. 
#===================================================================

df <- df |> mutate(yr_c = year - ref_year)                  # centered linear trend

# Fitting the model - binary treatment 
m_dt_pres <- feols(
  as.formula(sprintf("%s ~ did_presence + farc_logint:yr_c | cod_mpio + %s^year",
                     outcome, dept)),
  data = df, weights = ~w_takers, cluster = ~cod_mpio)

# Fitting the model - Three-arms approach
m_dt_arms <- feols(
  as.formula(sprintf("%s ~ did_low + did_high_u + farc_logint:yr_c | cod_mpio + %s^year",
                     outcome, dept)),
  data = df, weights = ~w_takers, cluster = ~cod_mpio)

check_iii <- bind_rows(
  grab_pp(m_dt_pres, "did_presence", "(3) Diff. trends: FARC presence (binary)"),
  grab_pp(m_dt_arms, "did_high_u",   "(3) Diff. trends: high vs. unexposed")
)

cat("\n=== (3) Differential (Wooldridge) pre-trends ===\n")
print(as.data.frame(check_iii))

#===================================================================
# CONSOLIDATED SUMMARY + PLOT WITH ALL OF THE COEFFICIENTS 
#===================================================================

# check_ii / check_iii carry a binary AND a high row; keep only the high row
# for the consolidated view (the baseline binary already appears once).

high_only <- function(tab) tab |> filter(grepl("high", spec, ignore.case = TRUE))

robustness_summary <- bind_rows(
  base_presence, base_low, base_high_u,          # three-arm baseline decomposition
  check_i,                                        # (1)  high, p75 threshold
  high_only(check_ii),                            # (2)  high, non-FARC controls
  high_only(check_iii)                            # (3)  high, differential trends
) |>
  mutate(
    ci_low  = att_pp - 1.96 * se_pp,
    ci_high = att_pp + 1.96 * se_pp,
    arm     = case_when(grepl("binary", spec)                    ~ "Any (binary)",
                        grepl("low",  spec, ignore.case = TRUE)  ~ "Low",
                        TRUE                                     ~ "High"),
    arm     = factor(arm, levels = c("Any (binary)", "Low", "High")),
    survives = arm == "High" & att_pp < 0 & p_value < 0.05
  )

cat("\n=== ROBUSTNESS SUMMARY (ATT in pp; by arm) ===\n")
print(as.data.frame(robustness_summary))

saveRDS(robustness_summary, paste0(data_final, "/robustness_summary.rds"))

# Parallel trend robustness
pretrends_summary <- bind_rows(pt_i, pt_ii)   # (3) diff-trends has no pre-trends test
cat("\n=== ROBUSTNESS PRE-TRENDS (Wald, H0: pre coefs = 0) ===\n")
print(as.data.frame(pretrends_summary))
saveRDS(pretrends_summary, paste0(data_final, "/robustness_pretrends.rds"))

# Forest plot — three-arm baseline (binary, low, high) + robustness highs,
# coloured by arm (blue = any/binary, green = low, red = high).
p_forest <- robustness_summary |>
  mutate(spec = fct_rev(fct_inorder(spec))) |>
  ggplot(aes(x = att_pp, y = spec, colour = arm)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#898781", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Any (binary)" = farc_palette[["Unexposed"]],
                                 "Low"          = farc_palette[["Low"]],
                                 "High"         = farc_palette[["High"]])) +
  labs(title = "ATT across the three-arm baseline and robustness checks",
       subtitle = "Point estimate and 95% CI, common-timing weighted TWFE design",
       x = "Effect on higher-education access (pp)", y = NULL, colour = NULL,
       caption = "Robustness rows (i)-(iii) are all high-vs-unexposed. Negative = access falls.") +
  theme_capstone()

ggsave(paste0(results_img, "/robustness_forest.png"), p_forest, width = 8, height = 5.5, dpi = 300)


#===================================================================
# APPENDIX CHECK — re-including the 13 main cities dropped in 06
# Rebuild the SAME (median-split) exposure definition on the panel 
# WITH the cities and confirm the estimate holds.
#===================================================================

df_city <- base_all |> filter(!is.na(poblacion))            # keep the 13 cities this time

exp_city <- build_exposure(df_city, exp_wind_cease) |>
  filter(is.finite(attacks_pc))
med_pos_c <- median(exp_city$attacks_pc[exp_city$attacks_pc > 0])

exp_city <- exp_city |>
  mutate(
    treat_presence = as.integer(attacks_pc > 0),
    intensity = factor(case_when(
                  attacks_pc == 0         ~ "unexposed",
                  attacks_pc <= med_pos_c ~ "low",
                  TRUE                    ~ "high"),
                levels = c("unexposed", "low", "high")),
    farc_logint = log1p(attacks_pc)
  )

panel_city <- df_city |>
  inner_join(select(exp_city, cod_mpio, attacks_pc, treat_presence,
                    intensity, farc_logint), by = "cod_mpio") |>
  mutate(
    cease        = as.integer(year >= year_cease),
    did_presence = cease * treat_presence,
    did_low      = cease * as.integer(intensity == "low"),
    did_high_u   = cease * as.integer(intensity == "high")
  )

n_added <- length(intersect(unique(panel_city$cod_mpio), main_cities))
cat(sprintf("\n(Appendix) Cities rebuild: %d municipalities (of which %d are main cities re-added)\n",
            length(unique(panel_city$cod_mpio)), n_added))

appendix_cities <- bind_rows(
  grab_pp(fit_static("did_presence",         data = panel_city), "did_presence", "Appendix: +13 cities: FARC presence (binary)"),
  grab_pp(fit_static("did_low + did_high_u", data = panel_city), "did_high_u",   "Appendix: +13 cities: high vs. unexposed")
)
cat("\n=== APPENDIX: Re-including the 13 main cities ===\n")
print(as.data.frame(appendix_cities))
saveRDS(appendix_cities, paste0(data_final, "/robustness_appendix_cities.rds"))

