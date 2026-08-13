#===================================================================
# Goal: APPENDIX robustness for the sex-heterogeneity result
#       (companion to 10_heterogeneous_sex.R). Two questions:
#
#  (A) Is the larger female penalty a real fall in ABSOLUTE female
#      access, or — like the pooled result in §8.1 — an artefact of a
#      growing denominator? Decompose the heavy-exposure (high vs.
#      unexposed) effect for each sex into:
#         denominator = log(1 + female/male takers)
#         numerator   = asinh(female/male HE-entrants)  [absolute access]
#         rate        = pro_he_F / pro_he_M
#      plus a population moderation of female absolute access (does the
#      female shortfall concentrate in the small municipalities, as the
#      supply-side story in §8.2 predicts?).
#
#  (B) Is the female LEVEL as fragile as the pooled heavy-exposure
#      result? Run @RambachanRoth2023 HonestDiD (relative magnitudes)
#      on the female (and male) high-vs-unexposed event study and report
#      the breakdown value.
#
# Author: Juliana Aragon
# LSE MY498 Capstone - Applied Social Data Science
# Date: July 2026
#===================================================================

library(tidyverse)
library(scales)
library(fixest)
if (!requireNamespace("HonestDiD", quietly = TRUE)) install.packages("HonestDiD")
library(HonestDiD)

#===================================================================
# Setup + data
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))
df <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))

ref_year <- 2013
dept     <- if ("cod_dep" %in% names(df)) "cod_dep" else "departamento"
fe       <- sprintf("cod_mpio + %s^year", dept)

stopifnot(all(c("pro_he_F", "pro_he_M", "numstudents_F", "numstudents_M") %in% names(df)))

#===================================================================
# 0. Sex-specific weights, outcomes, clean sample, population moderator
#===================================================================
wsex <- df |> filter(year %in% 2010:2014) |> group_by(cod_mpio) |>
  summarise(w_F = mean(numstudents_F, na.rm = TRUE),
            w_M = mean(numstudents_M, na.rm = TRUE), .groups = "drop")

pop_z_df <- df |> filter(year < ref_year) |> group_by(cod_mpio) |>
  summarise(pp = mean(poblacion, na.rm = TRUE), .groups = "drop") |>
  mutate(pop_z = (asinh(pp) - mean(asinh(pp), na.rm = TRUE)) / sd(asinh(pp), na.rm = TRUE)) |>
  select(cod_mpio, pop_z)

df <- df |> left_join(wsex, by = "cod_mpio") |> left_join(pop_z_df, by = "cod_mpio") |>
  mutate(
    nhe_F = numstudents_F * pro_he_F, asinh_nhe_F = asinh(nhe_F), ltak_F = log1p(numstudents_F),
    nhe_M = numstudents_M * pro_he_M, asinh_nhe_M = asinh(nhe_M), ltak_M = log1p(numstudents_M)
  )

dhu <- df |> filter(intensity %in% c("unexposed", "high")) |>
  mutate(treat_h = as.integer(intensity == "high"), did_h = cease * treat_h)

#===================================================================
# PART A. Decomposition of the sex-specific heavy-exposure effect
#-------------------------------------------------------------------
# Count outcomes (denominator, absolute numerator) UNWEIGHTED; rates
# weighted by sex-specific pre-ceasefire takers. Three-arm static, so
# did_high_u is the clean high-vs-unexposed effect.
#===================================================================
fitd <- function(y, wt) feols(as.formula(sprintf("%s ~ did_low + did_high_u | %s", y, fe)),
                              data = df, weights = wt, cluster = ~cod_mpio)
gr <- function(m, lab) {
  ct <- as.data.frame(coeftable(m))
  tibble(spec = lab, est = unname(ct["did_high_u", "Estimate"]),
         se = unname(ct["did_high_u", "Std. Error"]), p = unname(ct["did_high_u", "Pr(>|t|)"]))
}
decomp <- bind_rows(
  gr(fitd("ltak_F",      NULL), "Female: log(1+takers) [denominator]"),
  gr(fitd("asinh_nhe_F", NULL), "Female: asinh(HE-entrants) [absolute]"),
  gr(fitd("pro_he_F",   ~w_F ), "Female: rate (pro_he_F)"),
  gr(fitd("ltak_M",      NULL), "Male: log(1+takers) [denominator]"),
  gr(fitd("asinh_nhe_M", NULL), "Male: asinh(HE-entrants) [absolute]"),
  gr(fitd("pro_he_M",   ~w_M ), "Male: rate (pro_he_M)")
)
cat("\n=== Sex-specific decomposition (high vs. unexposed) ===\n")
print(as.data.frame(decomp))
write.csv(decomp, paste0(data_final, "/het_sex_decomp.csv"), row.names = FALSE)

# pre-trends (three-arm high arm) for the absolute and rate outcomes
fitd_es <- function(y, wt) feols(as.formula(sprintf(
  "%s ~ i(year, intensity, ref = %d, ref2 = 'unexposed') | %s", y, ref_year, fe)),
  data = df, weights = wt, cluster = ~cod_mpio)
wald_hi <- function(m) as.numeric(fixest::wald(
  m, paste0("year::(", paste(2010:(ref_year - 1), collapse = "|"), "):intensity::high"))["p"])

decomp_pretrends <- tibble(
  spec  = c("Female: absolute", "Female: rate", "Male: absolute", "Male: rate"),
  p_pre = c(wald_hi(fitd_es("asinh_nhe_F", NULL)), wald_hi(fitd_es("pro_he_F", ~w_F)),
            wald_hi(fitd_es("asinh_nhe_M", NULL)), wald_hi(fitd_es("pro_he_M", ~w_M))))
cat("\n=== Decomposition pre-trends (high arm) ===\n")
print(as.data.frame(decomp_pretrends))
write.csv(decomp_pretrends, paste0(data_final, "/het_sex_decomp_pretrends.csv"), row.names = FALSE)

# does the female ABSOLUTE-access shortfall concentrate in small munis?
m_nheF_pop <- feols(as.formula(sprintf(
  "asinh_nhe_F ~ did_h + did_h:pop_z + pop_z:cease | %s", fe)),
  data = dhu, cluster = ~cod_mpio)
ctp  <- as.data.frame(coeftable(m_nheF_pop)); nmp <- rownames(ctp)
trit <- nmp[grepl("pop_z", nmp) & grepl("did_h", nmp, fixed = TRUE)][1]
decomp_pop <- tibble(outcome = "Female asinh(HE-entrants), high vs unexposed",
                     effect_mean = unname(ctp["did_h", "Estimate"]),
                     p_mean      = unname(ctp["did_h", "Pr(>|t|)"]),
                     per_sd      = unname(ctp[trit, "Estimate"]),
                     p_per_sd    = unname(ctp[trit, "Pr(>|t|)"]))
cat("\n=== Female absolute access moderated by population ===\n")
print(as.data.frame(decomp_pop))
write.csv(decomp_pop, paste0(data_final, "/het_sex_decomp_pop.csv"), row.names = FALSE)

#===================================================================
# PART B. HonestDiD on the sex-specific RATE (high vs. unexposed)
#===================================================================
es_vec <- function(model, pattern) {
  b <- coef(model); V <- vcov(model); idx <- grep(pattern, names(b))
  yrs <- as.integer(sub(".*year::([0-9]{4}).*", "\\1", names(b)[idx]))
  o <- order(yrs); idx <- idx[o]
  list(beta = b[idx], sigma = V[idx, idx, drop = FALSE], years = sort(yrs))
}
breakdown <- function(rm) { ex <- rm[rm$lb > 0 | rm$ub < 0, ]; if (nrow(ex)) max(ex$Mbar) else 0 }

honest_sex <- function(yvar, wt, label) {
  m  <- feols(as.formula(sprintf("%s ~ i(year, treat_h, ref = %d) | %s", yvar, ref_year, fe)),
              data = dhu, weights = wt, cluster = ~cod_mpio)
  hu <- es_vec(m, "year::[0-9]{4}:treat_h")
  numPre <- sum(hu$years < ref_year); numPost <- sum(hu$years > ref_year)
  run_rm <- function(l) createSensitivityResults_relativeMagnitudes(
    betahat = hu$beta, sigma = hu$sigma, numPrePeriods = numPre, numPostPeriods = numPost,
    Mbarvec = seq(0, 2, by = 0.5), l_vec = l)
  bd_avg   <- breakdown(run_rm(rep(1 / numPost, numPost)))
  bd_first <- breakdown(run_rm(basisVector(index = 1, size = numPost)))
  tibble(series = label, breakdown_avg = bd_avg, breakdown_first = bd_first)
}

honestdid_sex <- bind_rows(
  honest_sex("pro_he_F", ~w_F, "Female (high vs. unexposed)"),
  honest_sex("pro_he_M", ~w_M, "Male (high vs. unexposed)")
)
cat("\n=== HonestDiD breakdown by sex (Mbar at which the robust CI first covers 0) ===\n")
print(as.data.frame(honestdid_sex))
write.csv(honestdid_sex, paste0(data_final, "/het_sex_honestdid.csv"), row.names = FALSE)

cat("\nSaved: het_sex_decomp.csv, het_sex_decomp_pretrends.csv,",
    "het_sex_decomp_pop.csv, het_sex_honestdid.csv\n")
