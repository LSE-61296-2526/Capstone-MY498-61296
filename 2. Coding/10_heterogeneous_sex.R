#===================================================================
# Goal: Heterogeneous effects of FARC exposure on higher-education
#       access BY STUDENT SEX, for the paper's main contrast
#       (heavy exposure: high vs. unexposed).
#         - static ATT separately for female and male students
#         - pooled triple interaction: is the female-male difference
#           in the heavy-exposure effect significant?
#         - pre-trends by sex + event-study figure

# Author: Juliana Aragon
# LSE MY498 Capstone - Applied Social Data Science
# Date: July 2026
#===================================================================

library(tidyverse)
library(scales)
library(fixest)

#===================================================================
# Setup + data
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))
df <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))

outcome  <- "pro_he"
ref_year <- 2013
dept     <- if ("cod_dep" %in% names(df)) "cod_dep" else "departamento"
fe       <- sprintf("cod_mpio + %s^year", dept)

#===================================================================
# 1. Detect the sex-specific columns and reshape to long
#    (one row per municipality-year-sex)
#===================================================================
sex_cols <- grep("^pro_he_[A-Za-z]", names(df), value = TRUE)      # e.g. pro_he_F, pro_he_M
tak_cols <- sub("^pro_he_", "numstudents_", sex_cols)

cat("Detected sex-specific outcome columns:", paste(sex_cols, collapse = ", "), "\n")
stopifnot(length(sex_cols) == 2,
          all(tak_cols %in% names(df)))

id_vars <- c("cod_mpio", dept, "year", "cease", "intensity",
             "treat_presence", "treat_high", "did_presence", "did_high",
             "did_low", "did_high_u")

dsex <- df |>
  select(all_of(id_vars), all_of(sex_cols), all_of(tak_cols)) |>
  pivot_longer(cols = c(all_of(sex_cols), all_of(tak_cols)),
               names_to = c(".value", "sex"),
               names_pattern = "(pro_he|numstudents)_(.+)")

# expect the two ICFES codes F (female) and M (male); stop clearly if not
if (!all(c("F", "M") %in% unique(dsex$sex))) {
  stop("estu_genero suffixes are ", paste(unique(dsex$sex), collapse = "/"),
       " (expected F/M). Adjust the female indicator below.")
}
dsex <- dsex |> mutate(female = as.integer(sex == "F"))

# sex-specific pre-ceasefire weight (2010-2014 mean takers), like w_takers
wsex <- dsex |>
  filter(year %in% 2010:2014) |>
  group_by(cod_mpio, sex) |>
  summarise(w_sex = mean(numstudents, na.rm = TRUE), .groups = "drop")
dsex <- dsex |> left_join(wsex, by = c("cod_mpio", "sex")) |>
  filter(is.finite(w_sex) & w_sex > 0)

# clean high-vs-unexposed sample (the main contrast)
dsex_hu <- dsex |>
  filter(intensity %in% c("unexposed", "high")) |>
  mutate(treat_h = as.integer(intensity == "high"),
         did_h   = cease * treat_h)

cat("\nMunicipality-year-sex observations by sex:\n")
print(table(dsex$sex))

#===================================================================
# 2. Helpers (event-study extraction + pre-trends + tidy row)
#===================================================================
tidy_es <- function(model, ref_year, var) {
  ct <- as.data.frame(coeftable(model)); ci <- as.data.frame(confint(model))
  ct$term <- rownames(ct); ct$ci_low <- ci[ct$term, 1]; ct$ci_high <- ci[ct$term, 2]
  ct <- ct[grepl(paste0("^year::.*:", var, "$"), ct$term), , drop = FALSE]
  ct$year <- as.integer(sub("^year::([0-9]{4}).*$", "\\1", ct$term))
  rbind(data.frame(year = ct$year, estimate = ct[["Estimate"]],
                   ci_low = ct$ci_low, ci_high = ct$ci_high),
        data.frame(year = ref_year, estimate = 0, ci_low = 0, ci_high = 0))
}

pre_years <- min(df$year, na.rm = TRUE):(ref_year - 1)
pre_pat   <- paste(pre_years, collapse = "|")
wald_pre  <- function(model, var, label) {
  w <- fixest::wald(model, paste0("year::(", pre_pat, "):", var))
  tibble(spec = label, F_stat = as.numeric(w["stat"]),
         df1 = as.numeric(w["df1"]), df2 = as.numeric(w["df2"]),
         p_value = as.numeric(w["p"]))
}
grab <- function(model, term, label) {
  ct <- as.data.frame(coeftable(model))
  tibble(spec = label, att_pp = 100 * ct[term, "Estimate"],
         se_pp = 100 * ct[term, "Std. Error"], p_value = ct[term, "Pr(>|t|)"],
         n_obs = nobs(model))
}

#===================================================================
# 3. Static heavy-exposure ATT, separately by sex
#===================================================================
fit_sex <- function(s) feols(
  as.formula(sprintf("pro_he ~ did_h | %s", fe)),
  data = filter(dsex_hu, sex == s), weights = ~w_sex, cluster = ~cod_mpio)

m_f <- fit_sex("F")
m_m <- fit_sex("M")

# also the binary "any exposure" contrast by sex (secondary)
fit_sex_pres <- function(s) feols(
  as.formula(sprintf("pro_he ~ did_presence | %s", fe)),
  data = filter(dsex, sex == s), weights = ~w_sex, cluster = ~cod_mpio)
m_f_pres <- fit_sex_pres("F")
m_m_pres <- fit_sex_pres("M")

#===================================================================
# 4. Pooled triple interaction: is the F-M difference significant?
#    cod_mpio^sex FE absorbs the municipality-sex level (and the female
#    main effect); dept^year FE the common time shocks. did_h is the MALE
#    effect (reference), did_h:female the female-minus-male difference.
#===================================================================
m_pool <- feols(
  as.formula(sprintf("pro_he ~ did_h + did_h:female + female:cease | cod_mpio^sex + %s^year", dept)),
  data = dsex_hu, weights = ~w_sex, cluster = ~cod_mpio)

diff_term <- grep("did_h.*female|female.*did_h", names(coef(m_pool)), value = TRUE)[1]

het_sex_static <- bind_rows(
  grab(m_m,      "did_h",        "High vs. unexposed - Male"),
  grab(m_f,      "did_h",        "High vs. unexposed - Female"),
  grab(m_pool,   diff_term,      "High vs. unexposed - Female minus Male (interaction)"),
  grab(m_m_pres, "did_presence", "Any exposure (binary) - Male"),
  grab(m_f_pres, "did_presence", "Any exposure (binary) - Female")
)
cat("\n=== Heavy-exposure ATT by sex (pp of HE access) ===\n")
print(as.data.frame(het_sex_static))
write.csv(het_sex_static, paste0(data_final, "/het_sex_static.csv"), row.names = FALSE)

#===================================================================
# 5. Pre-trends by sex + event study (high vs. unexposed)
#===================================================================
es_sex <- function(s) feols(
  as.formula(sprintf("pro_he ~ i(year, treat_h, ref = %d) | %s", ref_year, fe)),
  data = filter(dsex_hu, sex == s), weights = ~w_sex, cluster = ~cod_mpio)
m_f_es <- es_sex("F")
m_m_es <- es_sex("M")

het_sex_pretrends <- bind_rows(
  wald_pre(m_m_es, "treat_h", "High vs. unexposed - Male"),
  wald_pre(m_f_es, "treat_h", "High vs. unexposed - Female")
)
cat("\n=== Pre-trends by sex (high vs. unexposed) ===\n")
print(as.data.frame(het_sex_pretrends))
write.csv(het_sex_pretrends, paste0(data_final, "/het_sex_pretrends.csv"), row.names = FALSE)

es_plot_df <- bind_rows(
  transform(tidy_es(m_m_es, ref_year, "treat_h"), sex = "Male"),
  transform(tidy_es(m_f_es, ref_year, "treat_h"), sex = "Female")
)
# Both sexes on one plot: CI ribbon + connecting line + points, so the two
# post-ceasefire trajectories can be compared directly (no dodging, so the
# dynamics read cleanly; a light x-nudge keeps the points from overlapping).
sex_cols_plot <- c(Male = farc_palette[["Unexposed"]], Female = farc_palette[["High"]])
es_plot_df <- es_plot_df |>
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         year_j = year + ifelse(sex == "Female", 0.08, -0.08)) |>
  arrange(sex, year)

p_sex <- ggplot(es_plot_df, aes(year, estimate, colour = sex, fill = sex)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#c3c2b7") +
  add_ceasefire_line(ref_year) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(x = year_j), size = 2.2) +
  geom_errorbar(aes(x = year_j, ymin = ci_low, ymax = ci_high), width = 0.18, linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(min(es_plot_df$year), max(es_plot_df$year), by = 2)) +
  scale_colour_manual(values = sex_cols_plot) +
  scale_fill_manual(values = sex_cols_plot) +
  theme_capstone() +
  labs(colour = NULL, fill = NULL, x = "Year",
       y = sprintf("Effect on HE access (rel. to %d)", ref_year),
       title = "Heavy-exposure effect on higher-education access, by student sex",
       subtitle = "Event study, high vs. unexposed municipalities; 95% CI")
ggsave(paste0(results_img, "/event_sex.png"), p_sex, width = 8, height = 5, dpi = 300)

cat("\nSaved: het_sex_static.csv, het_sex_pretrends.csv, event_sex.png\n")
