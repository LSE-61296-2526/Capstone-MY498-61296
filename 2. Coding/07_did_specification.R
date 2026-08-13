#===================================================================
# Goal: DID identifying-assumption checks (parallel trends)
#       - Raw weighted parallel-trend plots
#       - Event studies (binary, high-vs-rest, 3-arm intensity,
#         clean high-vs-unexposed), all via robust coeftable extraction
#       - Formal pre-trends Wald test for EVERY specification
#       - HonestDiD (Rambachan-Roth) sensitivity on the clean high contrast
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(scales)
library(fixest)
if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  install.packages("HonestDiD")  # CRAN; or remotes::install_github("asheshrambachan/HonestDiD")
}
library(HonestDiD)

#===================================================================
# Setup (paths + shared functions)
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Data
df <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))
table(df$year)

#===================================================================
# Global parameters
outcome  <- "pro_he"
ref_year <- 2013                       # last clean pre-period (2013 cohort enrols 2014, pre-ceasefire)
dept     <- if ("cod_dep" %in% names(df)) "cod_dep" else "departamento"

#===================================================================
# Helper 1: tidy event-study coefficients from a feols model.
# Uses coeftable()/confint() (clustered SEs already in the model) rather
# than iplot(), which can silently drop interaction terms. Appends an
# explicit 0 row at ref_year so the normalization shows on the plot.
#===================================================================
tidy_es <- function(model, ref_year, group_var = NULL) {
  ct <- as.data.frame(coeftable(model))
  ci <- as.data.frame(confint(model))
  ct$term    <- rownames(ct)
  ct$ci_low  <- ci[ct$term, 1]
  ct$ci_high <- ci[ct$term, 2]

  ct <- ct[grepl("^year::", ct$term), , drop = FALSE]
  ct$year <- as.integer(sub("^year::([0-9]{4}).*$", "\\1", ct$term))

  if (!is.null(group_var)) {
    # trailing level label after the factor name (handles 'intensitylow'
    # and 'intensity::low'); if your fixest version names them differently,
    # check names(coef(model)) and adjust this one line.
    ct$group <- sub(paste0(".*", group_var, ":*"), "", ct$term)
  } else {
    ct$group <- "treated"
  }

  out <- data.frame(
    year = ct$year, group = ct$group,
    estimate = ct[["Estimate"]], ci_low = ct$ci_low, ci_high = ct$ci_high,
    stringsAsFactors = FALSE
  )
  refs <- data.frame(
    year = ref_year, group = unique(out$group),
    estimate = 0, ci_low = 0, ci_high = 0, stringsAsFactors = FALSE
  )
  rbind(out, refs)
}

#===================================================================
# Helper 2: event-study plot (single series or grouped)
#===================================================================
plot_es <- function(df_es, title, ref_year, grouped = FALSE) {
  p <- ggplot(df_es, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#c3c2b7") +
    add_ceasefire_line(ref_year) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(
      breaks = seq(min(df_es$year), max(df_es$year), by = 2),
      labels = seq(min(df_es$year), max(df_es$year), by = 2)
    ) +
    theme_capstone() +
    labs(title = title, x = "Year",
         subtitle = "Coefficients relative to the reference year, 95% CI",
         y = sprintf("Effect on HE access (rel. to %d)", ref_year))

  if (grouped) {
    p + aes(colour = group) +
      geom_point(position = position_dodge(width = 0.5), size = 2.5) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                    width = 0.2, position = position_dodge(width = 0.5)) +
      scale_colour_manual(values = farc_palette,
                           labels = c(low = "Low", high = "High")) +
      labs(colour = "FARC intensity")
  } else {
    p +
      geom_point(size = 2.5, colour = farc_palette[["High"]]) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2,
                    colour = farc_palette[["High"]])
  }
}

#===================================================================
# Helper 3: pre-trends Wald test (joint nullity of pre-period coefs)
#===================================================================
pre_years <- 2010:(ref_year - 1)                 # -> 2011, 2012
pre_pat   <- paste(pre_years, collapse = "|")

wald_pre <- function(model, var, label) {
  w <- wald(model, paste0("year::(", pre_pat, "):", var))
  tibble(spec = label,
         F_stat = as.numeric(w["stat"]),
         df1 = as.numeric(w["df1"]), df2 = as.numeric(w["df2"]),
         p_value = as.numeric(w["p"]))
}

#===================================================================
# PART A. RAW WEIGHTED PARALLEL-TREND PLOTS
#===================================================================

# A1. Binary treatment
g1 <- df |>
  filter(!is.na(pro_he) & !is.na(w_takers)) |>
  group_by(year, treat_presence) |>
  summarise(y = weighted.mean(pro_he, w = w_takers), .groups = "drop") |>
  mutate(treat_presence = factor(treat_presence, levels = c(0, 1),
                                  labels = c("Unexposed", "FARC-exposed"))) |>
  ggplot(aes(year, y, colour = treat_presence)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  add_ceasefire_line(2013.5) +
  scale_colour_manual(values = farc_palette) +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(
      breaks = seq(min(df$year), max(df$year), by = 2),
      labels = seq(min(df$year), max(df$year), by = 2)
    ) +
  labs(colour = NULL, x = NULL, y = "Higher-education access (weighted mean)",
       title = "Parallel trend validation — binary treatment",
       subtitle = "Municipality-year panel, 2010–2023, weighted by pre-ceasefire exam-takers") +
  theme_capstone()
ggsave(paste0(results_img, "/parallel_binaryT.png"), g1, width = 8, height = 5, dpi = 300)

# A2. High vs. rest
g2 <- df |>
  filter(!is.na(pro_he) & !is.na(w_takers)) |>
  group_by(year, treat_high) |>
  summarise(y = weighted.mean(pro_he, w = w_takers), .groups = "drop") |>
  mutate(treat_high = factor(treat_high, levels = c(0, 1),
                              labels = c("Rest", "High exposure"))) |>
  ggplot(aes(year, y, colour = treat_high)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  add_ceasefire_line(2013.5) +
  scale_colour_manual(values = c("Rest" = farc_palette[["Unexposed"]],
                                  "High exposure" = farc_palette[["High"]])) +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(
      breaks = seq(min(df$year), max(df$year), by = 2),
      labels = seq(min(df$year), max(df$year), by = 2)
    ) +
  labs(colour = NULL, x = NULL, y = "Higher-education access (weighted mean)",
       title = "Parallel trend validation — high FARC exposure",
       subtitle = "Municipality-year panel, 2010–2023, weighted by pre-ceasefire exam-takers") +
  theme_capstone()
ggsave(paste0(results_img, "/parallel_HighT.png"), g2, width = 8, height = 5, dpi = 300)

# A3. Three-arm intensity
g3 <- df |>
  filter(!is.na(pro_he) & !is.na(w_takers)) |>
  group_by(year, intensity) |>
  summarise(y = weighted.mean(pro_he, w = w_takers), .groups = "drop") |>
  mutate(intensity = factor(intensity, levels = c("unexposed", "low", "high"),
                             labels = c("Unexposed", "Low", "High"))) |>
  ggplot(aes(year, y, colour = intensity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  add_ceasefire_line(2013.5) +
  scale_colour_manual(values = farc_palette) +
  scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(
      breaks = seq(min(df$year), max(df$year), by = 2),
      labels = seq(min(df$year), max(df$year), by = 2)
    ) +
  labs(colour = "FARC intensity", x = NULL, y = "Higher-education access (weighted mean)",
       title = "Parallel trend validation — exposure intensity",
       subtitle = "Municipality-year panel, 2010–2023, weighted by pre-ceasefire exam-takers") +
  theme_capstone()
ggsave(paste0(results_img, "/parallel_intensityT.png"), g3, width = 8, height = 5, dpi = 300)

#===================================================================
# PART B. EVENT STUDIES + PRE-TRENDS WALD TESTS
#===================================================================

# B1. Binary treatment - FARC presence
fml_bin <- as.formula(sprintf(
  "%s ~ i(year, treat_presence, ref = %d) | cod_mpio + %s^year",
  outcome, ref_year, dept))
m_bin <- feols(fml_bin, data = df, weights = ~w_takers, cluster = ~cod_mpio)

p_bin <- plot_es(tidy_es(m_bin, ref_year), "Event study \u2014 binary FARC treatment", ref_year)
ggsave(file.path(results_img, "studyevent_binaryT.png"), p_bin, width = 8, height = 5, dpi = 300)

# B2. High vs. rest  (NOTE: control group mixes unexposed + low; see B4 for the clean contrast)
fml_high <- as.formula(sprintf(
  "%s ~ i(year, treat_high, ref = %d) | cod_mpio + %s^year",
  outcome, ref_year, dept))
m_high <- feols(fml_high, data = df, weights = ~w_takers, cluster = ~cod_mpio)

p_high <- plot_es(tidy_es(m_high, ref_year), "Event study \u2014 high FARC exposure (vs. rest)", ref_year)
ggsave(file.path(results_img, "studyevent_HighT.png"), p_high, width = 8, height = 5, dpi = 300)

# B3. Three-arm intensity  (low and high, each vs. unexposed)
fml_int <- as.formula(sprintf(
  "%s ~ i(year, intensity, ref = %d, ref2 = 'unexposed') | cod_mpio + %s^year",
  outcome, ref_year, dept))
m_int <- feols(fml_int, data = df, weights = ~w_takers, cluster = ~cod_mpio)

df_int <- tidy_es(m_int, ref_year, group_var = "intensity")
df_int$group <- factor(df_int$group, levels = c("low", "high"))
p_int <- plot_es(df_int, "Event study \u2014 FARC intensity (vs. unexposed)", ref_year, grouped = TRUE)
ggsave(file.path(results_img, "studyevent_intensityT.png"), p_int, width = 8, height = 5, dpi = 300)

# B4. Clean high vs. unexposed  (drops 'low'; single series for HonestDiD)
df_hu <- subset(df, intensity %in% c("unexposed", "high"))
df_hu$treat_high2 <- as.integer(df_hu$intensity == "high")
fml_hu <- as.formula(sprintf(
  "%s ~ i(year, treat_high2, ref = %d) | cod_mpio + %s^year",
  outcome, ref_year, dept))
m_hu <- feols(fml_hu, data = df_hu, weights = ~w_takers, cluster = ~cod_mpio)

p_hu <- plot_es(tidy_es(m_hu, ref_year), "Event study \u2014 high vs. unexposed", ref_year)
ggsave(file.path(results_img, "studyevent_HighvsUnexp.png"), p_hu, width = 8, height = 5, dpi = 300)

# B5. Continuous exposure (log per-capita attack rate)
fml_cont <- as.formula(sprintf(
  "%s ~ i(year, farc_logint, ref = %d) | cod_mpio + %s^year",
  outcome, ref_year, dept))
m_cont <- feols(fml_cont, data = df, weights = ~w_takers, cluster = ~cod_mpio)

p_cont <- plot_es(tidy_es(m_cont, ref_year),
                  "Event study — exposure intensity (log per-capita rate)", ref_year)
ggsave(file.path(results_img, "studyevent_contT.png"), p_cont, width = 8, height = 5, dpi = 300)

#==================================================================
# Pre-trends Wald tests - one per specification
#==================================================================
wald_table <- bind_rows(
  wald_pre(m_bin,  "treat_presence", "Binary (presence)"),
  wald_pre(m_high, "treat_high",     "High vs. rest"),
  wald_pre(m_int,  "intensity",      "Intensity (low & high jointly)"),
  wald_pre(m_hu,   "treat_high2",    "High vs. unexposed"),
  wald_pre(m_cont, "farc_logint", "Continuous (log per-capita)")
)
cat("\n=== Pre-trends Wald tests (H0: all pre-period coefs = 0) ===\n")
print(as.data.frame(wald_table))

#===================================================================
# PART C. HonestDiD SENSITIVITY  (clean high-vs-unexposed series)
#===================================================================

es_vec <- function(model, pattern) {
  b   <- coef(model); V <- vcov(model)
  idx <- grep(pattern, names(b))
  yrs <- as.integer(sub(".*year::([0-9]{4}).*", "\\1", names(b)[idx]))
  o   <- order(yrs); idx <- idx[o]
  list(beta = b[idx], sigma = V[idx, idx, drop = FALSE], years = sort(yrs))
}

hu <- es_vec(m_hu, "year::[0-9]{4}:treat_high2")
numPre  <- sum(hu$years < ref_year)    # = 3 con 2010 en el panel (2010, 2011, 2012)
numPost <- sum(hu$years > ref_year)    # = 10 (2014..2023)
stopifnot(length(hu$beta) == numPre + numPost)

run_rm <- function(l_vec) createSensitivityResults_relativeMagnitudes(
  betahat = hu$beta, sigma = hu$sigma,
  numPrePeriods = numPre, numPostPeriods = numPost,
  Mbarvec = seq(0, 2, by = 0.5), l_vec = l_vec)
breakdown <- function(rm) { ex <- rm[rm$lb > 0 | rm$ub < 0, ]; if (nrow(ex)) max(ex$Mbar) else 0 }

l_avg    <- rep(1 / numPost, numPost)               # efecto promedio post-cese
l_first  <- basisVector(index = 1, size = numPost)  # primer post-periodo (2014)
rm_avg   <- run_rm(l_avg)
rm_first <- run_rm(l_first)
bd_avg   <- breakdown(rm_avg)
bd_first <- breakdown(rm_first)

delta_rm    <- rm_avg
original_cs <- constructOriginalCS(
  betahat = hu$beta, sigma = hu$sigma,
  numPrePeriods = numPre, numPostPeriods = numPost, l_vec = l_avg)

p_honest <- createSensitivityPlot_relativeMagnitudes(delta_rm, original_cs) +
  labs(title = "HonestDiD sensitivity — high vs. unexposed",
       subtitle = expression("Robust confidence interval as the allowed pre-trend violation ("*bar(M)*") grows"),
       x = expression(bar(M)), y = "Effect on HE access (average post-period)",
       colour = "Method") +
  scale_colour_manual(values = c("Original" = farc_palette[["Unexposed"]],
                                  "C-LF" = farc_palette[["High"]])) +
  theme_capstone() +
  theme(legend.position = "bottom")
ggsave(file.path(results_img, "honestdid_highT.png"), p_honest, width = 8, height = 5, dpi = 300)

# Breakdown value (para citar en el texto): mayor Mbar con IC robusto que aún excluye 0
robust_excl    <- delta_rm[delta_rm$lb > 0 | delta_rm$ub < 0, ]
breakdown_mbar <- if (nrow(robust_excl)) max(robust_excl$Mbar) else 0
cat(sprintf("\nHonestDiD breakdown  |  average post-period: Mbar = %.2f  |  first post-period (2014): Mbar = %.2f\n",
            bd_avg, bd_first))

#===================================================================
# PART D. STATIC DID (pooled ATT) — main results table
#===================================================================

fe <- sprintf("cod_mpio + %s^year", dept)

m_static_pres <- feols(as.formula(sprintf("%s ~ did_presence | %s", outcome, fe)),
                       data = df, weights = ~w_takers, cluster = ~cod_mpio)
m_static_arms <- feols(as.formula(sprintf("%s ~ did_low + did_high_u | %s", outcome, fe)),
                       data = df, weights = ~w_takers, cluster = ~cod_mpio)
m_static_cont <- feols(as.formula(sprintf("%s ~ did_cont | %s", outcome, fe)),
                       data = df, weights = ~w_takers, cluster = ~cod_mpio)

# extractor de un término -> fila tidy en puntos porcentuales
grab_static <- function(model, term, label) {
  ct <- as.data.frame(coeftable(model))
  tibble(spec   = label,
         att_pp = 100 * ct[term, "Estimate"],
         se_pp  = 100 * ct[term, "Std. Error"],
         p      = ct[term, "Pr(>|t|)"])
}

static_tab <- bind_rows(
  grab_static(m_static_pres, "did_presence", "FARC presence (binary)"),
  grab_static(m_static_arms, "did_low",      "Low vs. unexposed"),
  grab_static(m_static_arms, "did_high_u",   "High vs. unexposed"),
  grab_static(m_static_cont, "did_cont",     "Continuous (log per-capita rate)")
)

c# --- conteos por celda en la muestra de estimación (pro_he y w_takers válidos) ---
est_df <- df |> filter(!is.na(pro_he) & !is.na(w_takers))
cell_counts <- est_df |>
  group_by(intensity) |>
  summarise(municipalities = n_distinct(cod_mpio),
            obs_mpio_year  = n(), .groups = "drop")

# municipios que identifican cada fila (grupo tratado) y referencia (unexposed)
gv      <- function(g) { v <- cell_counts$municipalities[cell_counts$intensity == g]; if (length(v)) v else 0L }
n_unexp <- gv("unexposed"); n_low <- gv("low"); n_high <- gv("high")
static_tab <- static_tab |>
  mutate(treated_mun = c(n_low + n_high, n_low, n_high, n_low + n_high),
         ref_mun     = c(n_unexp, n_unexp, n_unexp, NA_integer_))

cat("\n=== Estimation-sample size by exposure group ===\n")
print(as.data.frame(cell_counts))
cat("\n=== Static DID (pooled ATT, pp of HE access) ===\n")
print(as.data.frame(static_tab))

# Opcional: tabla LaTeX/consola comparativa de los tres modelos
# etable(m_static_pres, m_static_arms, m_static_cont,
#        cluster = ~cod_mpio, digits = 3)
