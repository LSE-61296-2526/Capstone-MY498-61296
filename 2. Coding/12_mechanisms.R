#===================================================================
# Goal: Mecanismos de heterogeneidad del cese al fuego (2013)
#       sobre acceso a educacion superior
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: July 2026
#-------------------------------------------------------------------
# Script completo y consolidado. Reemplaza todas las versiones
# anteriores.
#
# NOTA sobre nombres de coeficientes: fixest escribe la interaccion
# con el MECANISMO PRIMERO,
#     "high_tax:year::2010:intensity::low"
# y no
#     "year::2010:intensity::low:high_tax"
# Todo el parseo aqui es agnostico al orden por eso mismo.
#
# Correcciones respecto a la version original:
#   1. sin ancla "^year::" al filtrar terminos (era lo que hacia que
#      la interaccion se perdiera y los dos paneles salieran identicos)
#   2. intensidad leida del tag "intensity::", no de grepl("high"),
#      que chocaba con high_tax / high_spend / high_coca
#   3. IC del grupo High via combinacion lineal con la matriz de
#      varianzas, NO sumando los extremos de dos IC
#   4. mecanismos fiscales en per capita, no en niveles
#   5. corte de coca elegido segun la masa en cero
#   6. m_exp -> m_spend
#   7. Wald calculado a mano; falla con error si no encuentra
#      terminos, en vez de devolver NA en silencio
#   8. TRIPLE DIFERENCIA COMPLETA: se anade el termino year x
#      mecanismo, que "i(year,intensity)*mech" NO genera. Sin el, la
#      interaccion mezcla moderacion del efecto del cese con
#      tendencia diferencial de los municipios de alto mecanismo.
#===================================================================

library(tidyverse)
library(scales)
library(fixest)
library(broom)

#===================================================================
# 0. Setup y datos
#===================================================================

source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding",
                 "00_setup.R"))

df <- readRDS(paste0(data_final, "/final_panel_2010_2023_treatment.rds"))

outcome  <- "pro_he"
ref_year <- 2013
dept     <- if ("cod_dep" %in% names(df)) "cod_dep" else "departamento"

# AJUSTA ESTO: nombre real de la variable de poblacion en tu panel.
pop_var <- "poblacion"

if (!pop_var %in% names(df)) {
  stop("No encuentro la variable de poblacion '", pop_var,
       "'. Revisa names(df) y ajusta pop_var.")
}

#===================================================================
# 1. Construccion de mecanismos (solo informacion pre-cese)
#===================================================================

df_pre <- df |>
  filter(year < ref_year) |>
  group_by(cod_mpio) |>
  summarise(
    pop_pre       = mean(.data[[pop_var]],     na.rm = TRUE),
    tax_tot       = mean(total_income_real,    na.rm = TRUE),
    spend_tot     = mean(total_expenses_real,  na.rm = TRUE),
    transfers_tot = mean(total_transfers_real, na.rm = TRUE),
    coca_pre      = mean(coca_crops,           na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tax_pc       = tax_tot       / pop_pre,
    spend_pc     = spend_tot     / pop_pre,
    transfers_pc = transfers_tot / pop_pre
  )

# Diagnostico 1: si la correlacion con el nivel total es alta (>0.9),
# el corte per capita sigue capturando tamano poblacional.
message("cor(tax_pc, tax_tot)     = ",
        round(with(df_pre, cor(tax_pc, tax_tot, use = "complete.obs")), 3))
message("cor(spend_pc, spend_tot) = ",
        round(with(df_pre, cor(spend_pc, spend_tot, use = "complete.obs")), 3))

med_tax       <- median(df_pre$tax_pc,       na.rm = TRUE)
med_spend     <- median(df_pre$spend_pc,     na.rm = TRUE)
med_transfers <- median(df_pre$transfers_pc, na.rm = TRUE)
# Population moderator (the key lead): high = larger-than-median pre-ceasefire
# population. "Low pop" is the SMALL exposed municipalities where the penalty
# is expected to concentrate.
med_pop       <- median(df_pre$pop_pre,      na.rm = TRUE)

# Diagnostico 2: son tax y spend la misma variable? Si la tabla sale
# casi diagonal, no son dos mecanismos distintos y no puedes
# presentarlos como canales separados en el paper.
print(table(df_pre$tax_pc > med_tax, df_pre$spend_pc > med_spend,
            dnn = c("high_tax", "high_spend")))
message("cor(tax_pc, spend_pc) = ",
        round(cor(df_pre$tax_pc, df_pre$spend_pc, use = "complete.obs"), 3))

## --- coca: mirar la distribucion antes de elegir el corte ---------
print(quantile(df_pre$coca_pre, c(.25, .5, .75, .9, .95), na.rm = TRUE))

share_zero <- mean(df_pre$coca_pre == 0, na.rm = TRUE)
message("Municipios sin coca pre-2013: ", round(100 * share_zero), "%")

if (share_zero > 0.40) {
  message("-> uso presencia/ausencia (un corte por cuantil seria ficticio)")
  coca_rule  <- function(x) as.integer(x > 0)
  coca_label <- "coca exposure"
} else {
  q_coca <- quantile(df_pre$coca_pre, 0.75, na.rm = TRUE)
  message("-> uso corte en p75 = ", q_coca)
  coca_rule  <- function(x) as.integer(x > q_coca)
  coca_label <- "coca exposure"
}

## --- binarios finales ---------------------------------------------
df <- df |>
  select(-any_of(c("tax_pc", "spend_pc", "transfers_pc", "coca_pre", "pop_pre",
                   "high_tax", "high_spend", "high_transfers", "high_coca", "high_pop"))) |>
  left_join(df_pre |> select(cod_mpio, tax_pc, spend_pc, transfers_pc, coca_pre, pop_pre),
            by = "cod_mpio") |>
  mutate(
    # NA_integer_ y no 0: los faltantes no deben caer en el grupo "low"
    high_tax       = if_else(is.na(tax_pc),       NA_integer_, as.integer(tax_pc       > med_tax)),
    high_spend     = if_else(is.na(spend_pc),     NA_integer_, as.integer(spend_pc     > med_spend)),
    high_transfers = if_else(is.na(transfers_pc), NA_integer_, as.integer(transfers_pc > med_transfers)),
    high_coca      = if_else(is.na(coca_pre),     NA_integer_, coca_rule(coca_pre)),
    high_pop       = if_else(is.na(pop_pre),      NA_integer_, as.integer(pop_pre      > med_pop))
  )

table(df$high_tax,       useNA = "always")
table(df$high_spend,     useNA = "always")
table(df$high_transfers, useNA = "always")
table(df$high_coca,      useNA = "always")
table(df$high_pop,       useNA = "always")

## --- municipios por celda mecanismo x intensidad -------------------
# Si alguna celda tiene menos de ~30 municipios, la interaccion no
# identifica nada util y conviene reportarlo como limitacion.
cells <- list(
  tax       = df |> distinct(cod_mpio, intensity, high_tax)       |> count(intensity, high_tax),
  spend     = df |> distinct(cod_mpio, intensity, high_spend)     |> count(intensity, high_spend),
  transfers = df |> distinct(cod_mpio, intensity, high_transfers) |> count(intensity, high_transfers),
  coca      = df |> distinct(cod_mpio, intensity, high_coca)      |> count(intensity, high_coca),
  pop       = df |> distinct(cod_mpio, intensity, high_pop)       |> count(intensity, high_pop)
)
print(cells)

#===================================================================
# 2. Helper: parseo de nombres de coeficientes
#-------------------------------------------------------------------
# Descompone cada nombre en (anio, intensidad, tipo) sin asumir el
# orden en que fixest concatena las piezas.
#
# Tipos de termino en la especificacion de triple diferencia:
#   "year::2014:intensity::high"            -> base   (int OK, mech NO)
#   "year::2014:high_tax"                   -> doble  (int NA, mech SI)
#   "high_tax:year::2014:intensity::high"   -> triple (int OK, mech SI)
#===================================================================

parse_terms <- function(model, mech_var) {

  nm <- names(coef(model))

  yr <- as.integer(str_match(nm, "year::(\\d{4})")[, 2])

  int <- str_match(nm, "intensity::([A-Za-z]+)")[, 2]
  if (all(is.na(int))) {                       # fallback: "year::2014:high"
    int <- str_match(nm, "year::\\d{4}:([A-Za-z]+)")[, 2]
  }

  ismech <- grepl(mech_var, nm, fixed = TRUE)

  list(
    nm      = nm,
    yr      = yr,
    int     = int,
    ismech  = ismech,
    isbase  = !ismech &  !is.na(int) & !is.na(yr),   # year x intensity
    isdoble =  ismech &   is.na(int) & !is.na(yr),   # year x mecanismo
    istriple =  ismech &  !is.na(int) & !is.na(yr)   # year x intensity x mech
  )
}

#===================================================================
# 3. Helper: coeficientes de event study por mecanismo
#-------------------------------------------------------------------
# Efecto del cese (expuestos vs. no expuestos) DENTRO de cada grupo
# del mecanismo:
#   grupo Low  del mecanismo -> coeficiente base
#   grupo High del mecanismo -> base + triple
# El termino doble (year x mecanismo) se cancela, porque afecta por
# igual a expuestos y no expuestos dentro del grupo.
# SE via t(c) V c: sumar los extremos de dos IC no tiene sentido.
#===================================================================

tidy_es_mech <- function(model, ref_year, mech_var, mech_label, level = 0.95) {

  b <- coef(model)
  V <- vcov(model)          # ya clusterizada por el modelo
  p <- parse_terms(model, mech_var)

  if (!any(p$istriple)) {
    stop("No encontre terminos triples para '", mech_var,
         "'. Revisa names(coef(model)).")
  }

  z <- qnorm(1 - (1 - level) / 2)

  lincom <- function(y, i, add_mech) {
    cv <- rep(0, length(b))
    cv[which(p$yr == y & p$int == i & p$isbase)] <- 1
    if (add_mech) cv[which(p$yr == y & p$int == i & p$istriple)] <- 1
    est <- sum(cv * b)
    se  <- sqrt(drop(t(cv) %*% V %*% cv))      # aqui entra la covarianza
    c(est, est - z * se, est + z * se)
  }

  keys <- unique(data.frame(year = p$yr[p$isbase], intensity = p$int[p$isbase],
                            stringsAsFactors = FALSE))
  keys <- keys[order(keys$intensity, keys$year), ]

  out <- map2_dfr(keys$year, keys$intensity, function(y, i) {
    lo <- lincom(y, i, add_mech = FALSE)
    hi <- lincom(y, i, add_mech = TRUE)
    bind_rows(
      tibble(year = y, intensity = i, mech_group = paste0("Low ",  mech_label),
             estimate = lo[1], ci_low = lo[2], ci_high = lo[3]),
      tibble(year = y, intensity = i, mech_group = paste0("High ", mech_label),
             estimate = hi[1], ci_low = hi[2], ci_high = hi[3])
    )
  })

  ## fila de referencia (efecto normalizado a 0)
  refs <- expand_grid(
    year       = ref_year,
    intensity  = unique(out$intensity),
    mech_group = unique(out$mech_group)
  ) |>
    mutate(estimate = 0, ci_low = 0, ci_high = 0)

  bind_rows(out, refs) |>
    mutate(
      intensity  = factor(str_to_title(intensity), levels = c("High", "Low")),
      mech_group = factor(mech_group,
                          levels = c(paste0("High ", mech_label),
                                     paste0("Low ",  mech_label)))
    ) |>
    arrange(mech_group, intensity, year)
}

#===================================================================
# 4. Helper: grafico
#===================================================================

plot_es_mech <- function(df_es, title, ref_year) {

  ggplot(df_es, aes(x = year, y = estimate, colour = intensity)) +

    geom_hline(yintercept = 0, linetype = "dashed", colour = "#c3c2b7") +
    add_ceasefire_line(ref_year) +

    # ribbon primero: si va despues tapa lineas y puntos
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = intensity),
                alpha = 0.15, colour = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +

    facet_wrap(~ mech_group) +
    scale_y_continuous(labels = scales::percent) +
    scale_colour_manual(values = farc_palette) +
    scale_fill_manual(values = farc_palette) +
    theme_capstone() +
    labs(
      title    = title,
      subtitle = "Heterogeneous effects by mechanism",
      x = "Year",
      y = sprintf("Effect on HE access (rel. to %d)", ref_year),
      colour = "Conflict intensity",
      fill   = "Conflict intensity"
    )
}

#===================================================================
# 5. Helpers: tests
#===================================================================

pre_years  <- min(df$year, na.rm = TRUE):(ref_year - 1)
post_years <- (ref_year + 1):max(df$year, na.rm = TRUE)

# Selecciona SOLO los terminos triples (no los dobles year x mecanismo)
mech_terms <- function(model, mech_var, years, intensity_level = "high") {
  p <- parse_terms(model, mech_var)
  keep <- which(p$istriple & p$yr %in% years & p$int == intensity_level)
  p$nm[keep]
}

# Wald conjunto: H0 = todos los coeficientes seleccionados son cero.
# ESTE es el test que sostiene "el mecanismo opera". Comparar dos
# bandas de confianza a ojo NO es un test de heterogeneidad.
wald_terms <- function(model, terms, spec, intensity_level, period) {

  if (length(terms) == 0) {
    stop("Cero terminos seleccionados para '", spec, "' (", period,
         ", intensity=", intensity_level, "). Revisa names(coef(model)).")
  }

  b <- coef(model)[terms]
  V <- vcov(model)[terms, terms, drop = FALSE]
  q <- length(b)

  # ginv si la submatriz esta mal condicionada (pocos municipios en
  # alguna celda, o coeficientes casi colineales)
  Vinv <- tryCatch(solve(V), error = function(e) {
    warning("V mal condicionada en '", spec, "' (", period,
            "): uso inversa generalizada. Revisa los conteos por celda.")
    MASS::ginv(V)
  })

  chi2 <- drop(t(b) %*% Vinv %*% b)
  Fst  <- chi2 / q

  df2 <- tryCatch(fixest::degrees_freedom(model, type = "t"),
                  error = function(e) NA_real_)

  tibble(
    spec      = spec,
    intensity = intensity_level,
    period    = period,
    n_terms   = q,
    chi2      = chi2,
    p_chi2    = pchisq(chi2, df = q, lower.tail = FALSE),
    F_stat    = Fst,
    df1       = q,
    df2       = df2,
    p_F       = if (is.na(df2)) NA_real_ else pf(Fst, q, df2, lower.tail = FALSE)
  )
}

wald_pre <- function(model, mech_var, label, intensity_level = "high") {
  wald_terms(model, mech_terms(model, mech_var, pre_years, intensity_level),
             label, intensity_level, "pre")
}

wald_mech <- function(model, mech_var, label, intensity_level = "high") {
  wald_terms(model, mech_terms(model, mech_var, post_years, intensity_level),
             label, intensity_level, "post")
}

# Coeficientes triples uno a uno, para tabla del paper
mech_coefs <- function(model, mech_var, label) {
  p  <- parse_terms(model, mech_var)
  tt <- p$nm[p$istriple]
  tidy(model, conf.int = TRUE) |>
    filter(term %in% tt) |>
    mutate(
      spec      = label,
      year      = as.integer(str_match(term, "year::(\\d{4})")[, 2]),
      intensity = str_match(term, "intensity::([A-Za-z]+)")[, 2]
    ) |>
    select(spec, year, intensity, estimate, std.error,
           conf.low, conf.high, p.value) |>
    arrange(intensity, year)
}

#===================================================================
# 6. Estimacion: triple diferencia completa
#-------------------------------------------------------------------
# Tres bloques de terminos:
#   i(year, intensity, ref2='unexposed')        year x intensity
#   i(year, mech, ref)                          year x mecanismo   <- FALTABA
#   i(year, intensity, ref2='unexposed'):mech   triple (interes)
#
# El efecto principal del mecanismo lo absorbe el FE de municipio
# (es invariante en el tiempo por construccion).
#===================================================================

run_mech <- function(mech_var) {

  fml <- as.formula(sprintf(
    paste0(
      "%s ~ i(year, intensity, ref = %d, ref2 = 'unexposed')",
      "   + i(year, %s, ref = %d)",
      "   + i(year, intensity, ref = %d, ref2 = 'unexposed'):%s",
      "   | cod_mpio + %s^year"
    ),
    outcome, ref_year, mech_var, ref_year, ref_year, mech_var, dept
  ))

  feols(fml, data = df, weights = ~w_takers, cluster = ~cod_mpio)
}

m_tax       <- run_mech("high_tax")
m_spend     <- run_mech("high_spend")
m_transfers <- run_mech("high_transfers")
m_coca      <- run_mech("high_coca")
m_pop       <- run_mech("high_pop")

# Diagnostico: nombres reales de los coeficientes
print(names(coef(m_tax)))

specs <- list(
  list(m = m_tax,       v = "high_tax",       lab = "Fiscal capacity (pc)",           g = "fiscal capacity"),
  list(m = m_spend,     v = "high_spend",     lab = "Public spending (pc)",           g = "public spending"),
  list(m = m_transfers, v = "high_transfers", lab = "Intergovernmental transfers (pc)", g = "transfers"),
  list(m = m_coca,      v = "high_coca",      lab = "Coca exposure",                  g = coca_label),
  list(m = m_pop,       v = "high_pop",       lab = "Population size",                g = "population size")
)

# Verificacion: deben salir 10 terminos, TODOS con ":intensity::"
# Si salen 20, los dobles se estan colando y el Wald no es valido.
print(mech_terms(m_tax,  "high_tax",  post_years, "high"))
print(mech_terms(m_coca, "high_coca", post_years, "high"))

#===================================================================
# 7. Tests
#-------------------------------------------------------------------
# Lee en este orden:
#   1. cells       -> hay municipios suficientes en cada celda?
#   2. tests_pre   -> los grupos ya divergian antes de 2013?
#   3. tests_post  -> solo si lo anterior pasa
#   4. graficos    -> ilustran lo que los tests ya dijeron
#
# Son 6 tests: con alpha=0.05 se esperan ~0.3 falsos positivos.
# Un p entre 0.01 y 0.05 aislado no sostiene un mecanismo.
#===================================================================

tests_pre <- bind_rows(
  map_dfr(specs, ~ wald_pre(.x$m, .x$v, .x$lab, "high")),
  map_dfr(specs, ~ wald_pre(.x$m, .x$v, .x$lab, "low"))
)

tests_post <- bind_rows(
  map_dfr(specs, ~ wald_mech(.x$m, .x$v, .x$lab, "high")),
  map_dfr(specs, ~ wald_mech(.x$m, .x$v, .x$lab, "low"))
)

print(tests_pre)
print(tests_post)

# Correccion por comparaciones multiples sobre los tests post
tests_post <- tests_post |>
  mutate(p_holm = p.adjust(p_F, method = "holm"),
         p_bh   = p.adjust(p_F, method = "BH"))
print(tests_post |> select(spec, intensity, F_stat, p_F, p_holm, p_bh))

coefs_mech <- map_dfr(specs, ~ mech_coefs(.x$m, .x$v, .x$lab))
print(coefs_mech, n = Inf)

#===================================================================
# 8. Graficos
#===================================================================

df_tax       <- tidy_es_mech(m_tax,       ref_year, "high_tax",       "fiscal capacity")
df_spend     <- tidy_es_mech(m_spend,     ref_year, "high_spend",     "public spending")
df_transfers <- tidy_es_mech(m_transfers, ref_year, "high_transfers", "transfers")
df_coca      <- tidy_es_mech(m_coca,      ref_year, "high_coca",      coca_label)
df_pop       <- tidy_es_mech(m_pop,       ref_year, "high_pop",       "population size")

# Chequeo de sanidad: los paneles NO deben ser identicos
df_tax |>
  filter(intensity == "High") |>
  select(year, mech_group, estimate) |>
  pivot_wider(names_from = mech_group, values_from = estimate) |>
  print(n = Inf)

p_tax       <- plot_es_mech(df_tax,       "Heterogeneity by fiscal capacity",         ref_year)
p_spend     <- plot_es_mech(df_spend,     "Heterogeneity by public spending",         ref_year)
p_transfers <- plot_es_mech(df_transfers, "Heterogeneity by intergovernmental transfers", ref_year)
p_coca      <- plot_es_mech(df_coca,      "Heterogeneity by coca exposure",           ref_year)
p_pop       <- plot_es_mech(df_pop,       "Heterogeneity by population size",          ref_year)

ggsave(file.path(results_img, "event_tax.png"),       p_tax,       width = 9, height = 5)
ggsave(file.path(results_img, "event_spend.png"),     p_spend,     width = 9, height = 5)
ggsave(file.path(results_img, "event_transfers.png"), p_transfers, width = 9, height = 5)
ggsave(file.path(results_img, "event_coca.png"),      p_coca,      width = 9, height = 5)
ggsave(file.path(results_img, "event_pop.png"),       p_pop,       width = 9, height = 5)

#===================================================================
# 8b. CSV exports (moderator tests + triple coefficients) for review
#===================================================================
write.csv(tests_pre,  paste0(data_final, "/mechanism_tests_pre.csv"),  row.names = FALSE)
write.csv(tests_post, paste0(data_final, "/mechanism_tests_post.csv"), row.names = FALSE)
write.csv(coefs_mech, paste0(data_final, "/mechanism_coefs.csv"),      row.names = FALSE)

#===================================================================
# 8c. MECHANISM — non-FARC "security vacuum"
#-------------------------------------------------------------------
# Does non-FARC (successor / other-actor) violence RISE in heavily
# FARC-exposed municipalities after the ceasefire? A positive & significant
# post-ceasefire gap would support a violent-vacuum story; a null or
# negative gap rules it out. Outcome = non-FARC events across ALL 11
# SIEVCAC categories (fuller than the 6-category total_nofarc_events).
# Unweighted (a violence count, not an education outcome); muni + dept^year
# FE; clustered by municipality. (Folded in from legacy/13.)
#===================================================================

violence_cats <- c("WA","DP","AS","AT","DB","DF","MI","MA","RU","SE","VS")
nofarc_cols   <- paste0("n_eventos_", violence_cats, "_0")
have_cols     <- nofarc_cols[nofarc_cols %in% names(df)]

if (length(have_cols) == 0) {
  warning("No encuentro columnas n_eventos_*_0 para el vacio de seguridad; se omite el bloque 8c.")
} else {
  df <- df |> mutate(total_nofarc_all = rowSums(across(all_of(have_cols)), na.rm = TRUE))

  fit_vac        <- function(rhs) feols(
    as.formula(sprintf("total_nofarc_all ~ %s | cod_mpio + %s^year", rhs, dept)),
    data = df, cluster = ~cod_mpio)
  m_vac_presence <- fit_vac("did_presence")
  m_vac_high     <- fit_vac("did_low + did_high_u")

  vacuum_static <- bind_rows(
    tibble(spec = "FARC presence (binary)", term = "did_presence",
           estimate = unname(coef(m_vac_presence)["did_presence"]),
           se = unname(se(m_vac_presence)["did_presence"]),
           p_value = unname(pvalue(m_vac_presence)["did_presence"])),
    tibble(spec = "High vs. unexposed", term = "did_high_u",
           estimate = unname(coef(m_vac_high)["did_high_u"]),
           se = unname(se(m_vac_high)["did_high_u"]),
           p_value = unname(pvalue(m_vac_high)["did_high_u"]))
  )

  cat("\n=== Security vacuum: does non-FARC violence rise post-ceasefire? ===\n")
  cat("(positive & significant = successor-violence surge; <=0 = no vacuum)\n")
  print(as.data.frame(vacuum_static))
  saveRDS(vacuum_static, paste0(data_final, "/mechanism_vacuum_static.rds"))
  write.csv(vacuum_static, paste0(data_final, "/mechanism_vacuum_static.csv"), row.names = FALSE)

  # Clean high-vs-unexposed event study + pre-trends on the vacuum outcome
  df_vac_hu <- df |> filter(intensity %in% c("unexposed", "high")) |>
    mutate(vh = as.integer(intensity == "high"))
  m_vac_es  <- feols(as.formula(sprintf(
    "total_nofarc_all ~ i(year, vh, ref = %d) | cod_mpio + %s^year", ref_year, dept)),
    data = df_vac_hu, cluster = ~cod_mpio)
  w_vac_pre <- fixest::wald(m_vac_es, paste0("year::(", paste(pre_years, collapse = "|"), "):vh"))
  vacuum_pretrends <- tibble(spec = "High vs. unexposed (non-FARC violence)",
                             F_stat = as.numeric(w_vac_pre["stat"]),
                             df1 = as.numeric(w_vac_pre["df1"]),
                             df2 = as.numeric(w_vac_pre["df2"]),
                             p_value = as.numeric(w_vac_pre["p"]))
  cat("\n=== Security-vacuum pre-trends (non-FARC violence outcome) ===\n")
  print(as.data.frame(vacuum_pretrends))
  write.csv(vacuum_pretrends, paste0(data_final, "/mechanism_vacuum_pretrends.csv"), row.names = FALSE)

  # Trend plot: mean non-FARC violence over time, by FARC-exposure intensity
  p_vac <- df |>
    group_by(year, intensity) |>
    summarise(mean_events = mean(total_nofarc_all, na.rm = TRUE), .groups = "drop") |>
    mutate(intensity = factor(intensity, levels = c("unexposed", "low", "high"),
                              labels = c("Unexposed", "Low", "High"))) |>
    ggplot(aes(year, mean_events, colour = intensity)) +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    add_ceasefire_line(2014.5) +
    scale_colour_manual(values = farc_palette) +
    theme_capstone() +
    labs(colour = "FARC intensity", x = NULL,
         y = "Mean non-FARC violent events per municipality",
         title = "Non-FARC violence trends by FARC-exposure intensity",
         subtitle = "All 11 SIEVCAC categories, non-FARC-attributed, 2010-2023")
  ggsave(file.path(results_img, "mechanism_vacuum_trend.png"), p_vac, width = 8, height = 5, dpi = 300)
}

#===================================================================
# 8d. POPULATION MODERATOR — continuous (asinh, z-standardized)
#-------------------------------------------------------------------
# The binary median split (high_pop) is low-powered. Here population
# enters as a CONTINUOUS baseline moderator: asinh of the pre-ceasefire
# population, then z-scored, in a static triple-interaction (legacy/14
# style). The did:pop_z coefficient reads as "change in the ceasefire
# effect per +1 SD of (asinh) population":
#   per_sd > 0  => the negative effect is SMALLER in larger municipalities
#                  (i.e. the penalty concentrates in the SMALL ones)
#   per_sd < 0  => the penalty is larger in bigger municipalities
# did_* main term = the effect at a mean-population municipality (pop_z = 0).
#===================================================================
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

pop_cont <- df_pre |> transmute(cod_mpio, pop_z = zscore(asinh(pop_pre)))
df <- df |> select(-any_of("pop_z")) |> left_join(pop_cont, by = "cod_mpio")

# clean high-vs-unexposed sample (drops the ambiguous low arm)
df_hu <- df |> filter(intensity %in% c("unexposed", "high")) |>
  mutate(did_h = cease * as.integer(intensity == "high"))

m_pop_pres <- feols(as.formula(sprintf(
  "%s ~ did_presence + did_presence:pop_z + pop_z:cease | cod_mpio + %s^year", outcome, dept)),
  data = df, weights = ~w_takers, cluster = ~cod_mpio)
m_pop_high <- feols(as.formula(sprintf(
  "%s ~ did_h + did_h:pop_z + pop_z:cease | cod_mpio + %s^year", outcome, dept)),
  data = df_hu, weights = ~w_takers, cluster = ~cod_mpio)

grab_pop <- function(model, did_var, label) {
  ct  <- as.data.frame(coeftable(model)); nm <- rownames(ct)
  tri <- nm[grepl("pop_z", nm) & grepl(did_var, nm, fixed = TRUE)][1]
  tibble(spec           = label,
         effect_mean_pp = 100 * ct[did_var, "Estimate"],   # effect at a mean-pop municipality
         p_mean         = ct[did_var, "Pr(>|t|)"],
         per_sd_pp      = 100 * ct[tri, "Estimate"],        # change per +1 SD asinh(pop)
         se_per_sd_pp   = 100 * ct[tri, "Std. Error"],
         p_per_sd       = ct[tri, "Pr(>|t|)"],
         N              = nobs(model))
}

pop_cont_tab <- bind_rows(
  grab_pop(m_pop_pres, "did_presence", "Any exposure x population"),
  grab_pop(m_pop_high, "did_h",        "High vs. unexposed x population")
)
cat("\n=== Population moderator (continuous, asinh z-scored) ===\n")
cat("(per_sd > 0 => negative effect SMALLER in larger munis => concentrated in the small)\n")
print(as.data.frame(pop_cont_tab))
write.csv(pop_cont_tab, paste0(data_final, "/mechanism_pop_continuous.csv"), row.names = FALSE)

#===================================================================
# 8e. COMPOSITION — did the DENOMINATOR (Saber-11 takers) grow?
#-------------------------------------------------------------------
# pro_he is a RATE = HE-transitions / Saber-11 takers. If the ceasefire
# raised secondary retention (Prem et al.; Montano) MORE in heavily-exposed
# municipalities, the taker pool expands with lower-propensity students, so
# the transition RATE can fall even as the ABSOLUTE number of HE-entrants
# holds or rises. Test:
#   (1) log(1+takers)      -> did the denominator grow post-ceasefire?
#   (2) asinh(HE-entrants) -> did absolute access fall, or only the rate?
#   (3) is the denominator growth concentrated in the SMALL municipalities
#       where the rate fell most (continuous population interaction)?
# numstudents = contemporaneous annual Saber-11 takers (03_preprocess_education.R).
# Count outcomes are UNWEIGHTED (weighting a count by a pre-count is odd); the
# rate is shown unweighted too, for a like-for-like comparison with the counts.
#===================================================================

if (!"numstudents" %in% names(df)) {
  warning("No encuentro 'numstudents' en el panel; se omite el bloque de composicion 8e.")
} else {
  df <- df |>
    mutate(log_takers = log1p(numstudents),
           n_he       = numstudents * pro_he,      # absolute HE-entrants
           asinh_nhe  = asinh(n_he))

  fit_comp_static <- function(yvar) feols(
    as.formula(sprintf("%s ~ did_low + did_high_u | cod_mpio + %s^year", yvar, dept)),
    data = df, cluster = ~cod_mpio)

  grab_c <- function(model, label) {
    ct <- as.data.frame(coeftable(model))
    tibble(outcome       = label,
           high_vs_unexp = unname(ct["did_high_u", "Estimate"]),
           se_high       = unname(ct["did_high_u", "Std. Error"]),
           p_high        = unname(ct["did_high_u", "Pr(>|t|)"]),
           low_vs_unexp  = unname(ct["did_low", "Estimate"]),
           p_low         = unname(ct["did_low", "Pr(>|t|)"]))
  }

  comp_tab <- bind_rows(
    grab_c(fit_comp_static("log_takers"), "log(1+takers)  [denominator]"),
    grab_c(fit_comp_static("asinh_nhe"),  "asinh(HE-entrants)  [absolute]"),
    grab_c(fit_comp_static("pro_he"),     "pro_he  [rate, unweighted]")
  )
  cat("\n=== Composition: denominator vs absolute access vs rate (high vs unexposed) ===\n")
  cat("(denominator UP + absolute flat/UP while rate DOWN => composition effect)\n")
  print(as.data.frame(comp_tab))
  write.csv(comp_tab, paste0(data_final, "/mechanism_composition.csv"), row.names = FALSE)

  # Population moderation of the denominator (takers) AND the absolute numerator
  # (HE-entrants), to locate where the small-municipality rate penalty comes from.
  df_hu_c <- df |> filter(intensity %in% c("unexposed", "high")) |>
    mutate(did_h = cease * as.integer(intensity == "high"))
  pop_mod <- function(yvar, label) {
    m  <- feols(as.formula(sprintf(
      "%s ~ did_h + did_h:pop_z + pop_z:cease | cod_mpio + %s^year", yvar, dept)),
      data = df_hu_c, cluster = ~cod_mpio)
    ct <- as.data.frame(coeftable(m)); nm <- rownames(ct)
    tri <- nm[grepl("pop_z", nm) & grepl("did_h", nm, fixed = TRUE)][1]
    tibble(outcome     = label,
           effect_mean = unname(ct["did_h", "Estimate"]),
           p_mean      = unname(ct["did_h", "Pr(>|t|)"]),
           per_sd      = unname(ct[tri, "Estimate"]),
           p_per_sd    = unname(ct[tri, "Pr(>|t|)"]))
  }
  comp_pop <- bind_rows(
    pop_mod("log_takers", "log(1+takers), high vs unexposed"),
    pop_mod("asinh_nhe",  "asinh(HE-entrants), high vs unexposed")
  )
  cat("\n=== Composition moderated by population (high vs unexposed) ===\n")
  cat("(takers per_sd ~ 0 => denominator grows evenly by size; HE-entrants per_sd > 0\n")
  cat(" => absolute access grows MORE in large munis, i.e. small munis lag on the numerator)\n")
  print(as.data.frame(comp_pop))
  write.csv(comp_pop, paste0(data_final, "/mechanism_composition_pop.csv"), row.names = FALSE)

  # Event study of the denominator by intensity (three-arm)
  m_tak_es <- feols(as.formula(sprintf(
    "log_takers ~ i(year, intensity, ref = %d, ref2 = 'unexposed') | cod_mpio + %s^year",
    ref_year, dept)), data = df, cluster = ~cod_mpio)
  es_ct <- as.data.frame(coeftable(m_tak_es)); es_ci <- as.data.frame(confint(m_tak_es))
  es_ct$term <- rownames(es_ct)
  es_ct <- es_ct[grepl("^year::", es_ct$term), ]
  es_ct$year  <- as.integer(sub("^year::([0-9]{4}).*$", "\\1", es_ct$term))
  es_ct$group <- sub(".*intensity:*", "", es_ct$term)
  es_df <- bind_rows(
    tibble(year = es_ct$year, group = es_ct$group, estimate = es_ct[["Estimate"]],
           ci_low = es_ci[es_ct$term, 1], ci_high = es_ci[es_ct$term, 2]),
    tibble(year = ref_year, group = c("low", "high"), estimate = 0, ci_low = 0, ci_high = 0))
  es_df$group <- factor(es_df$group, levels = c("low", "high"), labels = c("Low", "High"))
  p_takers <- ggplot(es_df, aes(year, estimate, colour = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#c3c2b7") +
    add_ceasefire_line(ref_year) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2,
                  position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = farc_palette) +
    theme_capstone() +
    labs(colour = "FARC intensity", x = "Year",
         y = sprintf("Effect on log(1+takers), rel. to %d", ref_year),
         title = "Composition: Saber-11 test-takers by FARC-exposure intensity",
         subtitle = "Event study of the denominator of the higher-education access rate")
  ggsave(file.path(results_img, "mechanism_composition_takers.png"), p_takers, width = 8, height = 5, dpi = 300)

  # Pre-trends for the composition outcomes: joint nullity of the pre-period
  # (2010-2012) intensity coefficients, so "denominator grew / absolute held"
  # rests on outcomes without differential pre-trends. (Same three-arm form as
  # the main event studies; tests low & high arms jointly.)
  m_nhe_es <- feols(as.formula(sprintf(
    "asinh_nhe ~ i(year, intensity, ref = %d, ref2 = 'unexposed') | cod_mpio + %s^year",
    ref_year, dept)), data = df, cluster = ~cod_mpio)
  wald_pre_comp <- function(model, label, arm = "") {
    suffix <- if (nzchar(arm)) paste0("::", arm) else ""
    w <- fixest::wald(model, paste0("year::(", paste(pre_years, collapse = "|"), "):intensity", suffix))
    tibble(spec = label, arm = if (nzchar(arm)) arm else "low & high",
           F_stat = as.numeric(w["stat"]), df1 = as.numeric(w["df1"]),
           df2 = as.numeric(w["df2"]), p_value = as.numeric(w["p"]))
  }
  comp_pretrends <- bind_rows(
    wald_pre_comp(m_tak_es, "log(1+takers)"),
    wald_pre_comp(m_tak_es, "log(1+takers)",      "high"),
    wald_pre_comp(m_nhe_es, "asinh(HE-entrants)"),
    wald_pre_comp(m_nhe_es, "asinh(HE-entrants)", "high")
  )
  cat("\n=== Composition pre-trends (joint nullity, pre-period intensity coefs) ===\n")
  print(as.data.frame(comp_pretrends))
  write.csv(comp_pretrends, paste0(data_final, "/mechanism_composition_pretrends.csv"), row.names = FALSE)
}

cat("\nSaved mechanism CSVs: mechanism_tests_pre.csv, mechanism_tests_post.csv,",
    "mechanism_coefs.csv, mechanism_vacuum_static.csv, mechanism_vacuum_pretrends.csv,",
    "mechanism_pop_continuous.csv, mechanism_composition.csv, mechanism_composition_pop.csv,",
    "mechanism_composition_pretrends.csv\n")

