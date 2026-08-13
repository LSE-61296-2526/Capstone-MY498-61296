# ===================================================================
# Goal: shared preprocessing and treatment-identification functions
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
# ===================================================================

# -------------------------------------------------------------------
# 1.1 Violence data
# -------------------------------------------------------------------

# To identify events perpetrated by FARC and unidentified events
FARC_SET <- c("FARC", "FARC/ELN", "FARC/EPL")
UNATTR   <- c("NO APLICA", "NO IDENTIFICADO", "NO IDENTIFICADA",
              "PRESENCIA GAO SIN ATRIBUCIÓN", "GRUPO ARMADO NO DIRIMIDO")

# Data processing function for AS, AT, DB, DF, MA, MI, RU, SE, VS
process_data_violence <- function(path) {

  df <- read.xlsx(path, sheet = 1) |>
    clean_names() |>
    rename(year = ano) |>
    # Actions caused by FARC identifier and unidentified events
    mutate(farc_attack = if_else(descripcion_presunto_responsable %in% FARC_SET, 1, 0, missing = 0),
           unidentified_attack = if_else(descripcion_presunto_responsable %in% UNATTR, 1, 0, missing = 0),
           year = as.numeric(year)) |>
    filter(year >= 2011, year <= 2023,
           municipio != "SIN INFORMACION", departamento != "EXTERIOR")

  # Unidentified events counting
  df_collap_unid <- df |>
    filter(unidentified_attack == 1) |>
    group_by(codigo_dane_de_municipio, departamento, municipio, year) |>
    summarise(n_events_unid = n(), .groups = "drop") |>
    select(codigo_dane_de_municipio, year, n_events_unid)

  # Identification events in which FARC was involved
  df_collap <- df |>
    group_by(codigo_dane_de_municipio, departamento, municipio, year, farc_attack) |>
    select(codigo_dane_de_municipio, municipio, departamento, year, region, farc_attack, total_de_victimas_del_caso) |>
    summarise(n_eventos = n(),
              total_victims = sum(total_de_victimas_del_caso), .groups = "drop") |>
    left_join(df_collap_unid, by = c("codigo_dane_de_municipio", "year"))

  return(df_collap)
}

# -------------------------------------------------------------------
# 1.2 Saber 11 - Education data
# -------------------------------------------------------------------
process_data_saber <- function(path) {

  df <- read_delim(path, col_names = TRUE, delim = ";", locale = locale(decimal_mark = ","))

  # Rename variables
  if ("estu_etnia" %in% names(df)) {
    df <- df |>
      mutate(
        estu_tieneetnia = if_else(is.na(estu_etnia) | estu_etnia == "Ninguno", "No", "Si")
      ) |>
      select(-estu_etnia)
  }

  df <- df |>
    filter(estu_estudiante == "ESTUDIANTE") |>
    select(periodo, estu_consecutivo, cole_area_ubicacion, cole_caracter, cole_cod_dane_establecimiento,
           cole_cod_dane_sede, cole_cod_depto_ubicacion, cole_cod_mcpio_ubicacion, cole_naturaleza,
           estu_cod_depto_presentacion, estu_cod_mcpio_presentacion, estu_cod_reside_depto, estu_cod_reside_mcpio,
           estu_tieneetnia, estu_genero)

  return(df)
}

# -------------------------------------------------------------------
# 2. Treatment identification: build the exposure cross-section for
#    ONE window. Improvement vs. the previous version: takes the
#    TOTAL attacks in the window divided by the mean population of
#    the period, instead of summing annual rates. This is more
#    stable and the interpretation (attacks per 10,000 inhabitants)
#    is clear.
# -------------------------------------------------------------------

build_exposure <- function(df, window) {
  df |>
    filter(year %in% window) |>
    mutate(
      pop_per10 = poblacion / 10000
    ) |>
    group_by(cod_mpio) |>
    summarise(
      farc_attacks_tot = sum(total_farc_events, na.rm = TRUE),
      pop_per10_mean   = mean(pop_per10, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      attacks_pc = farc_attacks_tot / pop_per10_mean,  # Events per 10,000 inhabitants
      attacks_pc = if_else(is.finite(attacks_pc), attacks_pc, NA_real_)
    )
}

# -------------------------------------------------------------------
# 3. Shared figure design system (all ggplot output across the project)
# -------------------------------------------------------------------
# One consistent, colorblind-safe categorical palette and theme, sourced by
# every script and by dissertation.qmd (via 00_setup.R), so the same category
# always has the same color and every figure shares the same typography.
# Palette is the CVD-safe 8-hue categorical set from the project's dataviz
# design-system reference (fixed order, worst adjacent CVD ΔE 24.2, well
# clear of the >=12 target); we use slots 1 (blue), 3 (yellow), 6 (red).

farc_palette <- c(
  "Unexposed"    = "#2a78d6",
  "Control"      = "#2a78d6",
  "Low"          = "#36972b",
  "low"          = "#36972b",
  "High"         = "#e34948",
  "high"         = "#e34948",
  "FARC-exposed" = "#e34948",
  "Treated"      = "#e34948"
)

#' Shared theme for every figure in the dissertation and pipeline.
#' A theme_minimal() base with recessive hairline gridlines, muted axis
#' ink, a bold title, and a bottom legend with a bold legend title -- so
#' every figure reads the same way regardless of which script produced it.
theme_capstone <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05),
                                                 colour = "#0b0b0b"),
      plot.subtitle    = ggplot2::element_text(colour = "#52514e", size = ggplot2::rel(0.9),
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption     = ggplot2::element_text(colour = "#898781", size = ggplot2::rel(0.75),
                                                 hjust = 0, margin = ggplot2::margin(t = 8)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
      axis.line        = ggplot2::element_line(colour = "#c3c2b7", linewidth = 0.3),
      axis.title       = ggplot2::element_text(colour = "#52514e"),
      axis.text        = ggplot2::element_text(colour = "#898781"),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),
      strip.text       = ggplot2::element_text(face = "bold")
    )
}

#' Add a self-labeled ceasefire reference line to an event-study/trend plot,
#' so the reference date is legible without reading the caption. Returns a
#' list of ggplot layers to add with `+`.
#' @param xintercept x-position of the reference line (e.g. ref_year + 0.5).
#' @param label Text to print beside the line.
#' @param y_pos Vertical position for the label, in [0,1] of the panel (NPC).
add_ceasefire_line <- function(xintercept, label = "Reference year", y_pos = 0.97) {
  list(
    ggplot2::geom_vline(xintercept = xintercept, linetype = "dashed",
                         colour = "#222221", linewidth = 0.4),
    ggplot2::annotate("text", x = xintercept, y = Inf, label = paste0("  ", label),
                       hjust = 0, vjust = 1.3, size = 3, colour = "#52514e")
  )
}

# Compact summary of a cross-section: % of zeros and quartiles of positives.
resumen_exposicion <- function(xs, etiqueta = "") {
  n_mun  <- nrow(xs)
  n_zero <- sum(xs$attacks_pc == 0)
  pos    <- xs$attacks_pc[xs$attacks_pc > 0]
  qpos   <- if (length(pos) > 0) quantile(pos, c(.25, .50, .75)) else rep(NA, 3)
  tibble(
    ventana       = etiqueta,
    n_municipios  = n_mun,
    n_cero        = n_zero,
    pct_cero      = round(100 * n_zero / n_mun, 1),
    n_positivos   = length(pos),
    p25_positivos = round(qpos[1], 3),
    mediana_pos   = round(qpos[2], 3),
    p75_positivos = round(qpos[3], 3)
  )
}
