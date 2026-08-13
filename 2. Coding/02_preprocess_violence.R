#===================================================================
# Goal: tidy violence data sets (SIEVCAC)
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: April 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(openxlsx)
library(janitor)

#===================================================================
# Setup (paths + shared functions)

source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# War actions
eventsWA <- read.xlsx(paste0(data_violence, "/CasosAB_202509.xlsx"), sheet = 1) |>
  clean_names() |>
  rename(year = ano) |>
  #Actions caused by FARC identifier and unidentified events
  mutate(farc_attack = if_else(descripcion_grupo_armado_1 %in% FARC_SET |
    descripcion_grupo_armado_2 %in% FARC_SET |
    descripcion_grupo_armado_3 %in% FARC_SET, 1, 0, missing = 0),
    unidentified_attack  = if_else(descripcion_grupo_armado_1 %in% UNATTR |
    descripcion_grupo_armado_2 %in% UNATTR |
    descripcion_grupo_armado_3 %in% UNATTR, 1, 0, missing = 0),
    year = as.numeric(year)
) |>
  filter(year >= 2010, year <= 2023,
  municipio != "SIN INFORMACION", departamento != 'EXTERIOR')

# Unidentified events counting
eventsWA_unid <- eventsWA |>
  filter(unidentified_attack == 1) |>
  group_by(codigo_dane_de_municipio, departamento, municipio, year) |>
  summarise(n_events_unid_WA = n(), .groups = "drop") |>
  select(codigo_dane_de_municipio, year, n_events_unid_WA)

# Identification events in which farc was involved
eventsWA_collap <- eventsWA |>
  group_by(codigo_dane_de_municipio, departamento, municipio, year, farc_attack) |>
  select(codigo_dane_de_municipio, municipio, departamento, year, region, farc_attack, total_de_victimas_del_caso) |>
  summarise(n_eventos_WA = n(),
  victims_WA = sum(total_de_victimas_del_caso), .groups = "drop") |>
  left_join(eventsWA_unid, by = c('codigo_dane_de_municipio', 'year'))

#-------------------------------------------------------------------
# Damage to settlements
eventsDP <- read.xlsx(paste0(data_violence,"/CasosAP_202509.xlsx"), sheet = 1) |>
  clean_names() |>
  rename(year = ano) |>
  #Actions caused by FARC identifier and unidentified events
  mutate(farc_attack = if_else(descripcion_grupo_armado_1 %in% FARC_SET |
    descripcion_grupo_armado_2 %in% FARC_SET |
    descripcion_grupo_armado_3 %in% FARC_SET, 1, 0, missing = 0),
    unidentified_attack  = if_else(descripcion_grupo_armado_1 %in% UNATTR |
    descripcion_grupo_armado_2 %in% UNATTR |
    descripcion_grupo_armado_3 %in% UNATTR, 1, 0, missing = 0),
    year = as.numeric(year)
) |>
  filter(year >= 2010, year <= 2023,
  municipio != "SIN INFORMACION", departamento != 'EXTERIOR')

# Unidentified events counting
eventsDP_unid <- eventsDP |>
  filter(unidentified_attack == 1) |>
  group_by(codigo_dane_de_municipio, departamento, municipio, year) |>
  summarise(n_events_unid_DP = n(), .groups = "drop") |>
  select(codigo_dane_de_municipio, year, n_events_unid_DP)

# Identification events in which farc was involved
eventsDP_collap <- eventsDP |>
  group_by(codigo_dane_de_municipio, departamento, municipio, year, farc_attack) |>
  select(codigo_dane_de_municipio, municipio, departamento, year, region, farc_attack, total_de_victimas_del_caso) |>
  summarise(n_eventos_DP = n(),
  victims_DP = sum(total_de_victimas_del_caso), .groups = "drop") |>
  left_join(eventsDP_unid, by = c('codigo_dane_de_municipio', 'year'))

#-------------------------------------------------------------------
# Data for: Selective murders (AS), Terrorists Attacks (AT), Damage to property (DB),
# Forced Disappearance (DF), Antipersonnel mines (MI), Massacres (MA)
# Forced recruitment (RU), Kidnapping (SE), Sexual abuse (VS)

dta_sets <-  c("AS", "AT", "DB", "DF", "MA", "MI", "RU", "SE", "VS")

events_list <- lapply(dta_sets, function(code) {

  file <- paste0(data_violence, "/Casos", code, "_202509.xlsx")

  df <- process_data_violence(file) |>
    rename(
      !!paste0("n_eventos_", code) := n_eventos,
      !!paste0("victims_", code) := total_victims,
      !!paste0("n_events_unid_", code) := n_events_unid
    )

  return(df)
})

# Sub-files names
names(events_list) <- paste0("events", dta_sets, "_collap")

events_list[['eventsWA_collap']] <- eventsWA_collap
events_list[['eventsDP_collap']] <- eventsDP_collap

rm(eventsDP, eventsDP_collap, eventsWA, eventsWA_collap )

#-------------------------------------------------------------------
# Merge all the information

dta_sets <-  c("AS", "AT", "DB", "DF", "MA", "MI", "RU", "SE", "VS", "DP", "WA")

join_vars <- c("codigo_dane_de_municipio", "departamento",
               "municipio", "year", "farc_attack")

# Merge all datasets
merged_data <- Reduce(function(x, y) {
  full_join(x, y, by = join_vars)
}, events_list) |>
  group_by(codigo_dane_de_municipio, departamento, municipio, year) |>
  mutate(
    across(
      starts_with("n_events_unid"),
      ~ ifelse(
          is.na(.x),
          ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE)),
          .x
        )
    )
  ) |>
  ungroup()

#-------------------------------------------------------------------
# Add municipalities identifiers  (divipola - official national encoder)

divipola <- read.xlsx(paste0(data_violence, "/DIVIPOLA-_Códigos_municipios_20260507.xlsx"), sheet = 1) |>
  clean_names() |>
  rename(
    cod_dep = codigo_departamento,
    departamento = nombre_departamento,
    cod_mpio = codigo_municipio,
    municipio = nombre_municipio) |>
    select(cod_dep, departamento, cod_mpio, municipio)
length(unique(divipola$cod_mpio))  # 1122 municipalities

merged_data <- merged_data |>
  # Pivot data set to have the number of victims and events in which FARC was involved and other actors
  # farc_attack = 1 if Farc was one or the main perpatraidor and 0 otherwise
  pivot_wider(
    names_from = farc_attack,
    values_from = c(n_eventos_WA,	victims_WA,	n_eventos_DP,	victims_DP,	n_eventos_AS,
    	victims_AS,	n_eventos_AT,	victims_AT,	n_eventos_DB,	victims_DB,	n_eventos_DF,
      victims_DF,	n_eventos_MA,	victims_MA,	n_eventos_MI,	victims_MI,	n_eventos_RU,	victims_RU,
      n_eventos_SE,	victims_SE,	n_eventos_VS,	victims_VS)) |>
  # Replace NA values with 0. Validate this!
  mutate(across(
  matches("^(n_eventos|victims|n_events_unid)_"),
  ~ replace_na(.x, 0)
  ))  |>
  rename(cod_mpio = codigo_dane_de_municipio) |>
  right_join(divipola, by = "cod_mpio")  |>
  select(-departamento.x, -municipio.x)
length(unique(merged_data$cod_mpio))  # 1122 municipalities
table(merged_data$year) # Number of municipalities with at least  1 event

# Population
population <- readRDS(paste0(data_population, "/poblacion_municipal_2010_2024.rds")) |>
  select(year, cod_mpio, poblacion, main_city)
length(unique(population$cod_mpio))  # 1123 municipalities
colSums(is.na(population))

# Complete panel information for all of the municipalities
final_panel <- merged_data |>
  mutate(year = if_else(is.na(year), 2010, year)) |>
  rename(departamento = departamento.y, municipio = municipio.y) |>
  complete(cod_mpio, year) |>
  arrange(cod_mpio, year) |>
  group_by(cod_mpio) |>
  fill(cod_dep, departamento, municipio, .direction = "downup") |>
  ungroup() |>
  mutate(
    cod_dep = if_else(cod_mpio=='27086', '27', cod_dep ),
    departamento = if_else(cod_mpio=='27086', 'CHOCO', departamento ),
    municipio = if_else(cod_mpio=='27086', 'BELEN DE BAJIRA', municipio )
  ) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  left_join(population, by=c('cod_mpio', 'year'))

length(unique(final_panel$cod_mpio))

# Farc event variables:
# To build treatment measure of FARC exposure, I am gonna use only AS,
# AT, DB, MI, SE, WA

# All events
# farc_event <- c("n_eventos_WA_1", "n_eventos_DP_1", "n_eventos_AS_1", "n_eventos_AT_1",
#               "n_eventos_DB_1", "n_eventos_DF_1", "n_eventos_MA_1", "n_eventos_MI_1",
#               "n_eventos_RU_1", "n_eventos_SE_1", "n_eventos_VS_1")

# Only the selection
farc_event <- c("n_eventos_WA_1", "n_eventos_AS_1", "n_eventos_AT_1",
               "n_eventos_DB_1", "n_eventos_MI_1", "n_eventos_SE_1")

nofarc_event <- c("n_eventos_WA_0", "n_eventos_AS_0", "n_eventos_AT_0",
               "n_eventos_DB_0", "n_eventos_MI_0", "n_eventos_SE_0")

uniden_event <- c("n_events_unid_WA", "n_events_unid_AS", "n_events_unid_AT",
               "n_events_unid_DB", "n_events_unid_MI", "n_events_unid_SE")

final_panel <- final_panel |>
  mutate(
    total_farc_events = rowSums(
      across(all_of(farc_event)),
      na.rm = TRUE
    ),
    total_nofarc_events = rowSums(
      across(all_of(nofarc_event)),
      na.rm = TRUE
    ),
    total_events = total_farc_events+total_nofarc_events,
    total_uniden = rowSums(
      across(all_of(uniden_event)),
      na.rm = TRUE
    )
  )

# Statistics

# Table per event, counting the total events, FARC related, No FARC related and unidentified
dta_sets_short <-  c("AS", "AT", "DB", "MI", "SE", "WA")

table_events <- map_dfr(dta_sets, function(s) {

  final_panel |>
    filter(year >= 2010 & year <= 2014) |>
    summarise(
      dataset = s,
      total_farc_events   = sum(.data[[paste0("n_eventos_", s, "_1")]], na.rm = TRUE),
      total_nofarc_events = sum(.data[[paste0("n_eventos_", s, "_0")]], na.rm = TRUE),
      total_uniden        = sum(.data[[paste0("n_events_unid_", s)]], na.rm = TRUE)
    ) |>
    mutate(total_events = total_farc_events + total_nofarc_events)
})

# Distribution of events per year
final_panel |>
  filter(year >= 2010 & year<= 2014) |>
  group_by(year) |>
  summarise( total_events = sum(total_events),
            total_farc = sum(total_farc_events),
            total_nofarc = sum(total_nofarc_events),
            total_uniden = sum(total_uniden)
          ) |>
  ungroup()

final_panel |>
  filter(year >= 2010 & year<= 2014 & total_events>0) |>
  summarise( total_mun = n_distinct(cod_mpio),
             mun_with_farc = n_distinct(cod_mpio[total_farc_events > 0]),
             mun_with_nofarc = n_distinct(cod_mpio[total_farc_events == 0 & total_nofarc_events >0]),
             mun_with_alluniden = n_distinct(total_uniden == total_events),
             mun_nofarc_uniden = n_distinct(cod_mpio[total_farc_events == 0 & total_uniden >0])
)

# Clean violence data set
saveRDS(final_panel, paste0(data_violence, "/violence_panel2010_2024.rds"))
table(final_panel$year)
length(unique(final_panel$cod_mpio))  # 1122 municipalities
