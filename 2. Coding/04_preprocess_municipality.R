# ============================================================
# Goal: Join Municipal indicators
# Sources:
#   - TerriData_PublicFinances: TerriData_Dim7.xlsx
#   - Inflation : 
#   - Coca crops: 
# ============================================================

# Packages and libraries

library(tidyverse)
library(openxlsx)
library(janitor)

#===================================================================
# Setup (paths + shared functions)

source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Inflation 

base_year <- 2023

df_inflation <- read.xlsx(paste0(data_mun, "/IPC_Índice de Precios al Consumidor.xlsx"),startRow = 2) |>
  clean_names() |>
  slice(-c(863, 864, 865)) |>
  mutate(date = as.Date(yyyy_mm_dd)) |>
  filter(month(date)==12 & (year(date)>=2010 & year(date)<=2023 )  ) |>
  mutate( year = year(date) ) |>
  mutate(cpi = indice / indice[year == base_year] * 100) |>
  select(year, cpi)

#===================================================================
# Coca crops 

df_coca <- read.csv(paste0(data_mun, "/Coca_crops_2001_2023.csv")) |>
 clean_names() |>
 mutate(across(starts_with("x"),
                ~ . |>
                  str_remove_all("-") |>
                  str_trim() |>
                  str_replace_all(",", "") |>
                  as.numeric())) |>
  pivot_longer(starts_with("x"), names_to='year', values_to = 'coca_crops') |>
  mutate(year = as.numeric(str_remove_all(year, "x"))) |>
  filter(year >=2010 & year<=2023) |>
  mutate(
    cod_dpto = str_pad(coddepto, width = 2, pad = "0"),
    cod_mpio = str_pad(codmpio, width = 5, pad = "0")
  )|>
  select(cod_dpto, cod_mpio, year, coca_crops)

#===================================================================
# Terridata information 

# Public finance indicators 
indicators <- c("Ingresos totales per cápita",
  "Gastos totales per cápita",
  "Transferencias nacionales (SGP, etc.) per cápita")

df_budget <- read.xlsx(paste0(data_mun, "/TerriData_PublicFinances/TerriData_Dim7.xlsx"),  sheet = "Hoja01") |>
  clean_names() |>
  rename(year = ano) |>
  filter((indicador %in% indicators) & year>=2010 & year<=2023) |>
  mutate(values = as.numeric(gsub(",", ".", gsub("\\.", "", dato_numerico)))) |>
  mutate(values = if_else(year !=2023 , values/1000000, values)  ) |>
  select(codigo_departamento,codigo_entidad, indicador, year, values)  |>
  pivot_wider(
    names_from = indicador,
    values_from = values
  ) |>
  rename(cod_dpto = codigo_departamento,
         cod_mpio = codigo_entidad,
         total_expenses = "Gastos totales per cápita",
         total_income = "Ingresos totales per cápita",
         total_transfers = "Transferencias nacionales (SGP, etc.) per cápita"
       ) |>
  left_join(df_inflation, by = "year") |>
  mutate(
    total_expenses_real = (total_expenses/cpi)*100 ,
    total_income_real = (total_income/cpi)*100,
    total_transfers_real = (total_transfers/cpi)*100
  ) |>
  left_join(df_coca, by = c("year", "cod_dpto", "cod_mpio")) |>
  mutate(coca_crops = if_else(is.na(coca_crops), 0, coca_crops)) 

colSums(is.na(df_budget))

# Clean violence data set
saveRDS(df_budget, paste0(data_mun, "/municipal_panel2010_2023.rds"))

table(df_budget$cod_dpto, df_budget$year) 
length(unique(df_budget$cod_mpio))