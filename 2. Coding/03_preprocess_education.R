#====================================================================
# Goal: tidy Saber 11 data sets for capstone project 
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: April 2026
#====================================================================

# Packages and libraries
library(tidyverse)

#===================================================================
# Setup (paths + shared functions)

source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Transito a educacion superior 

df_ed <- read_delim(paste0(data_education, "/Llave Examen Saber 11 2010 - 2024 - SNIES.txt"), col_names = TRUE, delim = ";" )
  
# Saber 11 
dta_periods <-  c("20101", "20102", "20111", "20112", "20121", "20122", "20131", "20132",
                  "20141", "20142", "20151", "20152", "20161", "20162", "20171", "20172",
                  "20181", "20182", "20191", "20192", "20201", "20202", "20211", "20212",
                  "20221", "20222", "20231", "20232")
  
events_list <- lapply(dta_periods, function(period) {
  
  print(period)
  file <- paste0(data_education, "/Examen_Saber_11_", period, ".txt")
  df <- process_data_saber(file) 
  return(df)
})

# Sub-files names 
names(events_list) <- paste0("saber11_", dta_periods)

#===================================================================
# Join all the information 

saber_panel <- Reduce(function(x, y) {
  bind_rows(x, y)
}, events_list)

nrow(saber_panel)
saber_panel <- left_join(saber_panel, df_ed, by = "estu_consecutivo")

rm(events_list, df_ed)

#==================================================================
# Checks 

nrow(saber_panel)       # 7978288 obs between 2010 and 2023
ncol(saber_panel)       # 40 variables 
str(saber_panel)        

colSums(is.na(saber_panel))  # Relevant notes: 34695 obs has missing value 
# in estu_cod_reside_mcpio, which I am going to use to identify where the students live

saber_panel <- saber_panel |>
  mutate( estu_cod_reside_mcpio = if_else(is.na(estu_cod_reside_mcpio),
 estu_cod_mcpio_presentacion, estu_cod_reside_mcpio ), 
          estu_cod_reside_depto = if_else(is.na(estu_cod_reside_depto),
 estu_cod_depto_presentacion, estu_cod_reside_depto) 
) |>
  filter(!is.na(estu_cod_reside_mcpio)) # Drop 13 obs from the 34695 initial missing values.

table(saber_panel$estu_genero)
table(saber_panel$estu_tieneetnia)

#==================================================================
# When they take the Saber11 exam 

saber_panel$year <- round(saber_panel$periodo /10, digits = 0)
colSums(is.na(saber_panel))
#==================================================================
# Access to higher education identifier 

saber_panel$transit_he <- if_else(
  !is.na(saber_panel$anio_matricula_1 - saber_panel$year) 
    & (saber_panel$anio_matricula_1 - saber_panel$year == 1),
  1, 
  0
)

#==================================================================
# Access to higher education per year and municipality 
total_prop <- saber_panel |> 
  filter(year>=2010) |>
  mutate(
    cod_dpto = str_pad(estu_cod_reside_depto, width = 2, pad = "0"),
    cod_mpio = str_pad(estu_cod_reside_mcpio, width = 5, pad = "0")
  ) |>
  select(year, cod_dpto, cod_mpio, transit_he ) |>
  group_by(year, cod_dpto, cod_mpio ) |>
  summarise( numstudents = n(), 
             pro_he = mean(transit_he))

weights_df <- total_prop |>
  filter(year %in% 2010:2014) |>
  group_by(cod_dpto, cod_mpio) |>
  summarise(w_takers = mean(numstudents, na.rm = TRUE), .groups = "drop")

prop_bysex <- saber_panel |> 
  filter(!is.na(estu_genero) & year>=2010) |>
  mutate(
    cod_dpto = str_pad(estu_cod_reside_depto, width = 2, pad = "0"),
    cod_mpio = str_pad(estu_cod_reside_mcpio, width = 5, pad = "0")
  ) |>
  select(year, cod_dpto, cod_mpio, estu_genero, transit_he ) |>
  group_by(year, cod_dpto, cod_mpio, estu_genero ) |>
  summarise( numstudents = n(), 
             pro_he = mean(transit_he)) |>
  pivot_wider(
    names_from = estu_genero,
    values_from = c('pro_he','numstudents')
  )

panel_access <- total_prop |>
  left_join(prop_bysex, by = c("year", "cod_dpto", "cod_mpio" )) |>
  left_join(weights_df, by = c("cod_dpto", "cod_mpio" ))

#==================================================================
table(panel_access$year)

# Clean violence data set
saveRDS(panel_access, paste0(data_education, "/saber11_panel2010_2023.rds"))
