#===================================================================
# Goal: join municipal population data, 2010-2024
# Sources:
#   - PoblacionMun2005a2017.xlsx  (sheet: NuevaMpal)
#   - PoblacionMun2018a2042.xlsx  (sheet: PobMunicipalxÁrea)
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(readxl)

#===================================================================
# Setup (paths + shared functions)

source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Population data

# 1. Load 2005-2017 data
# Column order: DP, DPNOM, DPMP (mpio code), MPIO (mpio name), AÑO, ÁREA, Población

df_2005 <- read_excel(
  paste0(data_population, "/PoblacionMun2005a2017.xlsx"),
  sheet = "NuevaMpal",
  skip  = 3          # skip the 3 header/title rows before the column names row
)

# Standardise column names
df_2005 <- df_2005 |>
  rename(
    cod_dpto   = 1,   # DP
    dpto       = 2,   # DPNOM
    municipio   = 3,   # DPMP  <- municipality name
    cod_mpio    = 4,   # MPIO  <- municipality code in this file
    year       = 5,   # AÑO
    area       = 6,   # ÁREA GEOGRÁFICA
    poblacion  = 7    # Población
  )

# 2. Load 2018-2042 data
# Column order: DP, DPNOM, MPIO (mpio code), DPMP (mpio name), AÑO, ÁREA, TOTAL

df_2018 <- read_excel(
  paste0(data_population, "/PoblacionMun2018a2042.xlsx"),
  sheet = "PobMunicipalxÁrea",
  skip  = 3
)

df_2018 <- df_2018 |>
  rename(
    cod_dpto   = 1,   # DP
    dpto       = 2,   # DPNOM
    cod_mpio   = 3,   # MPIO  <- municipality code in this file
    municipio  = 4,   # DPMP  <- municipality name in this file
    year       = 5,   # AÑO
    area       = 6,   # ÁREA GEOGRÁFICA
    poblacion  = 7    # TOTAL
  )

# 3. Keep only "Total" rows (all areas combined)
df_2005_total <- df_2005 |>
  filter(area == "Total", year <= 2017) |>
    mutate(
    cod_dpto  = as.character(cod_dpto),
    cod_mpio  = as.character(cod_mpio),
    year      = as.integer(year),
    poblacion = as.numeric(poblacion)
  )

length(unique(df_2005_total$cod_mpio))
length(unique(df_2005_total$year))
sum(is.na(df_2005_total$poblacion))


df_2018_total <- df_2018 |>
  filter(area == "Total")  |>
    mutate(
    cod_dpto  = as.character(cod_dpto),
    cod_mpio  = as.character(cod_mpio),
    year      = as.integer(year),
    poblacion = as.numeric(poblacion)
  )

length(unique(df_2018_total$cod_mpio))
length(unique(df_2018_total$year))
sum(is.na(df_2018_total$poblacion))

# 4. Stack both files and filter years 2010-2024
main_cities <- c("11001", "05001", "76001",	"08001",
	"13001", "68001",	"54001",	"73001", "66001", "17001",
  "47001",	"50001",	"63001"
)

poblacion_mpal <- bind_rows(df_2005_total, df_2018_total) |>
  filter(year >= 2010, year <= 2024)|>
  select(cod_dpto, cod_mpio, year, poblacion) |>
  arrange(cod_mpio, year) |>
  mutate(main_city = if_else(cod_mpio %in% main_cities, 1, 0))


length(unique(poblacion_mpal$cod_mpio))
length(unique(poblacion_mpal$year))
sum(is.na(poblacion_mpal$poblacion))

# 5. Quick checks
cat("Years covered:", sort(unique(poblacion_mpal$year)), "\n")
cat("Number of municipalities:", n_distinct(poblacion_mpal$cod_mpio), "\n")
cat("Total rows:", nrow(poblacion_mpal), "\n")
glimpse(poblacion_mpal)

# Distribution of the population in 2010.
dfx <- poblacion_mpal |>
  filter(year == 2010) |>
  mutate(poblacion = poblacion/10000) |>
  filter(poblacion >= 2)

pop_plot <-  ggplot(dfx, aes(x = poblacion)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15,
                 fill = "lightblue",
                 color = "black") +
  labs(title = "Population distribution in 2010",
       x = "Number of inhabitants",
       y = "Density") +
  theme_minimal()
pop_plot

# 6. Export
saveRDS(poblacion_mpal, paste0(data_population, "/poblacion_municipal_2010_2024.rds"))
