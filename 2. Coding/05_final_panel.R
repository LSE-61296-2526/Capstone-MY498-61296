#====================================================================
# Goal: Final panel - Join violence, education and municipal inf
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
# Join all data sets

access_panel <- readRDS(paste0(data_violence, "/violence_panel2010_2024.rds"))  |>
  left_join(readRDS(paste0(data_mun, "/municipal_panel2010_2023.rds")), by=c("year", "cod_mpio" )) |>
  left_join(readRDS(paste0(data_education, "/saber11_panel2010_2023.rds")) |> select(-cod_dpto),
            by=c("year", "cod_mpio" ))

# Saving final version
saveRDS(access_panel, paste0(data_final, "/final_panel_2010_2023.rds"))
table(access_panel$year) 
length(unique(access_panel$cod_mpio))  # 1123 municipalities 
