# ===================================================================
# Goal: shared project paths and helper functions
#       Sourced by every pipeline script (01-09) so that path
#       definitions and preprocessing functions live in one place.
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
# ===================================================================

project_root    <- "C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone"
coding_dir      <- file.path(project_root, "2. Coding")
data_violence   <- file.path(project_root, "1. Data/SIEVCAC")
data_education  <- file.path(project_root, "1. Data/Saber11")
data_mun        <- file.path(project_root, "1. Data/Municipality")
data_population <- file.path(project_root, "1. Data/Population")
data_final      <- file.path(project_root, "1. Data/Final")
results_img     <- file.path(project_root, "3. Results/Descriptive statistics")

source(file.path(coding_dir, "00_functions.R"))
