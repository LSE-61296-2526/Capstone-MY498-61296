#===================================================================
# Goal: treatment identification and descriptive statistics
# Author: Juliana Aragon
# LSE MY Capstone - Applied social data science
# Date: June 2026
#===================================================================

# Packages and libraries
library(tidyverse)
library(scales)

#===================================================================
# Setup (paths + shared functions)
source(file.path("C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone/2. Coding", "00_setup.R"))

#===================================================================
# Data
df <- readRDS(paste0(data_final,"/final_panel_2010_2023.rds"))
table(df$year)

#====================================================================
# FARC Municipality identification 

# Exposure window: STRICTLY prior to cessation (permanent cessation: 20-Dec-2014).
# At the annual level, 2014 is considered pre-cessation.
exp_wind_agree <- 2010:2016
exp_wind_cease <- 2010:2014

# pro_he[t] measures enrollment in t+1. The 2014 cohort enrols in 2015
# (post 20-Dec-2014 ceasefire), so exam-year 2014 is the first treated outcome period.
year_cease <- 2014 

#-------------------------------------------------------------------
# How to define the treatment? 
#-------------------------------------------------------------------

# List of 13 main cities in the country 
main_cities <- c("11001", "05001", "76001", "08001", "13001", "68001", "17001", "52001", "66001", "54001", "73001", "23001", "50001")
# Cities are: bogota, medellin, calu, barranquilla, cartagena, bucaramanga,
# manizales, pasto, pereira, cucuta, ibague, monteria, villavicencio

# Some municipalities does not have information for population 
subset(df, is.na(poblacion)) # One municipalities in Choco 27493

df_filtered <- df |>
  filter(!is.na(poblacion) & !(cod_mpio %in% main_cities) ) #Population per 100 thousand and remove 13 main cities 
length(unique(df_filtered$cod_mpio))  # 1109 municipalities only 1 without population information

# There are two options, before the ceasefire or before the peace agreement
time_windows  <- list(
  "2011-2016 (Peace agreement)" = exp_wind_agree,
  "2011-2014 (Ceasefire)"       = exp_wind_cease  )

table_windows <- imap_dfr(time_windows, function(w, nombre){
  build_exposure(df_filtered, w)|>
  resumen_exposicion(etiqueta = nombre)
})

cat("\n= Municipality distribution \n")
print(as.data.frame(table_windows))

# I am going to choose the ceasefire window to define the treatment due to is cleaner

# Distribution of per-capita events 
exp_panel <- build_exposure(df_filtered, exp_wind_cease)
exp_panel    <- exp_panel |> filter(is.finite(attacks_pc))
n_mun <- nrow(exp_panel)

n_zero <- sum(exp_panel$attacks_pc == 0)
cat(sprintf("\nChosen window %d-%d | municipalities: %d | zeros: %d (%.1f%%)\n",
            min(exp_wind_cease), max(exp_wind_cease), n_mun, n_zero,
            100 * n_zero / n_mun))

cat("\nPercentiles of attacks_pc (ALL municipalities):\n")
print(round(quantile(exp_panel$attacks_pc, c(.10,.25,.50,.75,.90,.95,.99)), 3))

pos     <- exp_panel$attacks_pc[exp_panel$attacks_pc > 0]
med_pos <- median(pos)

cat("\nQuartiles of attacks_pc among EXPOSED (>0):\n")
print(round(quantile(pos, c(.25,.50,.75)), 3))
cat(sprintf("Mean/median ratio (all): %s  (>> 1 = strong right skew)\n",
            ifelse(median(exp_panel$attacks_pc) == 0, "Inf",
                   round(mean(exp_panel$attacks_pc)/median(exp_panel$attacks_pc), 2))))

#==================================================================
#  Treatment definition 
#==================================================================

q3_cut <- quantile(exp_panel$attacks_pc, 0.75)   # top-quartile threshold (full dist.)

exp_panel <- exp_panel |>
  mutate(
    # A. Binary presence: exposed vs. unexposed municipalities
    treat_presence = as.integer(attacks_pc > 0),

    # B. High/low intensity conditional on >=1 attack 
    intensity = factor(case_when(
                  attacks_pc == 0       ~ "unexposed",
                  attacks_pc <= med_pos ~ "low",
                  TRUE                  ~ "high"),
                  levels = c("unexposed", "low", "high")),
    treat_high = as.integer(attacks_pc > med_pos),  # high vs. the rest

    # C. Continuous, LOG(1+ ATTACK PER CAPITA)
    farc_logint = log1p(attacks_pc)
  )

saveRDS(exp_panel, paste0(data_final, "/exposure_crosssection.rds"))

group_sizes <- tibble(
  definition = c("A. Presence (>0)",
                 "B. High intensity (> q3 of exposed)"),
  n_treated  = c(sum(exp_panel$treat_presence), sum(exp_panel$treat_high))
) |>
  mutate(n_control   = n_mun - n_treated,
         pct_treated = round(100 * n_treated / n_mun, 1))

cat("\n=== Group sizes by candidate definition ===\n")
print(as.data.frame(group_sizes))

cat("\nThree-arm design (definition B):\n")
print(table(exp_panel$intensity))

#==================================================================
#   Including treatment variables into the panel   
#==================================================================

treat_vars <- c("cod_mpio", "attacks_pc",  "treat_presence",
                "intensity", "treat_high", "farc_logint")

panel_t <- df_filtered |>
  inner_join(select(exp_panel, all_of(treat_vars)), by = "cod_mpio") |>   # drops the ~2 invalid
  mutate(
    cease        = as.integer(year >= year_cease),
    did_presence = cease * treat_presence,
    did_high     = cease * treat_high,
    did_cont     = cease * farc_logint,
    did_low    = cease * as.integer(intensity == "low"),
    did_high_u = cease * as.integer(intensity == "high")   # high vs. unexposed

  )

cat("\nSaved panel_with_treatment.rds | municipalities:",
    length(unique(panel_t$cod_mpio)), "\n")

saveRDS(panel_t, paste0(data_final,"/final_panel_2010_2023_treatment.rds"))

#==================================================================
# PLOTS (distribution of exposure)  
#==================================================================

p_hist <- exp_panel |>

  filter(attacks_pc > 0) |>
  ggplot(aes(attacks_pc)) +
  geom_histogram(
    binwidth = 3,      # ajusta según tu escala
    boundary = 0,
    fill = "steelblue",
    colour = "white"
  ) +
  labs(
    title = "FARC events per 10,000 inhabitants (2011-2014)",
    x = "Events per 10,000",
    y = "Municipalities"
  ) +
  theme_minimal()

ggsave(paste0(results_img, "/exposure_hist.png"), p_hist,     width = 8, height = 5, dpi = 300)
