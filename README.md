# Violence Reduction and Immediate Access to Higher Education
## Post-Conflict Evidence from Colombia

**MY498 Capstone Project**

MSc Applied Social Data Science, Department of Methodology, LSE
Candidate 61296 · Supervisor: Eleanor Power

This repository contains all the code, data references, and outputs needed to
reproduce the dissertation. The project estimates the effect of the 2014 FARC
ceasefire on immediate access to higher education across Colombian
municipalities, using a difference-in-differences (DiD) design that compares
FARC-exposed and non-exposed municipalities before and after the ceasefire.

---

## 1. Requirements

- **R ≥ 4.6.1** (the code uses the native pipe `|>`, which requires R ≥ 4.6.1).
- **RStudio** recommended (project developed with an `.Rproj`).
- A LaTeX distribution (e.g. **TinyTeX**: `quarto install tinytex`) and
  **Quarto ≥ 1.3** to render the dissertation.

### R packages

Install once before running the pipeline:

```r
install.packages(c(
  "tidyverse",   # data wrangling + ggplot2
  "readxl",      # .xlsx reading
  "openxlsx",    # .xlsx reading (SIEVCAC/TerriData)
  "janitor",     # clean_names()
  "scales",      # axis formatting
  "fixest",      # fixed-effects / event-study estimation
  "sf",          # spatial join + choropleth maps
  "broom",       # tidy model output
  "HonestDiD"    # Rambachan–Roth sensitivity analysis
))
```

`HonestDiD` is on CRAN; if it is unavailable, install from GitHub with
`remotes::install_github("asheshrambachan/HonestDiD")`. Script `07` installs it
automatically if missing.

---

## 2. One thing to change before running

Every script sources a single setup file that defines all paths. Open
`2. Coding/00_setup.R` and set `project_root` to wherever you cloned the repo:

```r
project_root <- "C:/Users/Juliana/Documents/LSE_ASD_JA/Capstone"
```

All other paths (data, results) derive from `project_root`, so this is the only
line you need to edit. **Note:** the individual scripts also hard-code this path
in their `source(...)` line at the top — update those too if you move the repo,
or replace them with `source("00_setup.R")` after setting the working directory
to `2. Coding`.

---

## 3. Folder structure

```
Capstone/
├── 0. Documents/          # presentation, ethics form, literature (PDFs, gitignored)
├── 1. Data/               # raw inputs + generated panels (gitignored)
│   ├── SIEVCAC/           # RAW: conflict-violence microdata (CNMH/SIEVCAC) + DIVIPOLA codes
│   ├── Saber11/           # RAW: ICFES Saber 11 exam records + SNIES higher-ed key
│   ├── Municipality/      # RAW: DNP TerriData, UNODC coca crops, DANE IPC, DANE MGN geojson
│   ├── Population/         # RAW: DANE municipal population projections
│   └── Final/             # GENERATED: analysis panels, result tables (.rds / .csv)
├── 2. Coding/             # all R scripts (the pipeline)
├── 3. Results/            # GENERATED: figures (.png), gitignored
│   └── Descriptive statistics/
└── 4. Dissertation/       # Quarto source (.qmd), references.bib, rendered PDF
```

`1. Data/`, `3. Results/`, and `0. Documents/Literature/` are excluded from git
(see `.gitignore`) because of size and licensing. The raw data must be placed in
the folders above before running — see Section 6 for sources.

---

## 4. How to reproduce (run order)

Run the scripts in `2. Coding/` **in numerical order**. Each preprocessing
script writes a panel that later scripts read, so the order matters. Scripts
`00_*` are not run directly — they are sourced automatically.

| Step | Script | Purpose | Key output |
|------|--------|---------|------------|
| — | `00_setup.R` | Defines paths; sources `00_functions.R` | (sourced) |
| — | `00_functions.R` | Shared preprocessing + exposure + plotting helpers | (sourced) |
| 1 | `01_preprocess_population.R` | Build municipal population panel 2010–2024 | `Population/poblacion_municipal_2010_2024.rds` |
| 2 | `02_preprocess_violence.R` | Tidy SIEVCAC conflict-violence data | `SIEVCAC/violence_panel2010_2024.rds` |
| 3 | `03_preprocess_education.R` | Tidy Saber 11 + immediate higher-ed transition | `Saber11/saber11_panel2010_2023.rds` |
| 4 | `04_preprocess_municipality.R` | Join municipal controls (TerriData, coca, IPC) | `Municipality/municipal_panel2010_2023.rds` |
| 5 | `05_final_panel.R` | Join violence + education + controls | `Final/final_panel_2010_2023.rds` |
| 6 | `06_treatment_identification.R` | Define FARC exposure & treatment groups | `Final/exposure_crosssection.rds`, `Final/final_panel_2010_2023_treatment.rds` |
| 7 | `07_did_specification.R` | Parallel-trends checks, event studies, HonestDiD | event-study figures + `robustness_*` inputs |
| 8 | `08_map_treatment.R` | Choropleth maps of treated vs. control | `map_treated_control.png`, `map_intensity_three_arm.png` |
| 9 | `09_violence_reduction.R` | First-stage: violence decline by exposure | `violence_reduction_table.rds`, `violence_reduction_trend.png` |
| 10 | `10_heterogeneous_sex.R` | Heterogeneous effects by student sex | `het_sex_*.csv`, `event_sex.png` |
| 10.1 | `10.1_sex_decomposition_honestdid.R` | Appendix: decomposition + HonestDiD by sex | `het_sex_decomp*.csv`, `het_sex_honestdid.csv` |
| 11 | `11_robusteness.R` | Robustness: thresholds, comparison groups, pre-trends | `robustness_*.rds/.csv`, `robustness_forest.png` |
| 12 | `12_mechanisms.R` | Mechanisms: fiscal, coca, population, composition | `mechanism_*.csv/.rds`, `event_*.png` |

Steps 1–6 must run in sequence. Steps 7–12 all depend on step 6 but are
independent of each other and can be run in any order after it.

To run non-interactively from the `2. Coding/` directory:

```bash
for f in 01 02 03 04 05 06 07 08 09 10 10.1 11 12; do
  Rscript "$(ls ${f}_*.R 2>/dev/null || echo ${f%.*}*.R)"
done
```

(Or simply open each script in RStudio and source it in order.)

## 5. Key methodological choices (for reference)

- **Treatment event:** permanent FARC ceasefire (20 Dec 2014). Exam-year 2014 is
  the first treated outcome period, because the Saber 11 higher-education
  transition is measured in year *t+1*.
- **Exposure window:** 2010–2014 (strictly pre-ceasefire). Exposure is measured
  as total FARC-attributed attacks in the window per 10,000 mean inhabitants.
- **Treatment definitions** (built in `06`): (A) binary presence (any attack),
  (B) high vs. low intensity split at the median of exposed municipalities, and
  (C) continuous `log1p(attacks per capita)`. The main contrast is heavy
  exposure (high) vs. unexposed.
- **Sample:** 1,102 municipalities; the 13 largest cities are dropped in the
  main specification (re-included in an appendix robustness check) and municipalities with zero exam-takers previous to the ceasefire are dropped too.
- **Attribution:** FARC set = `FARC`, `FARC/ELN`, `FARC/EPL`; unidentified
  perpetrators tracked separately (see `00_functions.R`).

---

## 6. Data sources

All raw data is public. Because of size and redistribution terms it is not
committed to the repo; download and place each set in the folder shown.

| Folder | Dataset | Source |
|--------|---------|--------|
| `1. Data/SIEVCAC/` | Conflict-violence cases & victims by type; DIVIPOLA municipality codes | CNMH — Observatorio de Memoria y Conflicto (SIEVCAC) https://micrositios.centrodememoriahistorica.gov.co/observatorio/sievcac/ |
| `1. Data/Saber11/` | Saber 11 exam microdata 2010–2023; SNIES higher-ed enrolment key | ICFES / MEN (SNIES) https://www.icfes.gov.co/investigaciones/data-icfes/ |
| `1. Data/Municipality/` | TerriData fiscal, economic, education, poverty dimensions; coca crops; IPC; municipal boundaries (MGN geojson) | DNP TerriData; UNODC/SIMCI https://terridata.dnp.gov.co/index-app.html#/descargas |
| `1. Data/Population/` | Municipal population projections 2005–2042 | DANE https://www.dane.gov.co/index.php/estadisticas-por-tema-2/demografia-y-poblacion/series-de-poblacion |

Result tables and figures in `1. Data/Final/` and `3. Results/` are regenerated
by the scripts, so they do not need to be downloaded.
---

## 7. Reproducibility notes

- The pipeline is deterministic. No random seeds are required.
- Figures use a shared colorblind-safe palette and theme (`farc_palette`,
  `theme_capstone()` in `00_functions.R`) so every figure is styled
  consistently, including those built inside the dissertation `.qmd`.
