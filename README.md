# Ground Combat Systems (GCS) Analysis

This repository contains an R/Quarto workflow for exploring FY16-FY20 federal contract data for three ground combat system programs: Abrams, Bradley, and Stryker. The analysis focuses on spending trends, vendors, contracting agencies, key technologies, and practical data-cleaning approaches such as vendor name normalization and USASpending API enrichment.

## Repository contents

- [gcs_analysis.qmd](gcs_analysis.qmd) – main Quarto analysis document
- [functions/clean.R](functions/clean.R) – helper functions for cleaning contract identifiers and querying the USASpending API
- [AnalyticsExercises.Rproj](AnalyticsExercises.Rproj) – RStudio project file
- [figures/](figures) – generated charts (created when the analysis is rendered)

## Requirements

- R 4.x or newer
- Quarto
- R packages: dplyr, openxlsx2, purrr, stringr, stringdist, tidyr, ggplot2, scales, readxl, httr2, and jsonlite

## Getting started

1. Open [AnalyticsExercises.Rproj](AnalyticsExercises.Rproj) in RStudio.
2. Install the required packages if needed.
3. Place the source Excel workbook in the location referenced by the analysis, or update the `dir_path`/`data_location` values in [gcs_analysis.qmd](gcs_analysis.qmd).
4. Render the document with:

   ```bash
   quarto render gcs_analysis.qmd
   ```

## Notes

- The workflow uses string-based normalization and USASpending transaction lookups to enrich contract records.
- The analysis expects an input workbook named `ground_vehicles.xlsx` and writes generated figures to the [figures/](figures) folder.
- This project is intended as an exploratory analytics exercise rather than a production-ready application.