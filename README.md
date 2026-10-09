# GAMLSS Models of Child Growth: Secular Trend Analysis

## Project overview

This repository contains the R scripts used to clean, explore, model, and visualize pediatric anthropometric data. The analysis focuses on comparing BMI and height distributions in 2012 and 2025 and developing contemporary GAMLSS reference models.

The workflow is organized into sequential scripts, from initial data cleaning to final figure generation.

## Analysis workflow

| Script | Description |
|---|---|
| `List-of-functions` | functions
| `Additional-data` | data needed
| `01-data-cleaning-deduplication.R` | Identify and remove duplicate records and standardize the dataset for subsequent analyses. |
| `02-data-cleaning-removal-of-biologically-impossible-values.R` | Exclude observations with biologically impossible anthropometric values using predefined criteria. |
| `03-data-cleaning-multivariate-filter-IS.R` | Detect and remove potential multivariate outliers using the Isolation Forest (IS) algorithm. |
| `04-data-EDA.R` | Perform exploratory data analysis (EDA), including checks of sample structure, age and sex distributions, and anthropometric variables. |
| `05-GAMLSS-model-optimization.R` | Optimize GAMLSS model specifications, including the selection of distribution families and smoothing parameters. |
| `06-Sensitivity-analysis.R` | Assess the robustness of model results using alternative specifications and sensitivity analyses. |
| `07-GAMLSS-model-check.R` | Evaluate model convergence, goodness-of-fit statistics, residual diagnostics, and agreement between empirical and model-based centiles. |
| `08-secular-trends.R` | Analyze changes in anthropometric distributions and centiles over time using historical and contemporary reference models. |
| `09-final-visualixation.R` | Generate final figures for reporting and publication. |

## Methods

The analysis uses Generalized Additive Models for Location, Scale and Shape (GAMLSS) to model age-specific anthropometric distributions. Depending on the outcome and sex-specific dataset, candidate distribution families include BCCG, BCPE, and BCT.

Model assessment considers convergence, information criteria (including AIC), normalized quantile residuals, and graphical diagnostics. The final centile curves are examined across the age range.

## Reproducibility

Run the scripts in numerical order. Each script is intended to prepare data or results for the subsequent stage.

Before running the workflow:

- Ensure that the required input data are available in the expected location.
- Check that the R packages used in the scripts are installed.
- Review file paths and output directories, and update them if needed.

## Notes

- Apply exclusion criteria consistently and document any changes to the data-cleaning workflow.
- Treat observations flagged by the Isolation Forest as potential multivariate outliers; removal should follow the predefined analysis protocol.