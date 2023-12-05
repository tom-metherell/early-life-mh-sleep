# Associations between early-life mental health and abnormal sleep duration in midlife: findings from a prospective cohort study in Great Britain
This repository contains the supplementary data for the paper "Associations between early-life mental health and abnormal sleep duration in midlife: findings from a prospective cohort study in Great Britain" (preprint available at \[OSF LINK\]).

## 1. Introduction
There are a number of steps to the analysis. The underlying raw data are available free of charge as follows:
* Main survey data: via the [UK Data Service](https://doi.org/10.5255/UKDA-Series-200001).
* CLOSER harmonised variables: via the [UK Data Service](https://doi.org/10.5255/UKDA-Series-2000111).
* Raw sleep diary, accelerometry data and derived sleep variables: by application to the [Centre for Longitudinal Studies](https://cls.ucl.ac.uk/data-access-training/data-access/accessing-data-directly-from-cls/).

## 2. Deriving sleep durations from accelerometry
These R markdown (`.Rmd`) scripts were rendered separately and do not use the `renv` environment present in the repository. Use `R` version 4.2.0.

## 3. Cleaning survey data, incorporating sleep estimates and performing regressions
These steps use the `targets` framework and `renv` environment present in the repository. After producing or acquiring the derived sleep estimates, you can run these steps by running `targets::tar_make()` in `R` 4.2.0 inside the repository. The `mice_report` and `models_report` Quarto files are rendered automatically as part of the `{targets}` pipeline, and the output `.html` files are also provided in the repository.

## 4. Plots
Because of technological limitations, the plots must be rendered separately after the completion of the cleaning and analysis steps by rendering the Quarto file `4_plots.Qmd`. Use `R` version 4.2.0 and the `renv` environment present in the repository. The output `.html` file and the plot `.svg` and `.png` files are also provided in the repository.

## 5. Supplementary information
The data tables listed in the supplementary information can be accessed by rendering the Quarto file `5_supplementary_info.Qmd`. Use `R` version 4.2.0 and the `renv` environment present in the repository. The output `.html` file is also provided in the repository.

## Accessing additional raw model outputs
Once you have completed step 3, you can access all raw model outputs, intermediate datasets and summaries using the `targets::tar_read()` function. For more details, see the [`{targets}` package documentation](https://books.ropensci.org/targets/).
