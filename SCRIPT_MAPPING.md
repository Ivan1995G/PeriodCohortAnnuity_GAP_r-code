# Original-to-Repository Script Mapping

This control table maps the principal blocks of
`mortality_ensemble_eiopa_clean_full.R` to the modular single-population
pipeline. Line numbers refer to the original working script supplied with
manuscript AAS-2026-0050.

| Original block | New file or function | Status | Reason | Paper connection |
|---|---|---|---|---|
| Lines 16-50: packages | `main.R`, stage 1 | Included | Loads the packages used by the original models and the local EIOPA reader. | Entire analysis |
| Lines 55-82: user and liability parameters | `main.R` population selector; `R/config.R` | Included | Preserves country, years, ages, retirement age, five-model selection, benefit, and 3% rate. | Methods; Tables 1-8; Figures 1-9 |
| Lines 85-113: Camarda dependencies | `main.R`; `R/vendor/camarda_cpspline_functions.R` | Included in required part | Supplies the CP-spline functions used by the CPSPLINE candidate model. | Mortality-model comparison |
| Lines 115-160: HMD data | `R/data.R` | Included | Same HMD country, population series, age checks, and year checks. | Data section; Tables 1-3; Figures 1-5 |
| Lines 162-214: model specifications | `R/mortality_models.R` | Included | Preserves LC, APC, RH, CBD, M6, PLAT, HU, and CPSPLINE specifications and tuning parameters. | Methods; Table 1 |
| Lines 216-348: basic utilities | `R/functions_auxiliary.R` | Included | Matrix cleaning, interpolation, observed surfaces, panel conversion, and mortality-surface combination. | All mortality and liability results |
| Lines 349-616: model fit and forecast dispatch | `R/mortality_models.R` | Included | Preserves StMoMo, HU, and Camarda fit/forecast code. | Tables 1, 3, 5-8; Figures 1, 3-5, 7-9 |
| Lines 617-817: Shapley, performance, equal weights, rolling forecast | `R/functions_ensemble.R` | Included | Preserves coalition values, weights, loss, and rolling one-step design. | Table 2; Figure 2 |
| Lines 818-960: validation and five-model selection | `R/pipeline_single_population.R` | Included | Same validation years, MSE ranking, and selected-model criterion. | Table 1; Figure 1 |
| Lines 961-1045: ensemble weights | `R/pipeline_single_population.R` | Included | Same Shapley, performance, and equal weights on the mortality scale. | Table 2; Figure 2 |
| Lines 1046-1345: independent test | `R/pipeline_single_population.R`; `R/graphs.R` | Included in paper-relevant part | Same fixed validation weights, test period, accuracy measures, and reported plots. | Table 3; Figures 3-5 |
| Lines 1156-1199: pairwise ensemble tests | None | Excluded | Not reported in the manuscript and not used by subsequent paper calculations. | None |
| Lines 1347-1561: EIOPA curves | `R/functions_financial.R`; `R/financial_pipeline.R` | Included | Uses the exact 31 December 2019 workbook while preserving curve definitions and annual discounting. | Table 4; Figure 6 |
| Lines 1563-1630: period/cohort annuity functions | `R/functions_financial.R` | Included in paper-relevant part | Preserves survival and present-value formulas used for the annuity gap. | Equations and Tables 5-8 |
| Lines 1631-1680: maturity decomposition helper | None | Excluded | Not reported and not required by any published table or figure. | None |
| Lines 1683-1747: final refit and mortality projection | `R/financial_pipeline.R` | Included | Extends selected models far enough to follow cohorts from age 67 to age 100. | Tables 5-8; Figures 7-9 |
| Lines 1748-1884: liability calculations and paper tables | `R/financial_pipeline.R` | Included in paper-relevant part | Produces period/cohort liabilities, absolute and relative gaps, correction factors, and EIOPA stress results. | Tables 5-7 |
| Lines 78-82 plus paper flat-rate comparison | `R/config.R`; `R/functions_financial.R`; `R/financial_pipeline.R` | Included | Applies the original 3% parameter to the comparison explicitly reported in the proof. | Table 8 |
| Lines 1885-1981: maturity-bucket table | None | Excluded | Not present in the submitted proof. | None |
| Lines 1984-2147: financial plots | `R/graphs.R` | Included in paper-relevant part | Retains EIOPA curves, liability levels, model gaps, and stress gaps only. | Figures 6-9 |
| Duration, PV01, and their plots | None | Excluded | Not present in the submitted proof and not inputs to reported results. | None |
| Lines 2150-2240: result object and save | `R/outputs.R` | Included in paper-relevant part | Saves the common result object, eight numerical tables, audit files, and proof-value checks. | Reproducibility of Tables 1-8 |

