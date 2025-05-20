The Imbalance of nature: Critical Role of Species Environmental Responses for Stability

This repository contains data, analysis, and supplementary materials for the manuscript titled "The Imbalance of nature: Critical Role of Species Environmental Responses for Stability".

Contents

Additional analyses: contains code and report of analysis with different cutoff day (i.e. using only the last 20 or 40 days of the experiment) and just aim at showing that our results are robust to different cutoff length.

Code: contains code to reproduce all analyses and results presented in the main text and supplementary information. There are multiple .Rmd files in this folder:

- Analysis_30Days.Rmd: contains the same analysis presented in the main text, but using only the last 30 days of the experiment (excluding biomass peak)

- Interactions_edm.Rmd: Code to preform the EDM analysis used to estimate species interactions coefficients.

- Supplementary_info1_new_model.Rmd: provide a reproducible record of all analyses and figures in the main article.

- Supplementary_info2.Rmd: Initial analysis of the response surface experiment. Contains the code used to select community compositions based on "divergence".

- Supplementary_info3.Rmd: Analysis of the results of the Empirical Dynamic Modelling used to calculate species interactions.

- Symmetry and Magnitude.Rmd: contains an exploratory analysis looking at how magnitude and and symmentry (two dimensions of imbalance) change with species richness and distribution of species responses.

Data: Contains the datasets needed to reproduce the analysis.

Figures_ms: Contains all the figures presented in the manuscript that are generated mainly in the .Rmd file called "Supplementary_info1".

Management: Information regarding project management and author contributions.

old:_exploratory Contains deprecated or older code used in exploratory analysis.

r: Stores custom R functions used in the analysis.

Reports: folder containing the .html and .pdf files generated knitting the .Rmd documents in the "Code" folder

tables: tables generates in R (mainly in Analysis_30Days.Rmd and Supplementary_info1_new_model.Rmd) and used in the Supplementary Information




