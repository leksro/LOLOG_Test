# LOLOG_Test
This project focuses on the analysis of social network data using the Lolog modeling framework in R. Specifically, it evaluates and compares several statistical models to understand the underlying structure of the Sample Lazega dataset, which captures the professional relationships among attorneys in a law firm. This is just a test of the new statistic on a sample dataset.

The objectives of this analysis include:

- **Model Fitting:** Fit multiple Lolog models to the Lazega network data, including models that incorporate edges, triadic closure, and degree-based metrics.
- **Goodness-of-Fit Assessment:** Generate goodness-of-fit plots to visually assess how well each model aligns with the observed network structure.
- **Model Comparison:** Calculate Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC) values to facilitate comparison among the fitted models, aiding in the selection of the most appropriate model based on statistical performance.

Through this analysis, we aim to gain insights into the factors that influence relationships within the network and to identify the model that best captures the dynamics of the data.

## Dependencies

Make sure to install the following R packages:

```r
install.packages(c("Rcpp", "lolog", "igraph", "tidyverse", "tidygraph", "ggraph", "statnet", "network", "LologExtension"))

Run the "lolog test.r" file to see the results
