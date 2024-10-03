library(Rcpp)
library(lolog)
library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(statnet)
library(network)

# Load the LologExtension package
library(LologExtension)

# sample data
data(lazega)
net <- as.network(lazega)
plot(net)

# Fit multiple models
modtriad <- lolog(net ~ edges + minTriads(1))  # 0.6192
summary (modtriad) # 30 obs stat                  
mod2 <- lolog(net ~ edges + minTriads(1))
summary(mod2)
modtriad_degree <- lolog(net ~ edges + minTriads(1) + degree(2))  # Model with minTriads and degree(2)
summary (modtriad_degree) # 30 obs stat for minTriads and 2 for deg (2) 
modminDeg <- lolog(net~edges + minDegree(1)) # 
summary(modminDeg) #34 minDegree nodes
modtriangle <- lolog (net~edges + triangles ())
summary (modtriangle)
modgwesp <- lolog(net~edges + gwesp (2.0), verbose = 1)
summary(modgwesp)

# Generate goodness-of-fit data for each model
gof_modtriad <- gofit(modtriad, formula = net ~ edges + minTriads(1), nsim = 100)
gof_modtriad_degree <- gofit(modtriad_degree, formula = net ~ edges + minTriads(1) + degree(2), nsim = 100)
gof_modminDeg <- gofit(modminDeg, formula = net~edges + minDegree(1), nsim = 100)
gof_modtriangle <- gofit(modtriangle, formula = net~edges + triangles(), nsim = 100)
gof_modgwesp <- gofit(modgwesp, formula = net~edges + gwesp(2.0), nsim = 100)

# We shall plot the gof firts and then compare all the three
library(ggplot2)

# Extract observed and mean values from gofit results
extract_gof_data <- function(gof_result, model_name) {
  observed <- gof_result$summary$obs
  mean_vals <- gof_result$summary$mean
  degree_vals <- 0:(length(observed) - 1)
  
  data.frame(
    Degree = rep(degree_vals, 2),
    Value = c(observed, mean_vals),
    Type = rep(c("Observed", "Mean"), each = length(degree_vals)),
    Model = model_name
  )
}

# Extract data for each model
plot_data_modtriad <- extract_gof_data(gof_modtriad, "modtriad")
plot_data_modtriad_degree <- extract_gof_data(gof_modtriad_degree, "modtriad_degree")
plot_data_modminDeg <- extract_gof_data(gof_modminDeg, "modminDeg")
plot_data_modtriangle <-extract_gof_data(gof_modtriangle, "modtriangle")
plot_data_modgwesp <- extract_gof_data(gof_modgwesp, "modgwesp")
# Combine all data
plot_data <- rbind(plot_data_modtriad, plot_data_modtriad_degree, plot_data_modminDeg, plot_data_modtriangle, plot_data_modgwesp)

# Plot goodness-of-fit for all models
ggplot(plot_data, aes(x = Degree, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Goodness-of-Fit Comparison Across Models",
    x = "Degree",
    y = "Node Count",
    color = "Type",
    linetype = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )


# Compute the AIC/BIC 
# Function to compute AIC and BIC
compute_aic_bic <- function(gof_result, num_parameters) {
  # Extract observed stats
  observed <- gof_result$ostats
  
  # Compute log-likelihood using the observed and simulated statistics
  # Assuming a normal approximation for the likelihood
  mean_vals <- apply(gof_result$stats, 2, mean)
  variance_vals <- apply(gof_result$stats, 2, var)
  
  # Log-likelihood (normal approximation)
  log_likelihood <- -0.5 * sum((observed - mean_vals)^2 / variance_vals)
  
  # AIC and BIC calculations
  AIC <- 2 * num_parameters - 2 * log_likelihood
  BIC <- num_parameters * log(length(observed)) - 2 * log_likelihood
  
  return(c(AIC = AIC, BIC = BIC))
}
# Number of parameters for each model
num_params_modtriad <- 2  
num_params_modtriad_degree <- 3  
num_params_modminDeg <- 2 
num_params_modtriangle <- 2
num_params_modgwesp <-2

# Compute AIC and BIC for each model
aic_bic_modtriad <- compute_aic_bic(gof_modtriad, num_params_modtriad)
aic_bic_modtriad_degree <- compute_aic_bic(gof_modtriad_degree, num_params_modtriad_degree)
aic_bic_modminDeg <- compute_aic_bic(gof_modminDeg, num_params_modminDeg)
aic_bic_modtriangle <- compute_aic_bic(gof_modtriangle, num_params_modtriangle)
aic_bic_mod_gwesp <- compute_aic_bic(gof_modgwesp, num_params_modgwesp)

# Create a data frame for plotting AIC and BIC
aic_bic_data <- data.frame(
  Model = c("modtriad", "modtriad_degree", "modminDeg", "modtriangle", "modgwesp"),
  AIC = c(aic_bic_modtriad["AIC"], aic_bic_modtriad_degree["AIC"], aic_bic_modminDeg["AIC"], aic_bic_modtriangle["AIC"], aic_bic_mod_gwesp["AIC"]),
  BIC = c(aic_bic_modtriad["BIC"], aic_bic_modtriad_degree["BIC"], aic_bic_modminDeg["BIC"], aic_bic_modtriangle["BIC"], aic_bic_mod_gwesp["BIC"])
)

# Plot AIC and BIC for different models
ggplot(aic_bic_data, aes(x = Model)) +
  geom_bar(aes(y = AIC, fill = "AIC"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = BIC, fill = "BIC"), stat = "identity", position = "dodge") +
  labs(
    title = "AIC and BIC Comparison Across Models",
    x = "Model",
    y = "Value",
    fill = "Criterion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
