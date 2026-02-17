# Place for main analysis code.
# Imports:
## Libraries.
library(ggplot2)
library(mice)
library(dplyr)
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.

# Analysis:

# Test run.
t <- mc$mc_study(
  methods = c("cart", "cart_boot", "pmm"), m=  30, formula = "X3 ~ X1 + X2",
  true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5), 
  n = 500, cycles = 300, miss_vars = "X3", true_means = c("X3" = 12.8),
  miss = "MCAR", miss_rates =  0.3, miss_aux = NULL, seed = 161
)
t$means