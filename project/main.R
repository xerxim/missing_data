# Place for main analysis code.
# Imports:
## Libraries.
library(ggplot2)
library(mice)
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.

# Analysis:
mc$mc_study()
# Test run.
t <- mc$mc_study(
  c("cart_boot", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=8, "X2"=6, "X3"=12.8), 
  500, 10, c("X1", "X2", "X3"), 
  "MCAR", c(0.2, 0.5, 0.3), NULL, NA
)
t