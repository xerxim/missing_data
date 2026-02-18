# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse, mice,cowplot,gridExtra)
## Eigener Code:
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
sys.source("project/src/mc_helpers.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")



##### ii) INTERACTION DATA

#MC Simulation Result import
load("project/raw_mc_results/mc_raw_results_s=300_interaction_effects.RData")

#
iia <- dfs_mar_X3_miss_nonlin
iib <- dfs_mcar_all_miss_nonlin
iic <- dfs_mar_X3_miss_nonlin
iid <- dfs_mar_all_miss_nonlin

### Plotting
miss_perc <- c(10, 20, 30, 40, 50)

plot_names <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(beta[3]),
  expression(mu[3])
)

row_labels <- c("MCAR",
"MCAR\nX1: 10% missing,\nX2: 50% missing",
"MAR",
"MAR\nX1: 10% missing,\nX2: 50% missing"
)

mc$make_coverages_plot(c("iia", "iib", "iic", "iid"), 
  miss_perc, plot_names, row_labels)
