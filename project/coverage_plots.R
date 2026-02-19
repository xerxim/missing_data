# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse,cowplot,gridExtra)
## Eigener Code:
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
sys.source("project/src/mc_helpers.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")

#data loading
load("project/dta/main_data.RData")


#festes arg
row_labels <- c("MCAR",
"MCAR\nX1: 10% missing,\nX2: 50% missing",
"MAR",
"MAR\nX1: 10% missing,\nX2: 50% missing"
)

##### i) Linear Data
plot_names <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(mu[3])
)

i1a <- full_output$`1a`
i1b <- full_output$`1b`
i1c <- full_output$`1c`
i1d <- full_output$`1d`


mc$make_coverages_plot(c("i1c","i1d","i1a","i1b"), plot_names, row_labels = c("MCAR", "MCAR, CE", "MAR", "MAR, CE"))
##### ii) INTERACTION DATA

#MC Simulation Result import
load("project/raw_mc_results/mc_raw_results_s=300_interaction_effects.RData")

#
iia <- full_output$`2a`
iib <- full_output$`2b`
iic <- full_output$`2c`
iid <- full_output$`2d`

### Plotting

plot_names <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(beta[3]),
  expression(mu[3])
)


mc$make_coverages_plot(c("iic", "iid","iia", "iib" ),
  plot_names, row_labels = c("MCAR", "MCAR, CE", "MAR", "MAR, CE"))

