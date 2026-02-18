
# Imports:
## Libraries.
pacman::p_load(patchwork, tidyverse, mice,cowplot,gridExtra)
## Eigener Code:
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
sys.source("project/src/mc_helpers.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")

# Working constants:
plot_path <- "project/plots/" 



##### generation für MCAR, X3 missing
dfs_mcar_X3_mis <- list()
i <- 1

for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- 5
  
  dfs_mcar_X3_mis[[i]] <- mc$mc_study(
    methods = c("cart","cart_boot", "pmm"), m = 30, 
    formula = "X3 ~ X1 +X2",
    c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
    c("X3" = 12.8), 
    n = 500, cycles =  mc_cycles, 
    miss_vars =c("X3"), 
    miss = "MCAR", miss_rates = mis_rate, miss_aux = NULL, seed = 161
  )
  
  i <- i+1
  
}


####generation für MCAR, missings in all variables

dfs_mcar_all_mis <- list()
i <- 1

for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- 5
  
  dfs_mcar_all_mis[[i]] <- mc$mc_study(
    methods = c("cart","cart_boot", "pmm"), m = 30, 
    formula = "X3 ~ X1 +X2",
    c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
    c("X3" = 12.8), 
    n = 500, cycles =  mc_cycles, 
    miss_vars =c("X1","X2","X3"), 
    miss = "MCAR", miss_rates = c(0.2,0.5,mis_rate), miss_aux = NULL, seed = 161
  )
  
  i <- i+1
  
}


##### MAR X1 missing

dfs_MAR_X1_mis <- list()
i <- 1

for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- 5
  
  dfs_MAR_X1_mis[[i]] <- mc$mc_study(
    methods = c("cart","cart_boot", "pmm"), m = 30, 
    formula = "X3 ~ X1 +X2",
    c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
    c("X3" = 12.8), 
    n = 500, cycles =  mc_cycles, 
    miss_vars ="X3", 
    miss = "MAR", miss_rates = mis_rate, miss_aux = c("X2"), seed = 161
  )
  
  i <- i+1
  
}



##### MAR all missing

dfs_MAR_all_mis <- list()
i <- 1

for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- 5
  
  dfs_MAR_all_mis[[i]] <- mc$mc_study(
    methods = c("cart","cart_boot", "pmm"), m = 30, 
    formula = "X3 ~ X1 +X2",
    c("(Intercept)"=5, "X1"=0.6, "X2"=0.5),
    c("X3" = 12.8), 
    n = 500, cycles =  mc_cycles, 
    miss_vars =c("X1","X2","X3"), 
    miss = "MAR", miss_rates = c(0.2,0.5,miss_rate), 
    miss_aux = c("X1","X2","X1"), 
    seed = 161
  )
  
  i <- i+1
  
}

