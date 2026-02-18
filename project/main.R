# Place for main analysis code.
# Imports:
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.
dta_path <- "project/dta/"
# Activate multithreading.
future::plan(future::multisession, workers = future::availableCores()) 
# Activate progress bar.
progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_cli(
  format = "{cli::pb_bar} {cli::pb_percent} | ETA {cli::pb_eta}"
))

# Analysis:



t <- mc$mc_study_furrr(
  methods = c("cart", "pmm", "cart_boot"), m = 30, formula = "X3 ~ X1 + X2",
  true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5), 
  n = 500, cycles = 10, miss_vars = c("X1", "X2", "X3"), true_means = c("X3" = 12.8),
  miss = c("MCAR", "MCAR", "MAR"), miss_rates = c(0.2, 0.3, 0.3) , miss_aux = c(NULL, NULL, "X2"), seed = 161
)
t


