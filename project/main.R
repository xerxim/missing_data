# Place for main analysis code.
# Imports:
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
helpers <- new.env()
sys.source("project/src/mc_helpers.R", envir = helpers)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.
dta_path <- "project/dta/"
dta_single_path <- "project/dta/single/"
# Activate multithreading.
future::plan(future::multisession, workers = future::availableCores()) 
# Activate progress bar.
progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_cli(
  format = "{cli::pb_bar} {cli::pb_percent} | ETA {cli::pb_eta}"
))

# Set true if data is already generated.
DATA_GENERATED <- TRUE

if(!DATA_GENERATED){
  # Start timing.
  start <- Sys.time()

  # Analysis:
  # Constant parameters.
  N <- 500
  CYCLES <- 500
  M <- 30
  METHODS <- c("cart", "pmm", "cart_boot")
  SEED <- 161
  # Define output list.
  full_output <- vector("list", length = 8)

  #########################################################
  # i) Linear data.
  # Constant parameters.
  data_generator <- helpers$generate_data
  formula <- "X3 ~ X1 + X2"
  true_vals <- c("(Intercept)"=5, "X1"=0.6, "X2"=0.5)
  true_means <- c("X3" = 12.8)

  #--------------------------------------------------------
  ## a) MAR: Missings only in X3, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X3")
  miss <- c("MAR")
  miss_aux <- c("X1")
  ## Varying parameters.
  miss_rates <- list(
    c(0.1), # 10% in X3.
    c(0.2), # 20% in X3.
    c(0.3), # 30% in X3.
    c(0.4), # 40% in X3.
    c(0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 1a:")
  output_1a <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_1a) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_1a, file = glue::glue("{dta_single_path}/1a_data.RData"))
  # Fill output in full list.
  full_output[[1]] <- output_1a

  #--------------------------------------------------------
  ## b) MAR: Missings in all vars, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X1", "X2", "X3")
  miss <- c("MCAR", "MCAR", "MAR")
  miss_aux <- c(NULL, NULL, "X1")
  ## Varying parameters.
  miss_rates <- list(
    c(0.2, 0.5, 0.1), # 10% in X3.
    c(0.2, 0.5, 0.2), # 20% in X3.
    c(0.2, 0.5, 0.3), # 30% in X3.
    c(0.2, 0.5, 0.4), # 40% in X3.
    c(0.2, 0.5, 0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 1b:")
  output_1b <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_1b) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_1b, file = glue::glue("{dta_single_path}/1b_data.RData"))
  # Fill output in full list.
  full_output[[2]] <- output_1b

  #--------------------------------------------------------
  ## c) MCAR: Missings only in X3, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X3")
  miss <- c("MCAR")
  miss_aux <- NULL
  ## Varying parameters.
  miss_rates <- list(
    c(0.1), # 10% in X3.
    c(0.2), # 20% in X3.
    c(0.3), # 30% in X3.
    c(0.4), # 40% in X3.
    c(0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 1c:")
  output_1c <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_1c) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_1c, file = glue::glue("{dta_single_path}/1c_data.RData"))
  # Fill output in full list.
  full_output[[3]] <- output_1c

  #--------------------------------------------------------
  ## d) MCAR: Missings in all vars, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X1", "X2", "X3")
  miss <- c("MCAR", "MCAR", "MCAR")
  miss_aux <- NULL
  ## Varying parameters.
  miss_rates <- list(
    c(0.2, 0.5, 0.1), # 10% in X3.
    c(0.2, 0.5, 0.2), # 20% in X3.
    c(0.2, 0.5, 0.3), # 30% in X3.
    c(0.2, 0.5, 0.4), # 40% in X3.
    c(0.2, 0.5, 0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 1d:")
  output_1d <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_1d) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_1d, file = glue::glue("{dta_single_path}/1d_data.RData"))
  # Fill output in full list.
  full_output[[4]] <- output_1d

  #########################################################
  # ii) Data with interaction effect.
  # Constant parameters.
  data_generator <- helpers$generate_data_nonlinear_moderate
  formula <- "X3 ~ X1 + X2 + X1*X2"
  true_vals <- c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.25)
  true_means <- c("X3" = 3.875)

  #--------------------------------------------------------
  ## a) MAR: Missings only in X3, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X3")
  miss <- c("MAR")
  miss_aux <- c("X1")
  ## Varying parameters.
  miss_rates <- list(
    c(0.1), # 10% in X3.
    c(0.2), # 20% in X3.
    c(0.3), # 30% in X3.
    c(0.4), # 40% in X3.
    c(0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 2a:")
  output_2a <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_2a) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_2a, file = glue::glue("{dta_single_path}/2a_data.RData"))
  # Fill output in full list.
  full_output[[5]] <- output_2a

  #--------------------------------------------------------
  ## b) MAR: Missings in all vars, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X1", "X2", "X3")
  miss <- c("MCAR", "MCAR", "MAR")
  miss_aux <- c(NULL, NULL, "X1")
  ## Varying parameters.
  miss_rates <- list(
    c(0.2, 0.5, 0.1), # 10% in X3.
    c(0.2, 0.5, 0.2), # 20% in X3.
    c(0.2, 0.5, 0.3), # 30% in X3.
    c(0.2, 0.5, 0.4), # 40% in X3.
    c(0.2, 0.5, 0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 2b:")
  output_2b <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_2b) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_2b, file = glue::glue("{dta_single_path}/2b_data.RData"))
  # Fill output in full list.
  full_output[[6]] <- output_2b

  #--------------------------------------------------------
  ## c) MCAR: Missings only in X3, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X3")
  miss <- c("MCAR")
  miss_aux <- NULL
  ## Varying parameters.
  miss_rates <- list(
    c(0.1), # 10% in X3.
    c(0.2), # 20% in X3.
    c(0.3), # 30% in X3.
    c(0.4), # 40% in X3.
    c(0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 2c:")
  output_2c <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_2c) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_2c, file = glue::glue("{dta_single_path}/2c_data.RData"))
  # Fill output in full list.
  full_output[[7]] <- output_2c

  #--------------------------------------------------------
  ## d) MCAR: Missings in all vars, varying missing %.
  ## Constant parameters.
  miss_vars <- c("X1", "X2", "X3")
  miss <- c("MCAR", "MCAR", "MCAR")
  miss_aux <- NULL
  ## Varying parameters.
  miss_rates <- list(
    c(0.2, 0.5, 0.1), # 10% in X3.
    c(0.2, 0.5, 0.2), # 20% in X3.
    c(0.2, 0.5, 0.3), # 30% in X3.
    c(0.2, 0.5, 0.4), # 40% in X3.
    c(0.2, 0.5, 0.5)  # 50% in X3.
  )
  # Generate data.
  print("----------------------------------------------------------")
  print("Running 2d:")
  output_2d <- purrr::map(miss_rates, function(miss_rate){
    mc$mc_study_furrr(
      n = N, cycles = CYCLES, m = M, methods = METHODS, seed = SEED, 
      data_generator = data_generator, formula = formula, true_vals = true_vals,
      miss_vars = miss_vars, true_means = true_means, miss = miss, miss_aux = miss_aux,
      miss_rates = miss_rate
    )
  })
  # Name output list.
  names(output_2d) <- purrr::map(miss_rates, \(rate)glue::glue("miss_rate={rate}"))
  # Save output.
  save(output_2d, file = glue::glue("{dta_single_path}/2d_data.RData"))
  # Fill output in full list.
  full_output[[8]] <- output_2d

  # Name full list.
  n <- c(
    "1a", "1b", "1c", "1d",
    "2a", "2b", "2c", "2d"
  )
  names(full_output) <- n
  # Save.
  save(full_output, file = glue::glue("{dta_path}/main_data.RData"))

  # Stop timing.
  end <- Sys.time()
  timed <- end - start
  print("Finished all Analysis!")
  print(glue::glue("Duration: {timed}"))
} else {
  # Load data if already generated.
  load(glue::glue("{dta_path}/main_data.RData"))
}
