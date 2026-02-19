# Code:
#' Compare different mice methods.
#' 
#' @description This function compares different mice methods using a
#' mc study. It generates data and missing values on its own.
#' 
#' @param methods character. Method(s) to compare.
#' @param m integer. Number of imputation every mice call.
#' @param formula character(1). LM formula for imputed and pooled data.
#' @param true_vals named numeric. Vector with true parameters of linear relationship.
#' @param true_means named numeric. Vector containing true means of all variables with missings.
#' @param n integer(1). Size of dataset.
#' @param cycles integer(1). Number of mc cycles.
#' @param data_generator function. Function with args n and seed that generates dataset.
#'                                 !This is where the column names are defined!
#' @param miss_vars character. Name(s) of column(s) to generate missings for.
#' @param miss character. Vector or string with same dimensions as miss_vars. 
#'                        Missing method that should be applied for every var.
#' @param miss_rates numeric. Vector of numerics or single numeric between 0 and 1 with
#'                            same dimensions as miss_vars. Proportion of variable that
#'                            should be missing. 
#' @param miss_aux character. Only needed if one or more "miss" are set to "MAR". Vector
#'                            or string with same dimensions as miss_vars. Name of column
#'                            that influences missings for the corresponing column.
#' @param seed numeric(1). Optional seed for reproducable rng.
#'
#' @return A list containing two data frames:
#' \describe{
#'   \item{estimates}{Pooled regression results for each cycle and method.
#'   Includes parameter estimates, standard errors, confidence intervals,
#'   bias, and coverage for the model defined by `formula`.}
#'   \item{means}{Pooled mean estimates for each specified variable, cycle,
#'   and method based on intercept-only models. Includes standard errors,
#'   confidence intervals, bias, and coverage compared to `true_means`.}
#' }#' @export
mc_study <- function(
  methods, m, formula, true_vals, true_means,
  n, cycles, data_generator = helpers$generate_data,
  miss_vars, miss, miss_rates, miss_aux = NULL, 
  seed = NA
){ 
  # Check args.

  # Initalize variables.
  results <- list()
  means_results <- list()
  k <- 1L
  # Initiate progress bar.
  print("Running MC Study...")
  cli::cli_progress_bar(
    name = "MC",
    total = cycles * length(methods),
    format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta} | {cli::pb_rate}"
  )
  # Iterate over cycles.
  for(cy in (1:cycles)){
    # Generate Data.
    if(!is.na(seed)) {
      seed <- seed + cy
    }
    data <- data_generator(n = n, seed = seed)
    # Create missings.
    data_w_na <- helpers$make_missing(
      data, vars = miss_vars,
      methods = miss, rates = miss_rates,
      aux = miss_aux, seed = seed
    )
    # Imputation for all methods.
    for(method in methods) {
      # Imputation.
      imp <- mice::mice(
        data_w_na, m = m, method = method,
        printFlag = FALSE, seed = seed
      )

      # Fit means.
      means_df <- fit_means(
        imp = imp, 
        method = method, cycle = cy, true_means = true_means
      )
      results[[k]] <- means_df

      lm_df <- fit_lm(
        imp = imp, formula = formula,
        method = method, cycle = cy, 
        true_vals = true_vals
      )
      results[[k + 1]] <- lm_df
      
      k <- k + 2
      # Increase progress bar.
      cli::cli_progress_update()
    }
  }
  # Close bar.
  cli::cli_progress_done()
  print("Finished MC Study!")

  # Create df.
  results_df <- do.call(rbind, results)

  results_df
}


fit_means_old <- function(
  imp, vars, method, cycle
){
  # Create sub df.
  means_df <- data.frame(
    cycle = integer(0),
    method = character(0),
    var = character(0),
    mean = numeric(0),
    se = numeric(0),
    row.names = NULL
  )
  # For every imputed variable.
  for(v in vars) {
    # Get means.
    f_mean <- paste(v, "~ 1")
    fit_m  <- with(imp, lm(as.formula(f_mean)))
    pooled <- summary(mice::pool(fit_m))
    # Fill df.
    means_df <- rbind(
      means_df,
      data.frame(
        cycle = cycle,
        method = method,
        var = v,
        mean = pooled$estimate[1],
        se   = pooled$std.error[1],
        row.names = NULL
      )
    )
  }

  means_df
}

freeze <- function(x, f) {
  if (is.null(x)) return(NULL)
  unname(f(x))
}
