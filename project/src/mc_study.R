# Imports:
## Self written code.
helpers <- new.env()
sys.source("project/src/mc_helpers.R", envir = helpers)

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
  print("Starting MC Study please wait...")
  pb <- txtProgressBar(min = 0, max = cycles * length(methods), style = 3)
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
      setTxtProgressBar(pb, k)
    }
  }
  # Close bar.
  close(pb)
  print("Finished MC Study!")

  # Create df.
  results_df <- do.call(rbind, results)

  results_df
}

fit_lm <- function(
  imp, formula, method, cycle, true_vals
){
  # Fit lm.
  fit <- with(imp, lm(as.formula(formula)))
  res <- mice::pool(fit)
  s   <- summary(res)

  # CI.
  crit <- qt(0.975, df = s$df)
  ci_low  <- s$estimate - crit * s$std.error
  ci_high <- s$estimate + crit * s$std.error

  # (total) Bias and coverage.
  true <- true_vals[s$term]
  bias  <- s$estimate - true
  cover <- (ci_low <= true) & (ci_high >= true)

  # Change term names.
  terms <- helpers$rename_coef_levels(s$term)

  # Fill results.
  lm_df <- data.frame(
    cycle   = cycle,
    method = method,
    term   = terms,
    est    = s$estimate,
    se     = s$std.error,
    ci_l   = ci_low,
    ci_u   = ci_high,
    bias   = bias,
    cover  = cover,
    row.names = NULL
  )

  lm_df
}

fit_means <- function(
  imp, method, cycle, true_means
){
  # Create sub df.
  means_df <- data.frame(
    cycle = integer(0),
    method = character(0),
    est    = numeric(0),
    se     = numeric(0),
    ci_l   = numeric(0),
    ci_u   = numeric(0),
    bias   = numeric(0),
    cover  = logical(0),
    row.names = NULL
  )
  # For every imputed variable.
  for(v in names(true_means)) {
    # Get means.
    formula <- paste(v, "~ 1")
    true_vals <- c(
      "(Intercept)" = true_means[[v]]
    )

    fit <- fit_lm(
      imp = imp, formula = formula,
      method = method, cycle = cycle, 
      true_vals = true_vals
    )

    fit$term <- glue::glue("mu({v})")

    means_df <- rbind(
      means_df,
      fit
    )
  }

  means_df
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
