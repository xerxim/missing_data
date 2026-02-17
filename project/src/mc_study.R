# Imports:
## Self written code.
helpers <- new.env()
sys.source("project/src/mc_helpers.R", envir = helpers)

# Code:
mc_study <- function(
  methods, m, formula, true_vals,
  n, cycles, 
  miss_vars, miss, miss_rates, miss_aux = NULL, 
  seed = NULL
){
  #' Compare different mice methods.
  #' 
  #' @description This function compares different mice methods using a
  #' mc study. It generates data and missisng values on its own.
  #' 
  #' @param methods character. Method(s) to compare.
  #' @param m integer. Number of imputation every mice call.
  #' @param formula character(1). LM formula for imputed and pooled data.
  #' @param true_vals named numeric. Vector with true parameters of linear relationship.
  #' @param n integer(1). Size of dataset.
  #' @param cycles integer(1). Number of mc cycles.
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
  #' @return Summary for every cycle in one dataframe.
  
  # Check args.

  # Initalize variables.
  results <- list()
  k <- 1L
  # Initiate progress bar.
  print("Starting MC Study please wait...")
  pb <- txtProgressBar(min = 0, max = cycles * length(methods), style = 3)
  # Iterate over cycles.
  for(c in (1:cycles)){
    # Generate Data.
    data <- helpers$generate_data(n = n)
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
        printFlag = FALSE
      )
      # Fit.
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
      results[[k]] <- data.frame(
        cycle   = c,
        method = method,
        term   = s$term,
        est    = s$estimate,
        se     = s$std.error,
        ci_l   = ci_low,
        ci_u   = ci_high,
        bias   = bias,
        cover  = cover,
        row.names = NULL
      )
      k <- k + 1L
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

t <- mc_study(
  c("cart", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=8, "X2"=6, "X3"=12.8), 
  500, 10, c("X1", "X2", "X3"), 
  "MCAR", c(0.2, 0.5, 0.3), NULL, NULL
)
t


