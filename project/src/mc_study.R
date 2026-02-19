# Imports:
## Self written code.
helpers <- new.env()
sys.source("project/src/mc_helpers.R", envir = helpers)

#' Compare different mice methods.
#' 
#' @description This function compares different mice methods using a
#' multithreaded mc study. It generates data and missing values on its own.
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
#' @return A dataframe containing pooled descriptive values for regression parameters and means.
#' }#' @export
mc_study_furrr <- function(
  methods, m, formula, true_vals, true_means,
  n, cycles, data_generator = helpers$generate_data,
  miss_vars, miss, miss_rates, miss_aux = NULL, 
  seed = NA
){
  print("Starting MC Study...")
  # Furr options.
  opts <- furrr::furrr_options(
    seed = TRUE, # Seeding.
    globals = c("mice.impute.cart_boot") # Give our boot function.
  )
  # Save names.
  names_vals <- names(true_vals)
  names_means <- names(true_means)
  # Ensure local copies of variables for workers.
  names_vals     <- freeze(names_vals, as.character)
  names_means    <- freeze(names_means, as.character)
  methods        <- freeze(methods, as.character)
  m              <- freeze(m, as.numeric)
  formula        <- freeze(formula, as.character)
  true_vals      <- freeze(true_vals, as.numeric)
  true_means     <- freeze(true_means, as.numeric)
  n              <- freeze(n, as.numeric)
  cycles         <- freeze(cycles, as.numeric)
  data_generator <- if (is.null(data_generator)) NULL else as.function(data_generator)
  miss_vars      <- freeze(miss_vars, as.character)
  miss           <- freeze(miss, as.character)
  miss_rates     <- freeze(miss_rates, as.numeric)
  miss_aux       <- freeze(miss_aux, as.character)
  seed           <- freeze(seed, as.numeric)
  # Set names again.
  names(true_vals) <- names_vals 
  names(true_means) <- names_means

  # Use progress bar.
  results_list <- progressr::with_progress({
    p <- progressr::progressor(steps = cycles)

    # Define one cycle.
    one_cycle <- function(cy) {
      # Increase seed.
      cy_seed <- if (!is.na(seed)) seed + cy else NA
      # Generate data.
      data <- data_generator(n = n, seed = cy_seed)
      # Create NAs.
      data_w_na <- helpers$make_missing(
        data, vars = miss_vars,
        methods = miss, rates = miss_rates,
        aux = miss_aux, seed = cy_seed
      )
      # Create output for every method.
      out <- lapply(methods, function(method){
        # Impute with mice using method.
        imp <- mice::mice(
          data_w_na, m = m, method = method,
          printFlag = FALSE, seed = cy_seed
        )
        # Fit means and extract relevant values.
        means_df <- fit_means(
          imp = imp,
          method = method, cycle = cy, true_means = true_means
        )
        # Fit formula and extract relevant values.
        lm_df <- fit_lm(
          imp = imp, formula = formula,
          method = method, cycle = cy,
          true_vals = true_vals
        )

        list(means_df, lm_df)
      })
      # Increse bar.
      p(sprintf("cycle %d", cy))
      # Flatten 2d list to 1d and return.
      unlist(out, recursive = FALSE)
    }
    # Map over cycles, multithreaded.
    results_list <- furrr::future_map(1:cycles, one_cycle, .options = opts)
  })

  # Flatten 2d list to 1d.
  results <- unlist(results_list, recursive = FALSE)
  # Combine to Dataframe.
  results_df <- do.call(rbind, results)

  # Return.
  print("Finished MC Study!")
  results_df
}

#' Fit a linear model on imputations.
#' 
#' @description This function fits a linear model on imputation values and 
#'              calculates the following pooled results for every coeficient:
#'              estimate, std error, CI, bias, relative bias, cover (single coverage)
#'              
#' 
#' @param imp mids(1). A mice mids object with the imptations to be fitted. 
#' @param formula character(1). LM formula.
#' @param method character(1). A string of the used mice method.
#' @param cycle integer(1). A integer that notes the currently running cycle.
#' @param true_vals named numeric. Vector containing true parameter values for all lm coefs.
#'
#' @return A dataframe containing pooled descriptive values for regression parameters.
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
  rel_bias = bias / true
  cover <- (ci_low <= true) & (ci_high >= true)

  # Change term names.
  terms <- helpers$rename_coef_levels(s$term)

  # Fill results.
  lm_df <- data.frame(
    cycle     = cycle,
    method    = method,
    term      = terms,
    est       = s$estimate,
    se        = s$std.error,
    ci_l      = ci_low,
    ci_u      = ci_high,
    bias      = bias,
    rel_bias  = rel_bias,
    cover     = cover,
    row.names = NULL
  )

  lm_df
}

#' Calculate the means for imputation
#' 
#' @description This function calculates the pooled mean for given variables on 
#'              imputation values and calculates the following pooled results
#'              for every mean:
#'              estimate, std error, CI, bias, relative bias, cover (single coverage)
#'              For this it uses a linear model with fromula variable ~ 1.
#'              
#' 
#' @param imp mids(1). A mice mids object with the imptations to be fitted. 
#' @param method character(1). A string of the used mice method.
#' @param cycle integer(1). A integer that notes the currently running cycle.
#' @param true_means named numeric. Vector containing true mean(s) values for all 
#'                                  variables that mean is needed for.
#'
#' @return A dataframe containing pooled descriptive values for means.
fit_means <- function(
  imp, method, cycle, true_means
){
  # Create sub df.
  means_df <- data.frame(
    cycle    = integer(0),
    method   = character(0),
    est      = numeric(0),
    se       = numeric(0),
    ci_l     = numeric(0),
    ci_u     = numeric(0),
    bias     = numeric(0),
    rel_bias = numeric(0),
    cover    = logical(0),
    row.names = NULL
  )
  # For every imputed variable.
  for(v in names(true_means)) {
    # Create args for special fit_lm.
    formula <- paste(v, "~ 1")
    true_vals <- c(
      "(Intercept)" = true_means[[v]]
    )
    # Fit.
    fit <- fit_lm(
      imp = imp, formula = formula,
      method = method, cycle = cycle, 
      true_vals = true_vals
    )
    # Rename term.
    fit$term <- glue::glue("mu({v})")
    # Add to df.
    means_df <- rbind(
      means_df,
      fit
    )
  }

  means_df
}

#' Unname not NULL variables and apply function. Weird magic.
#' 
#' @description This helper functions helps variables used in furr mapping to be
#'              evaluated. Its ugly but i tried a lot and nothing else would work :(
#'              
#' 
#' @param x any. Variable unname and given function is used on. 
#' @param f function(1). Function to be applied before unname. Mostly as.(datatype).
#'
#' @return The unnamed variable with given function applied before.
freeze <- function(x, f) {
  if (is.null(x)) return(NULL)
  unname(f(x))
}