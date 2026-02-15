#VC MC Study helper functions
# functions for diagnostics:
calc_coverage <- function(value, CI_low, CI_upper) {
  (CI_low <= value & CI_upper >= value)
}

calc_rel_bias <- function(value, est) {
  rel_bias <- (est - value) / value
  return(rel_bias)
}


pool_mean <- function(mids, varname, conf = 0.95) {

  # Extract completed datasets
  datalist <- complete(mids, "all")

  # Point estimates and within variances
  Q <- sapply(datalist, function(d) mean(d[[varname]]))
  U <- sapply(datalist, function(d) var(d[[varname]]) / length(d[[varname]]))

  m <- length(Q)

  # Rubin's pooling
  Qbar <- mean(Q)        # pooled estimate
  Ubar <- mean(U)        # within-imputation variance
  B <- var(Q)            # between-imputation variance

  # Total variance
  Tvar <- Ubar + (1 + 1/m) * B
  se <- sqrt(Tvar)

  # ---- Added inference machinery ----

  # Relative increase in variance
  r <- ((1 + 1/m) * B) / Ubar

  # Barnard-Rubin degrees of freedom
  df <- (m - 1) * (1 + 1/r)^2

  # t critical value
  alpha <- 1 - conf
  tcrit <- qt(1 - alpha/2, df = df)

  # Confidence interval
  lower <- Qbar - tcrit * se
  upper <- Qbar + tcrit * se

  list(
    estimate = Qbar,
    se = se,
    within = Ubar,
    between = B,
    df = df,
    conf.level = conf,
    lower = lower,
    upper = upper
  )
}
