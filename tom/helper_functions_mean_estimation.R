# Helper Functions

## ---------------------
### mice.impute.cart_boot
# Einschub des Bootstraps in mice method
##-----------------------

#Only works for numreic covariates so far, working on else (Line 43 etc)

mice.impute.cart_boot <- function (y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, ...) 
{
    #install.on.demand("rpart")
    if (is.null(wy)) {
        wy <- !ry
    }
    minbucket <- max(1, minbucket)
    if (dim(x)[2] == 0) {
        x <- cbind(x, 1)
        dimnames(x) <- list(NULL, "int")
    }
    xobs <- data.frame(x[ry, , drop = FALSE])
    xmis <- data.frame(x[wy, , drop = FALSE])
    yobs <- y[ry]
    
  #BOOTSTRAPPING Einschub
  
    n_obs <- length(yobs)

    boot_id <- sample.int(n_obs, size = n_obs, replace = TRUE)

    y_boot <- yobs[boot_id]
    x_boot <- xobs[boot_id, , drop = FALSE]

    dat_boot <- data.frame(y = y_boot, x_boot)
  
  # 
  
    if (!is.factor(yobs)) {
        fit <- rpart::rpart(y_boot ~ ., data = cbind(y_boot, x_boot), 
            method = "anova", control = rpart::rpart.control(minbucket = minbucket, 
                cp = cp, ...))
        leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, 
            ])))
        fit$frame$yval <- as.numeric(row.names(fit$frame))
        nodes <- predict(object = fit, newdata = xmis)
        donor <- lapply(nodes, function(s) y_boot[leafnr == s])
        impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 
            1), numeric(1))
    }
    else {
        cat.has.all.obs <- table(yobs) == sum(ry)
        if (any(cat.has.all.obs)) {
            return(rep(levels(yobs)[cat.has.all.obs], sum(wy)))
        }
        xy <- cbind(yobs, xobs)
        xy <- droplevels(xy)
        fit <- rpart::rpart(yobs ~ ., data = xy, method = "class", 
            control = rpart::rpart.control(minbucket = minbucket, 
                cp = cp, ...))
        nodes <- predict(object = fit, newdata = xmis)
        impute <- apply(nodes, MARGIN = 1, FUN = function(s) {
            sample(colnames(nodes), size = 1, prob = s)
        })
    }
    impute
}


## ------------
# make_data()
# generiert einen synthetischen Datensatz und eine version davon mit missings
## -------

make_data <- function(n_rows,
                      n_cols,
                      miss_cols = NULL,
                      miss_prop = 0.2,
                      mechanism = c("MCAR", "MAR"),
                      seed = NULL) {

  # -------------------------------
  # 0. Input checks
  # -------------------------------
  mechanism <- match.arg(mechanism)

  if (!is.null(seed)) set.seed(seed)

  if (n_cols < 2)
    stop("Need at least 2 columns to allow MAR dependence.")

  if (is.null(miss_cols))
    miss_cols <- integer(0)

  if (any(miss_cols > n_cols))
    stop("miss_cols contains indices larger than n_cols.")

  if (length(miss_prop) == 1)
    miss_prop <- rep(miss_prop, length(miss_cols))

  if (length(miss_prop) != length(miss_cols))
    stop("miss_prop must have same length as miss_cols.")

  # -------------------------------
  # 1. Generate correlated data
  # -------------------------------
  # We simulate a multivariate normal structure using
  # a Toeplitz correlation matrix (realistic dependence).

  rho <- 0.4
  Sigma <- rho ^ abs(outer(1:n_cols, 1:n_cols, "-"))

  L <- chol(Sigma)
  Z <- matrix(rnorm(n_rows * n_cols), n_rows, n_cols)
  X <- Z %*% L

  data_full <- as.data.frame(X)
  names(data_full) <- paste0("x_", seq_len(n_cols))

  # -------------------------------
  # 2. Impose Missingness
  # -------------------------------
  data_miss <- data_full

  if (length(miss_cols) > 0) {

    for (j in seq_along(miss_cols)) {

      col_id <- miss_cols[j]
      p_miss <- miss_prop[j]

      if (mechanism == "MCAR") {
        # Missing Completely At Random:
        # Independent Bernoulli deletion.
        miss_index <- runif(n_rows) < p_miss
      }

      if (mechanism == "MAR") {
        # Missing At Random:
        # Missingness depends on another observed variable.
        # We use a logistic model depending on x_1 (fully observed driver).

        driver <- scale(data_full[[1]])

        logit_p <- qlogis(p_miss) + 0.8 * driver
        prob <- plogis(logit_p)

        miss_index <- runif(n_rows) < prob
      }

      data_miss[miss_index, col_id] <- NA
    }
  }

  # -------------------------------
  # 3. Document the DGP
  # -------------------------------
  info <- paste0(
    "DATA GENERATING PROCESS\n",
    "------------------------\n",
    "Rows: ", n_rows, "\n",
    "Columns: ", n_cols, "\n",
    "Distribution: Multivariate Normal\n",
    "Correlation: Toeplitz (rho = 0.4^|i-j|)\n\n",
    "Missingness Mechanism: ", mechanism, "\n",
    "Columns with Missingness: ",
    ifelse(length(miss_cols) == 0, "None", paste(miss_cols, collapse = ", ")), "\n",
    "Target Missing Proportions: ",
    ifelse(length(miss_prop) == 0, "None", paste(round(miss_prop, 3), collapse = ", ")), "\n"
  )

  #cat(info)

  # -------------------------------
  # 4. Return structured output
  # -------------------------------
  return(list(
    data_full = data_full,
    data_miss = data_miss,
    generation_info = info
  ))
}


#------
# pool_mean() for getting means and standard errors for variables of a mids object
#----

pool_mean <- function(mids, varname) {

  datalist <- complete(mids, "all")

  Q <- sapply(datalist, function(d) mean(d[[varname]]))
  U <- sapply(datalist, function(d) var(d[[varname]]) / length(d[[varname]]))

  m <- length(Q)

  Qbar <- mean(Q)
  Ubar <- mean(U)
  B <- var(Q)

  Tvar <- Ubar + (1 + 1/m) * B

  list(
    estimate = Qbar,
    se = sqrt(Tvar),
    within = Ubar,
    between = B
  )
}

###
# run_mc()
# nutzt cart_boot als MI method auf make_data() und summarises mit pool_mean() ua
###


run_mc <- function(S = 400, n = 2000, p = 6,
                   miss_cols = c(3,4),
                   mechanism = "MAR",
                   m_imp = 10,
                   seed = 123,
                   missing_prop = 0.2) {

  set.seed(seed)

  results <- matrix(NA, nrow = S, ncol = 6)
  colnames(results) <- c("cart_est","boot_est",
                         "cart_cover","boot_cover",
                         "cart_B","boot_B")

  start_time <- Sys.time()

  for (s in seq_len(S)) {

    # ---- Progress message every 10 iterations ----
    if (s == 1 || s %% 10 == 0 || s == S) {

      elapsed <- Sys.time() - start_time
      rate <- elapsed / s
      eta <- rate * (S - s)

      cat(sprintf(
        "\rReplication %d / %d (%.1f%%) | Elapsed: %s | ETA: %s",
        s, S, 100 * s / S,
        format(elapsed, digits = 3),
        format(eta, digits = 3)
      ))
      flush.console()
    }

    # ---- Generate Data ----
    dat <- invisible(make_data(n, p, miss_cols, mechanism = mechanism, miss_prop = missing_prop))

    full <- dat$data_full
    miss <- dat$data_miss

    true <- mean(full$x_3)

    # ---- Imputation ----
    imp_cart <- mice(miss, method = "cart", m = m_imp, printFlag = FALSE)
    imp_boot <- mice(miss, method = "cart_boot", m = m_imp, printFlag = FALSE)

    est_cart <- pool_mean(imp_cart, "x_3")
    est_boot <- pool_mean(imp_boot, "x_3")

    # ---- Coverage ----
    cover_cart <- abs(est_cart$estimate - true) <= 1.96 * est_cart$se
    cover_boot <- abs(est_boot$estimate - true) <= 1.96 * est_boot$se

    results[s, ] <- c(est_cart$estimate,
                      est_boot$estimate,
                      cover_cart,
                      cover_boot,
                      est_cart$between,
                      est_boot$between)
  }

  cat("\nSimulation finished.\n")

  as.data.frame(results)
}