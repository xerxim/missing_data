#helper_functions_regression_estimation


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


make_data <- function(n, missing_prop) {
  x <- rnorm(n)
  y <- 1 + 2*x + rnorm(n)

  # impose missingness in x
  miss <- rbinom(n, 1, missing_prop)
  x[miss == 1] <- NA

  data.frame(x = x, y = y)
}

fit_model <- function(dat) {
  fit <- lm(y ~ x, data = dat)
  coef(summary(fit))["x", ]
}

run_mc <- function(S = 100, n = 300, missing_prop = 0.5, m = 10) {

  TRUE_BETA <- 2

  res <- data.frame(
    cart_est  = numeric(S),
    boot_est  = numeric(S),
    cart_cover = numeric(S),
    boot_cover = numeric(S),
    cart_len  = numeric(S),
    boot_len  = numeric(S),
    cart_B    = numeric(S),
    boot_B    = numeric(S)
  )

  for (s in 1:S) {

    cat("Simulation", s, "of", S, "\n")

    dat <- make_data(n, missing_prop)

    ## ---------------- CART (standard mice) ----------------
    imp_cart <- mice(dat, m = m, method = "cart", printFlag = FALSE)

    fits_cart <- with(imp_cart, lm(y ~ x))
    pool_cart <- pool(fits_cart)
    summ_cart <- summary(pool_cart, conf.int = TRUE)

    res$cart_est[s]  <- summ_cart$estimate[2]
    res$cart_len[s]  <- summ_cart$`97.5 %`[2] - summ_cart$`2.5 %`[2]
    res$cart_cover[s] <- as.numeric(
      summ_cart$`2.5 %`[2] <= TRUE_BETA &
      summ_cart$`97.5 %`[2] >= TRUE_BETA
    )

    # Between-imputation variance
    qhat <- sapply(fits_cart$analyses, coef)["x", ]
    res$cart_B[s] <- var(qhat)


    ## ---------------- Bootstrap CART (your method) ----------------
    imp_boot <- mice(dat, m = m, method = "cart_boot", printFlag = FALSE)

    fits_boot <- with(imp_boot, lm(y ~ x))
    pool_boot <- pool(fits_boot)
    summ_boot <- summary(pool_boot, conf.int = TRUE)

    res$boot_est[s]  <- summ_boot$estimate[2]
    res$boot_len[s]  <- summ_boot$`97.5 %`[2] - summ_boot$`2.5 %`[2]
    res$boot_cover[s] <- as.numeric(
      summ_boot$`2.5 %`[2] <= TRUE_BETA &
      summ_boot$`97.5 %`[2] >= TRUE_BETA
    )

    qhatb <- sapply(fits_boot$analyses, coef)["x", ]
    res$boot_B[s] <- var(qhatb)


    if (s %% 10 == 0) {
      cat("  Completed", s, "simulations\n\n")
    }
  }

  res
}
