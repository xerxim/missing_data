#Only works for numreic covariates so far, working on else (Line 43 etc)

mice.impute.cart_boot <- function (y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, ...) 
{
    #install.on.demand("rpart")
    # Create wy as inversion if ry if missing.
    if (is.null(wy)) {
        wy <- !ry
    }
    # Ensure minbucket >= 1.
    minbucket <- max(1, minbucket)
    # Ensure x has minum one dimension thats only "1".
    if (dim(x)[2] == 0) {
        x <- cbind(x, 1)
        dimnames(x) <- list(NULL, "int")
    }
    # Filter missings and observations.
    xobs <- data.frame(x[ry, , drop = FALSE])
    xmis <- data.frame(x[wy, , drop = FALSE])
    yobs <- y[ry]
    
    # Create bootstrapped data.
    n_obs <- length(yobs)
    boot_id <- sample.int(n_obs, size = n_obs, replace = TRUE)

    y_boot <- yobs[boot_id]
    x_boot <- xobs[boot_id, , drop = FALSE]
    boot_ry <- ry[boot_id]
    
    # Use different models for factors and non-factors.
    if (!is.factor(yobs)) {
        # Fit tree to bootstrapped data.
        fit <- rpart::rpart(y_boot ~ ., data = cbind(y_boot, x_boot), 
            method = "anova", control = rpart::rpart.control(minbucket = minbucket, 
                cp = cp, ...))
        # Prepare fit for prediction.
        leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, 
            ])))
        fit$frame$yval <- as.numeric(row.names(fit$frame))
        # Predict missings.
        nodes <- predict(object = fit, newdata = xmis)
        donor <- lapply(nodes, function(s) y_boot[leafnr == s])
        impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 
            1), numeric(1))
    }
    # For factors:
    else {
        # Check if all observations are in the same category.
        cat.has.all.obs <- table(y_boot) == sum(boot_ry)
        # If they are just set missings as this category, no rpart needed.
        if (any(cat.has.all.obs)) {
            return(rep(levels(y_boot)[cat.has.all.obs], sum(wy)))
        }
        # Prepare data for rpart with method "class".
        xy <- cbind(y_boot, x_boot)
        xy <- droplevels(xy)
        # Fit tree to bootstrapped data.
        fit <- rpart::rpart(y_boot ~ ., data = xy, method = "class", 
            control = rpart::rpart.control(minbucket = minbucket, 
                cp = cp, ...))
        # Predict missings.
        nodes <- predict(object = fit, newdata = xmis)
        impute <- apply(nodes, MARGIN = 1, FUN = function(s) {
            sample(colnames(nodes), size = 1, prob = s)
        })
    }
    # Return predicted missings.
    impute
}
