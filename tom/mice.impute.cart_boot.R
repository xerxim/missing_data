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
    #TODO
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
