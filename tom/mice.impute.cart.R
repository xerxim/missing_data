mice.impute.cart
function (y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, ...) 
{
    install.on.demand("rpart")
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
    if (!is.factor(yobs)) {
        fit <- rpart::rpart(yobs ~ ., data = cbind(yobs, xobs), 
            method = "anova", control = rpart::rpart.control(minbucket = minbucket, 
                cp = cp, ...))
        leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, 
            ])))
        fit$frame$yval <- as.numeric(row.names(fit$frame))
        nodes <- predict(object = fit, newdata = xmis)
        donor <- lapply(nodes, function(s) yobs[leafnr == s])
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