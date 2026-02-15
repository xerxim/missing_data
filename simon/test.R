# Testdata generation.
set.seed(161)
library("rpart")

test <- function (y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, ...) 
{
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

n <- 10
X1 <- rnorm(n, 8, 3)
X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)

X3_latent <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))

X3 <- cut(
  X3_latent,
  breaks = quantile(X3_latent, probs = c(0, 0.33, 0.66, 1)),
  labels = c("low","medium","high"),
  include.lowest = TRUE
)

data1 <- data.frame(X1, X2, X3)


# Generate missing values
data1$X3[sample(x = 1:n, size = n/2, replace = FALSE)] <- NA

# Define args.
data <- data1

y <- data$X3
ry <- complete.cases(y)
wy <- !ry
x <- data[c("X1", "X2")]


i = test(y, ry, x, wy)
