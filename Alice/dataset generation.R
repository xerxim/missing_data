set.seed(1)

n <- 250
X1 <- rnorm(n, 8, 3)
X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
X3 <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))
data1 <- as.data.frame(cbind(X1, X2, X3))

# Quantities of interest for the original sample data set
(bef.Imp <- cbind(mean = mean(X3),
                  var = var(X3),
                  corX1_X3 = cor(X1, X3))
)


X3NonNA <- X3 # save for later

# Generate missing values
data1$X3[sample(x = 1:n, size = n/2, replace = FALSE)] <- NA

# Missing indicators for missing data in X3
misind <- is.na(data1$X3)

# Indicators for observed (not missing) data in X3
obsind <- !is.na(data1$X3)