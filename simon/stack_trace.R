library(mice)

# Testdata generation.
set.seed(161)

n <- 10
X1 <- rnorm(n, 8, 3)
X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
X3 <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))
data1 <- as.data.frame(cbind(X1, X2, X3))

# Generate missing values
data1$X3[sample(x = 1:n, size = n/2, replace = FALSE)] <- NA

# Define args.
data = data1
m = 3
method = "cart"
predictorMatrix = matrix(
                          data = c(
                            0, 1, 1, 
                            1, 0, 1,
                            1, 1, 0
                          ),
                          nrow = 3
                        )
ignore = NULL
where = is.na(data)

#debug(mice:::mice)

# dann normal aufrufen
imp <- mice(
  data = data1, m = m, method = "cart",
  predictorMatrix = predictorMatrix,
  ignore = NULL, where = is.na(data1), maxit = 1
)
imp$imp$X3
