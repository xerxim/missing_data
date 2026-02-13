pacman::p_load(mice)

source("tom/helper_functions_mean_estimation.R")

## Single Run
dataset <- make_data(10000, 6, c(3,4),mechanism = "MAR")

full <- dataset$data_full
miss <- dataset$data_miss


imp_cart <- mice(miss, method = "cart", m = 10)
imp_cart_boot <- mice(miss, method = "cart_boot", m = 10)
imp <- mice(miss, m = 10)


boot_sum <- unlist(pool_mean(imp_cart_boot,"x_3"))
cart_sum <- unlist(pool_mean(imp_cart,"x_3"))
default_sum <- unlist(pool_mean(imp,"x_3"))
full_data_sum <- c(mean(full$x_3), sd(full$x_3) / sqrt(length(full$x_3)), NA, NA )

rbind(boot_sum,cart_sum, default_sum, full_data_sum)


## MC

#might take a while
res <- run_mc(S = 100, n = 300, missing_prop = 0.5)


colMeans(res[,c("cart_cover","boot_cover")])
mean(res$boot_B) / mean(res$cart_B)
colMeans(res[,c("cart_est","boot_est")])


res <- run_mc(S = 100, n = 300, missing_prop = 0.5)

rm(list = ls())

#### V2
source("tom/helper_functions_regression_estimation.R")

res <- run_mc(S = 100, n = 300, missing_prop = 0.5)

#coverage
colMeans(res[,c("cart_cover","boot_cover")])

#bias
colMeans(res[,c("cart_est","boot_est")]) - 2

#rmse
sqrt(colMeans((res[,c("cart_est","boot_est")] - 2)^2))

#CI length
colMeans(res[,c("cart_len","boot_len")])

#Between Imp
mean(res$boot_B) / mean(res$cart_B)
