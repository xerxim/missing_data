pacman::p_load(mice)

#### V2
source("tom/helper_functions_regression_estimation.R")

res <- run_mc(S = 100, n = 300, missing_prop = 0.5)

#coverage


coverage <- c("Coverage",colMeans(res[,c("cart_cover","boot_cover")]))

#bias
bias <- c("Bias", colMeans(res[,c("cart_est","boot_est")]) - 2)



#rmse
rmse <- c("rmse", sqrt(colMeans((res[,c("cart_est","boot_est")] - 2)^2)))

#CI length
CIlength <- c("CI Length", colMeans(res[,c("cart_len","boot_len")]))

#Between-Imputation Variance Ratio
paste("Between-Imputation Variance Ratio:" , mean(res$boot_B) / mean(res$cart_B))

summary_mc <- rbind(coverage, bias, rmse, CIlength)
summary_mc

rownames(summary_mc) <- NULL

##another one

res_2 <- run_mc(S = 1000, n = 300, missing_prop = 0.8)

#coverage


coverage <- c("Coverage",colMeans(res_2[,c("cart_cover","boot_cover")]))

#bias
bias <- c("Bias", colMeans(res_2[,c("cart_est","boot_est")]) - 2)

#rmse
rmse <- c("rmse", sqrt(colMeans((res_2[,c("cart_est","boot_est")] - 2)^2)))

#CI length
CIlength <- c("CI Length", colMeans(res_2[,c("cart_len","boot_len")]))

#Between-Imputation Variance Ratio
paste("Between-Imputation Variance Ratio:" , mean(res_2$boot_B) / mean(res_2$cart_B))

summary_mc_2 <- rbind(coverage, bias, rmse, CIlength)
rownames(summary_mc_2) <- NULL
summary_mc
summary_mc_2


