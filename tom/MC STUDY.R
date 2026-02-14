#MC STUDY
pacman::p_load("VIM","mice","MASS","tidyverse")
source("tom/vc_mcstudy_helper.R")
source("tom/mice.impute.cart_boot.R")

trueMean <- 12.8
trueBetas <-  c(5, 0.6, 0.5)

nIter <- 100
n <- 500
M <- 10

combined_results <- data.frame()

for (i in 1:nIter) {    
  
  ## Data generating process
  X1 <- rnorm(n, 8, 3)
  X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
  X3 <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))
  
  #Before Deletion Data
  BD_dat <- as.data.frame(cbind(X1, X2, X3))
  
  #Data with Missing
  MISS_dat <- BD_dat
    
    ### Generate missing values
    misind <- sample(1:n,round(n/2))
    MISS_dat$X3[misind] <- NA
    obsind <- which(!is.na(MISS_dat$X3))
    n.obs <- length(obsind)
    
   
    #### MI Using Mice
  
    
    #1 Standard Mice (so pmm)
  imp <- mice(MISS_dat, m = M,  printFlag = FALSE)

    fits_pmm <- with(imp, lm(X3 ~ X1 +X2))
    pool_pmm <- pool(fits_pmm)
    summ_pmm <- summary(pool_pmm, conf.int = TRUE)
    summ_pmm
    mean_est <- pool_mean(imp, "X3")
    mean_est

    mu_covered <- (mean_est$lower <= trueMean & mean_est$upper >= trueMean)
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %`[2] >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_pmm <- data.frame(
    parameter = c("mu", "b_0", "b_1", "b_2"),
    method = rep("pmm", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(trueMean, trueBetas),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )
  
  combined_results <- rbind(combined_results, res_pmm)

      #2 cart
  imp <- mice(MISS_dat, method = "cart", m = M,  printFlag = FALSE)

    fits_pmm <- with(imp, lm(X3 ~ X1 +X2))
    pool_pmm <- pool(fits_pmm)
    summ_pmm <- summary(pool_pmm, conf.int = TRUE)
    summ_pmm
    mean_est <- pool_mean(imp, "X3")
    mean_est

    mu_covered <- (mean_est$lower <= trueMean & mean_est$upper >= trueMean)
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %`[2] >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart <- data.frame(
    parameter = c("mu", "b_0", "b_1", "b_2"),
    method = rep("cart", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(trueMean, trueBetas),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )
  
  combined_results <- rbind(combined_results, res_cart)


        #3 cart mit bootstrap
  imp <- mice(MISS_dat, method = "cart_boot", m = M,  printFlag = FALSE)

    fits_pmm <- with(imp, lm(X3 ~ X1 +X2))
    pool_pmm <- pool(fits_pmm)
    summ_pmm <- summary(pool_pmm, conf.int = TRUE)
    summ_pmm
    mean_est <- pool_mean(imp, "X3")
    mean_est

    mu_covered <- (mean_est$lower <= trueMean & mean_est$upper >= trueMean)
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %`[2] >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart_boot <- data.frame(
    parameter = c("mu", "b_0", "b_1", "b_2"),
    method = rep("cart with bootstrap", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(trueMean, trueBetas),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )
  
  combined_results <- rbind(combined_results, res_cart_boot)
  
}



df_summary_cov <- combined_results %>%
  group_by(parameter, method) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE))

df_summary_cov



ggplot(df_summary, aes(x = parameter, y = coverage, fill = method))+
 geom_bar(position = "dodge", stat = "identity", color = "gray80")+
  geom_hline(yintercept = 0.95, lty = 2)+
  theme_classic()



ggplot(combined_results, aes(x = parameter, y = relative_bias, fill = method))+
  geom_boxplot()+
   geom_hline(yintercept =  0, lty = 2)+
  theme_classic()
