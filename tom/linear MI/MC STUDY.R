#MC STUDY
pacman::p_load("VIM","mice","MASS","tidyverse","paletteer")
source("tom/linear MI/vc_mcstudy_helper.R")
source("tom/mice.impute.cart_boot.R")

trueMean <- 12.8
trueBetas <-  c(5, 0.6, 0.5)

nIter <- 200
n <- 300
M <- 10

combined_results <- data.frame()
parameter = c("mu", "beta[0]", "beta[1]", "beta[2]")

#MC mit Linearen Daten
for (i in 1:nIter) {    
  

  if(i%%10 == 0){
    flush.console()
    cat(i, "of", nIter, "Iterations done. \n")
  }
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
    
      ### Generate missing values in X2
    misind2 <- sample(1:n,round(n/4))
    MISS_dat$X2[misind2] <- NA
    obsind2 <- which(!is.na(MISS_dat$X2))
    n.obs <- length(obsind)
  
  #Fit on full data
  fit_full <- lm(X3~X1+ X2, BD_dat)
  summary(fit_full) 
  sample_estimates <- c(mean(BD_dat$X3),fit_full$coefficients)
  sample_estimates

  coef_CI <- confint(fit_full)
  betas_covered <- (coef_CI[,1] <= trueBetas & coef_CI[,2] >= trueBetas)

  muCI <- t.test(BD_dat$X3)$conf.int
  mu_covered <- (muCI[1] <= trueMean & muCI[2] >= trueMean)
  
  rel_biases <- calc_rel_bias(c(trueMean, trueBetas),
                              sample_estimates) %>% unlist 


    res_full <- data.frame(
    parameter = parameter,
    method = rep("full data", 4),
    estimates =   sample_estimates,
    true_values = c(trueMean, trueBetas),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )

  combined_results <- rbind(combined_results, res_full)
  
  
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
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %` >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_pmm <- data.frame(
    parameter = parameter,
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
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %` >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart <- data.frame(
    parameter = parameter,
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
    betas_covered <- (summ_pmm$`2.5 %` <= trueBetas & summ_pmm$`97.5 %` >= trueBetas)
  
    rel_biases <- calc_rel_bias(c(trueMean, trueBetas),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart_boot <- data.frame(
    parameter = parameter,
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

#Subtitle
dgp_label_txt <- paste(
  "Target: X3",
  "X1 ~ N(8, 3^2)",
  "X2 = 10 - 0.5·X1 + e2,  e2 ~ N(0, 3^2)",
  "X3 = 5 + 0.6·X1 + 0.5·X2 + e3,  e3 ~ N(0, 2)",
  sep = "\n"
)


ggplot(df_summary_cov, aes(x = parameter, y = coverage, fill = method))+
 geom_bar(position = "dodge", stat = "identity", color = "gray80")+
  geom_hline(yintercept = 0.95, lty = 2)+
  theme_classic()+
  scale_fill_paletteer_d("wesanderson::AsteroidCity1")+
  labs(x = "Parameter", 
       y = "Coverage",
       title ="Lineare Daten",
      subtitle = dgp_label_txt)+
   scale_x_discrete(labels = function(l) parse(text=l))
  


ggplot(combined_results, aes(x = parameter, y = relative_bias, fill = method))+
  geom_boxplot()+
   geom_hline(yintercept =  0, lty = 2)+
  theme_classic()+
  scale_fill_paletteer_d("wesanderson::AsteroidCity1")+
    labs(x = "Parameter", 
         y = "Rel. Bias",
         title ="Lineare Daten",
         subtitle = dgp_label_txt)+
  scale_x_discrete(labels = function(l) parse(text=l))







#MC mit nicht linearen Daten

#beta_star: population least_squares projection
N_large <- 3e6

X1 <- rnorm(N_large, 8, 3)
X2 <- 2 + 0.4*X1 + 0.08*(X1-8)^2 + 2*sin(X1/2) + rnorm(N_large,0,2)
sigma3 <- sqrt(1 + 0.15*abs(X1))
X3 <- 4 + 0.5*X1 + 0.3*X2 + 0.15*X1*X2 - 0.02*X2^2 + rnorm(N_large,0,sigma3)


beta_star <- coef(lm(X3 ~ X1 + X2))
beta_star

#mu_star: population mean projection
mu_star  <- mean(X3)
mu_star

for (i in 1:nIter) {    
  

  if(i%%10 == 0){
    flush.console()
    cat(i, "of", nIter, "Iterations done. \n")
  }
  ## ---------------------------
## Nonlinear Data Generating Process
## ---------------------------

X1 <- rnorm(n, 8, 3)

# nonlinear + oscillating relationship
X2 <- 2 +
      0.4 * X1 +
      0.08 * (X1 - 8)^2 +
      2 * sin(X1 / 2) +
      rnorm(n, 0, 2)

# interaction + quadratic + heteroskedastic noise
sigma3 <- sqrt(1 + 0.15 * abs(X1))

X3 <- 4 +
      0.5 * X1 +
      0.3 * X2 +
      0.15 * X1 * X2 -
      0.02 * X2^2 +
      rnorm(n, 0, sigma3)

# Before deletion
BD_dat <- data.frame(X1, X2, X3)

# Copy for missingness
MISS_dat <- BD_dat

  
  #Before Deletion Data
  BD_dat <- as.data.frame(cbind(X1, X2, X3))
  
  #Data with Missing
  MISS_dat <- BD_dat
    
    ### Generate missing values
    misind <- sample(1:n,round(n/2))
    MISS_dat$X3[misind] <- NA
    obsind <- which(!is.na(MISS_dat$X3))
    n.obs <- length(obsind)
    
  #Fit on full data
  fit_full <- lm(X3~X1+ X2, BD_dat)
  summary(fit_full) 
  sample_estimates <- c(mean(BD_dat$X3),fit_full$coefficients)
  sample_estimates

  coef_CI <- confint(fit_full)
  betas_covered <- (coef_CI[,1] <= beta_star & coef_CI[,2] >= beta_star)

  muCI <- t.test(BD_dat$X3)$conf.int
  mu_covered <- (muCI[1] <= mu_star & muCI[2] >= mu_star)
  
  rel_biases <- calc_rel_bias(c(mu_star, beta_star),
                              sample_estimates) %>% unlist 


    res_full <- data.frame(
    parameter = parameter,
    method = rep("full data", 4),
    estimates =   sample_estimates,
    true_values = c(mu_star, beta_star),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )

  combined_results <- rbind(combined_results, res_full)
  
  
  #### MI Using Mice
    
    #1 Standard Mice (so pmm)
  imp <- mice(MISS_dat, m = M,  printFlag = FALSE)

    fits_pmm <- with(imp, lm(X3 ~ X1 +X2))
    pool_pmm <- pool(fits_pmm)
    summ_pmm <- summary(pool_pmm, conf.int = TRUE)
    summ_pmm
    mean_est <- pool_mean(imp, "X3")
    mean_est

    mu_covered <- (mean_est$lower <= mu_star & mean_est$upper >= mu_star)
    betas_covered <- (summ_pmm$`2.5 %` <= beta_star & summ_pmm$`97.5 %` >= beta_star)
  
    rel_biases <- calc_rel_bias(c(mu_star, beta_star),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_pmm <- data.frame(
    parameter = parameter,
    method = rep("pmm", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(mu_star, beta_star),
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

    mu_covered <- (mean_est$lower <= mu_star & mean_est$upper >= mu_star)
    betas_covered <- (summ_pmm$`2.5 %` <= beta_star & summ_pmm$`97.5 %` >= beta_star)
  
    rel_biases <- calc_rel_bias(c(mu_star, beta_star),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart <- data.frame(
    parameter = parameter,
    method = rep("cart", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(mu_star, beta_star),
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

    mu_covered <- (mean_est$lower <= mu_star & mean_est$upper >= mu_star)
    betas_covered <- (summ_pmm$`2.5 %` <= beta_star & summ_pmm$`97.5 %` >= beta_star)
  
    rel_biases <- calc_rel_bias(c(mu_star, beta_star),c(mean_est[1],summ_pmm$estimate) %>% unlist )
  
  res_cart_boot <- data.frame(
    parameter = parameter,
    method = rep("cart with bootstrap", 4),
    estimates = c(mean_est[1],summ_pmm$estimate) %>% unlist,
    true_values = c(mu_star, beta_star),
    coverage = c(mu_covered, betas_covered) %>% as.numeric(),
    relative_bias = rel_biases 
  )
  
  combined_results <- rbind(combined_results, res_cart_boot)
  
}

df_summary_cov_nl <- combined_results %>%
  group_by(parameter, method) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE))


ggplot(df_summary_cov_nl, aes(x = parameter, y = coverage, fill = method))+
 geom_bar(position = "dodge", stat = "identity", color = "gray80")+
  geom_hline(yintercept = 0.95, lty = 2)+
  theme_classic()+
  scale_fill_paletteer_d("wesanderson::AsteroidCity1")+
  labs(x = "Parameter", 
       y = "Coverage",
       title ="Nichtlineare Daten")+
   scale_x_discrete(labels = function(l) parse(text=l))
  


ggplot(combined_results, aes(x = parameter, y = relative_bias, fill = method))+
  geom_boxplot()+
   geom_hline(yintercept =  0, lty = 2)+
  theme_classic()+
  scale_fill_paletteer_d("wesanderson::AsteroidCity1")+
    labs(x = "Parameter", 
         y = "Rel. Bias",
         title ="Nichtlineare Daten")+
  scale_x_discrete(labels = function(l) parse(text=l))


