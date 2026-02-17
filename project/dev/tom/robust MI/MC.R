set.seed(123)
pacman::p_load("mice", "tidyverse")
source("tom/robust MI/helper_functions.R")
source("tom/mice.impute.cart_boot.R")

#Ground truth
# sample size
n <- 50000
S <- 10000
mean_y <- numeric(S)
quantile_y <- numeric(S)

yTot <- c()
for(i in 1:S) {

  if(i%%50 == 0){
    flush.console()
    cat(i, "of", S, "Iterations done. \n")
  }

  # one normally and one uniformly distributed variable
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- runif(n = n, min = 0, max = 2)
  
  # continuous outcome variable with non-linear terms and non-normal error term
  eps <- rchisq(n = n, df = 107/96)
  y <- -3 + 3.5 * x1^2 + 2.75 * x2^3 + (eps - 107/96)
  
  #quantities of interest
  mean_y[i] <- mean(y)
  quantile_y[i] <- quantile(y, probs = 0.9)
  
  if(i < 6){  #save the first 5 iterations for better density estimate
    yTot <- c(yTot, y)  #enough for kernel density estimation
  }
  
}

# Monte carlo estimates
(true_mean <- mean(mean_y))
(true_quant <- mean(quantile_y))

dens_y <- approxfun(density(yTot)) #Kernel Density estimate (approxfun is needed to calculate the density at all points in y)


# number of simulation cycles
R <- 500

# store the results (rel. bias and coverage) in a matrix for each method
resBD <- matrix(nrow = R, ncol = 4)
colnames(resBD) <- c("relBias_mean", "Coverage_mean", 
                     "relBias_quant","Coverage_quant")

resMInorm <- resMIpmm <- resMImidas <- resMIcart <- resMIcartboot <- resMIrf <- resBD

# number of multiple imputations
M <- 10

# number of iterations for the chained equations (see mice ALGO: t = 1, ...,T)
niter <- 1

# sample size
n <- 2000



##MC Study
for(r in 1:R) {
  
  if (r%%10 == 0){
    cat(r, "of", R, "iterations done. \n")
  }

  ### Data generation
    
  # one normally and one uniformly distributed variable
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- runif(n = n, min = 0, max = 2)
  
  # continuous outcome variable with non-linear terms and non-linear error term
  eps <- rchisq(n = n, df = 107/96)
  y <- -3 + 3.5 * x1^2 + 2.75 * x2^3 + (eps - 107/96)
  
  data <- as.data.frame(cbind(x1, x2, y))
  data_bd <- data 
  
  ### Before deletion
  
  # mean
  mean_est <- mean(data_bd$y)
  emp_sd <- sd(data_bd$y)
  
  ci_up <- mean_est + (qnorm(p = 0.975)*emp_sd)/sqrt(length(data_bd$y))
  ci_low <- mean_est - (qnorm(p = 0.975)*emp_sd)/sqrt(length(data_bd$y))
  
  # quantile 
  quant_est <- quantile(data_bd$y, probs = 0.9)
  quant_sd <- sqrt(quantVar(y =data_bd$y, p = 0.9)) 
  
  ci_up_quant <- quant_est + (qnorm(p = 0.975)*quant_sd)/sqrt(length(data_bd$y))
  ci_low_quant <- quant_est - (qnorm(p = 0.975)*quant_sd)/sqrt(length(data_bd$y))
    
  
  resBD[r,] <- c(rel.bias(true_mean, mean_est),
                 coverage(true_mean, ci_low, ci_up),
                 rel.bias(quant_est, true_quant),
                 coverage(true_quant, ci_low_quant, ci_up_quant))
  
  
  
  ### Generation of missing data
  
  # MAR in y
  z1 <-rnorm(n = n, mean = 0, sd = sqrt(16)) * x2
  lin_y <- 1.75 - 1.5 * z1
  prob_y <- pnorm(lin_y)    # yields around 35% missing values
  res_y <- rbinom(n = n, size = 1, prob = prob_y) 
  
  data$y[which(res_y==0)] <- NA
  
  ### Multiple imputation and analysis
  
  # use all variables as predictors, but just consider linear relationships..
  
  ## initialization
  ini <- mice(data,maxit=0)
  
  ## PMM (default method)
  imp_pmm <- mice(data, m=M, maxit=niter, print=FALSE)
  
  mice_pmm <- complete(imp_pmm, action="long", include=FALSE)
  
  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_pmm, mean)$y
  #theta_MI <- mice_pmm %>% group_by(.imp) %>% summarise(mean(y))
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_pmm, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_pmm, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_pmm, quantVar, p = 0.9)$y/n
  
  
  ana_pmm <- MI_analysis(theta_MI,var_MI,m = M)   # -> Q_bar,CIlow,CIup
  ana_pmm_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMIpmm[r,] <- c(rel.bias(true_mean, ana_pmm[1]), 
                    coverage(true_mean, ana_pmm[2], ana_pmm[3]),
                    rel.bias(true_quant, ana_pmm_quant[1]),
                    coverage(true_quant, ana_pmm_quant[2], ana_pmm_quant[3]))
  
  ## midas
  meth <- ini$method
  meth["y"] <- "midastouch"
  
  imp_midas <- mice(data, m=M, maxit=niter, method=meth, print=FALSE)
  mice_midas <- complete(imp_midas, action="long", include=FALSE)
  
  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_midas, mean)$y
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_midas, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_midas, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_midas, quantVar, p = 0.9)$y/n
  
  ana_midas <- MI_analysis(theta_MI,var_MI,M)   # -> Q_bar,CIlow,CIup
  ana_midas_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMImidas[r,] <- c(rel.bias(true_mean, ana_midas[1]), 
                    coverage(true_mean, ana_midas[2], ana_midas[3]),
                    rel.bias(true_quant, ana_midas_quant[1]),
                    coverage(true_quant, ana_midas_quant[2], ana_midas_quant[3]))
  
  ## norm
  meth <- ini$method
  meth["y"] <- "norm"
  
  imp_norm <- mice(data, m=M, maxit=niter, method=meth, print=FALSE)
  mice_norm <- complete(imp_norm, action="long", include=FALSE)
  
  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_norm, mean)$y
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_norm, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_norm, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_norm, quantVar, p = 0.9)$y/n
  
  ana_norm <- MI_analysis(theta_MI,var_MI,M)   # -> Q_bar,CIlow,CIup
  ana_norm_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMInorm[r,] <- c(rel.bias(true_mean, ana_norm[1]), 
                    coverage(true_mean, ana_norm[2], ana_norm[3]),
                    rel.bias(true_quant, ana_norm_quant[1]),
                    coverage(true_quant, ana_norm_quant[2], ana_norm_quant[3]))

  ## cart
  meth <- ini$method
  meth["y"] <- "cart"
  
  imp_cart <- mice(data, m=M, maxit=niter, method=meth, print=FALSE)
  mice_cart <- complete(imp_cart, action="long", include=FALSE)

  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_cart, mean)$y
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_cart, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_cart, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_cart, quantVar, p = 0.9)$y/n
  
  
  ana_cart <- MI_analysis(theta_MI,var_MI,M)   # -> Q_bar,CIlow,CIup
  ana_cart_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMIcart[r,] <- c(rel.bias(true_mean, ana_cart[1]), 
                    coverage(true_mean, ana_cart[2], ana_cart[3]),
                    rel.bias(true_quant, ana_cart_quant[1]),
                    coverage(true_quant, ana_cart_quant[2], ana_cart_quant[3]))
  ## cart_boot
  meth <- ini$method
  meth["y"] <- "cart_boot"
  
  imp_cart_boot <- mice(data, m=M, maxit=niter, method=meth, print=FALSE)
  mice_cart_boot <- complete(imp_cart_boot, action="long", include=FALSE)

  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_cart_boot, mean)$y
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_cart_boot, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_cart_boot, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_cart_boot, quantVar, p = 0.9)$y/n
  
  
  ana_cart_boot <- MI_analysis(theta_MI,var_MI,M)   # -> Q_bar,CIlow,CIup
  ana_cart_boot_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMIcartboot[r,] <- c(rel.bias(true_mean, ana_cart_boot[1]), 
                    coverage(true_mean, ana_cart_boot[2], ana_cart_boot[3]),
                    rel.bias(true_quant, ana_cart_boot_quant[1]),
                    coverage(true_quant, ana_cart_boot_quant[2], ana_cart_boot_quant[3]))
  ## random forest
  meth <- ini$method
  meth["y"] <- "rf"
  
  imp_rf <- mice(data, m=M, maxit=niter, method=meth, print=FALSE)
  mice_rf <- complete(imp_rf, action="long", include=FALSE)
  
  # mean for each imputed data set
  theta_MI <- aggregate(y ~.imp, data=mice_rf, mean)$y
  # variance of mean for each imputed data set
  var_MI <-   aggregate(y ~.imp, data=mice_rf, var)$y/n
  
  quant_MI <- aggregate(y ~.imp, data = mice_rf, quantile, 0.9)$y
  quant_MI_var <- aggregate(y ~.imp, data = mice_rf, quantVar, p = 0.9)$y/n
  
  ana_rf <- MI_analysis(theta_MI,var_MI,M)   # -> Q_bar,CIlow,CIup
  ana_rf_quant <- MI_analysis(quant_MI, quant_MI_var,m = M)
  
  resMIrf[r,] <- c(rel.bias(true_mean, ana_rf[1]), 
                    coverage(true_mean, ana_rf[2], ana_rf[3]),
                    rel.bias(true_quant, ana_rf_quant[1]),
                    coverage(true_quant, ana_rf_quant[2], ana_rf_quant[3]))
}

#Export MC Result Dataframes
#save(resBD, resMIcart, resMIcartboot, resMImidas, resMInorm, resMIpmm, resMIrf, file = "tom/robust MI/mc_raw_results.RData")

Bias <- Coverage <- matrix(nrow=2, ncol=7)
colnames(Bias) <- colnames(Coverage) <- c("BD","pmm","midas","norm","cart","cart_boot","rf")
rownames(Bias) <- rownames(Coverage) <- c("mean(y)","quantile")

Bias[, "BD"] <- colMeans(resBD)[c(1,3)]
Bias[, "pmm"] <- colMeans(resMIpmm)[c(1,3)] 
Bias[, "midas"] <- colMeans(resMImidas)[c(1,3)] 
Bias[, "norm"] <- colMeans(resMInorm)[c(1,3)] 
Bias[, "cart"] <- colMeans(resMIcart)[c(1,3)]
Bias[, "cart_boot"] <- colMeans(resMIcartboot)[c(1,3)] 
Bias[, "rf"] <- colMeans(resMIrf)[c(1,3)] 

Coverage[, "BD"] <- colMeans(resBD)[c(2,4)]
Coverage[, "pmm"] <- colMeans(resMIpmm)[c(2,4)] 
Coverage[, "midas"] <- colMeans(resMImidas)[c(2,4)] 
Coverage[, "norm"] <- colMeans(resMInorm)[c(2,4)] 
Coverage[, "cart"] <- colMeans(resMIcart)[c(2,4)]
Coverage[, "cart_boot"] <- colMeans(resMIcartboot)[c(2,4)] 
Coverage[, "rf"] <- colMeans(resMIrf)[c(2,4)] 

round(Bias, 4)
round(Coverage, 4 )

graph_data <- data.frame(methode = colnames(Coverage),
                         coverage = Coverage[1,])

ggplot(graph_data, aes(x = methode, y = coverage))+
 geom_bar(position = "dodge", stat = "identity", color = "gray80")+
  geom_hline(yintercept = 0.95, lty = 2)+
  theme_classic()+
  labs(x = "Methode", 
       y = "Coverage",
       title ="Nichtlineare Daten")+
   scale_x_discrete(labels = function(l) parse(text=l))

alles <- rbind(
      cbind(rep("BD", nrow(resBD)),
      resBD ),
      cbind(rep("Cart", nrow(resMIcart)),
      resBD ),
      cbind(rep("Cart Bootsrap", nrow(resMIcartboot)),
      resBD ),
      cbind(rep("Midas", nrow(resMImidas)),
      resBD ),
      cbind(rep("Norm", nrow(resMInorm)),
      resBD ),
      cbind(rep("PMM", nrow(resMIpmm)),
      resBD ),
      cbind(rep("RF", nrow(resMIrf)),
      resBD )

) %>% data.frame()


alles <- alles %>%
  mutate(across(-1, as.numeric))


colnames(alles)[1] <- "methode"

ggplot(alles, aes(x = methode, y = relBias_mean))+
  geom_boxplot()+
   geom_hline(yintercept =  0, lty = 2)+
  theme_classic()

