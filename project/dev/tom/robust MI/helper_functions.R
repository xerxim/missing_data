## functions for diagnostics

coverage <- function(true.value, CI.low, CI.up){
  ifelse(test = CI.low <= true.value && CI.up >= true.value, yes = 1, no = 0)
}

rel.bias <- function(true.value, est) {
  rel_bias <- (est - true.value) / true.value
  return(rel_bias)
}
## function for Rubin's combining rules, gives pooled estimates, CI

MI_analysis <- function(Q.hat,U.hat,m){
  
  if (class(Q.hat)!="numeric") {
    stop("Estimator vector for all imputations has a different class")
  }
  else{
    # pooled estimator
    Q_bar <- sum(Q.hat)/m
    # within-variance
    U_bar <- sum(U.hat)/m
    # between-variance
    B <- (1/(m-1))*sum((Q.hat-Q_bar)^2)
  }
  
  # total variance
  Tot <- U_bar+B+B/m
  # degrees of freedom
  df <- (m-1)*(1+(m/(m+1))*U_bar/B)^2
  
  # confidence intervals
  CIlow <- Q_bar-qt(0.975,df)*sqrt(Tot)
  CIup <- Q_bar+qt(0.975,df)*sqrt(Tot)
  r <- (B+B/m)/U_bar
  
  # fraction of missing information
  FMI <- (r+2/(df+3))/(1+r)
  
  # t-test
  t_value <- Q_bar/sqrt(Tot)
  
  # p-value
  p.value <-2*(1-pt(abs(t_value),df))
  
  return(cbind(Q_bar,CIlow,CIup))
}

quantVar <- function(y, p = 0.9){
  quant <- quantile(y, prob = p)
  sigma <- (p*(1-p))/(dens_y(quant)^2)
  return(sigma)
}