library(mice)
library(rpart)

mi_cart_bootstrap <- function(data, m = 20) {
  
  # Input Checks: gibt es wirklich nur eine Variable mit missings?
  # -> deren Namen extracten
  if (is.data.frame(data) == FALSE) {
    stop("Braucht Dataframe als Input")
  }
  
  na_counts <- colSums(is.na(data))
  y_name <- names(na_counts[na_counts > 0])
  
  if (length(y_name) != 1) {
    stop("Data frame has", length(y_name), "columns with missing values when
         it needs to be exactly one.")
  } 
  
  
  
  # datensatz splitten in da wos fehlt und da wo nicht
  mis <- is.na(data[y_name])
  data_y_mis <- data[mis,]
  data_y_obs <- data[!mis,]
  
  # n calcen/festlegen
  n <- length(data_y_obs)
  imps <- list()
  
  for (i in 1:m) {
    
    # https://stefvanbuuren.name/fimd/sec-cart.html 
    
    #Draw a bootstrap sample (˙yobs,˙Xobs) of size n1 from (yobs,Xobs).
    boot <- sample(x = nrow(data_y_obs), size = n, replace = T)
    boot_data <- data_y_obs[boot, ]
    yobs <- boot_data[[y_name]]
    xobs <- boot_data[y_name, , drop = FALSE]
    
    #Fit ˙yobs by ˙Xobs by a tree model f(X).
    if (!is.factor(yobs)) {
      fit <- rpart::rpart(yobs ~ ., data = boot_data, 
                          method = "anova", control = rpart::rpart.control(minbucket = 5, 
                                                                           cp = 1e-04))
      leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, ])))
      fit$frame$yval <- as.numeric(row.names(fit$frame))
      #Predict the n0 terminal nodes gj from f(Xmis).
      nodes <- predict(object = fit, newdata = data_y_mis)
      #Construct n0 sets Zj of all cases at node gj, each containing dj candidate donors.
      donor <- lapply(nodes, function(s) yobs[leafnr == s])
      impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 
                                                            1), numeric(1))
    }
    else {
      cat.has.all.obs <- table(yobs) == sum(!mis)
      if (any(cat.has.all.obs)) {
        return(rep(levels(yobs)[cat.has.all.obs], sum(mis)))
      }
      xy <- boot_data
      xy <- droplevels(xy)
      fit <- rpart::rpart(yobs ~ ., data = xy, method = "class", 
                          control = rpart::rpart.control(minbucket = 5, 
                                                         cp = 1e-04))
      #Predict the n0 terminal nodes gj from f(Xmis).
      nodes <- predict(object = fit, newdata = data_y_mis)
      #Construct n0 sets Zj of all cases at node gj, each containing dj candidate donors.
      impute <- apply(nodes, MARGIN = 1, FUN = function(s) {
        sample(colnames(nodes), size = 1, prob = s)
      })
    }
    
    #imputationen anhängen
    imps[[i]] <- impute
    i <- i + 1
    
  }

  #output schön in form bringen
  output <- imps
  output
  
}
