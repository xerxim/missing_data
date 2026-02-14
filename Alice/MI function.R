library(mice)
library(rpart)




#Algorithm 3.4 (Imputation under a tree model using the bootstrap.)

#Draw a bootstrap sample (˙yobs,˙Xobs) of size n1 from (yobs,Xobs).

#Fit ˙yobs by ˙Xobs by a tree model f(X).

#Predict the n0 terminal nodes gj from f(Xmis).

#Construct n0 sets Zj of all cases at node gj, each containing dj candidate donors.

#Draw one donor ij from Zj randomly for j=1,…,n0.

#Calculate imputations ˙yj=yij for j=1,…,n0. 


mi_cart_bootstrap <- function(data, maximp = 20, n = 50) {
  
  # Input Checks: gibt es wirklich nur eine Variable mit missings?
  # -> deren Namen extracten
  
  # n calcen/festlegen
  
  # datensatz splitten in da wos fehlt und da wo nicht
  mis <- is.na(data1$X3)
  data_y_mis <- data1[mis,]
  data_y_obs <- data1[!mis,]
  
  
  
  for (i in 1:maximp) {
    
    # https://stefvanbuuren.name/fimd/sec-cart.html 
    
    #Draw a bootstrap sample (˙yobs,˙Xobs) of size n1 from (yobs,Xobs).
    sample(size = n, replace=T)
    
    #Fit ˙yobs by ˙Xobs by a tree model f(X).

    
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
      #Predict the n0 terminal nodes gj from f(Xmis).
      nodes <- predict(object = fit, newdata = xmis)
      #Construct n0 sets Zj of all cases at node gj, each containing dj candidate donors.
      impute <- apply(nodes, MARGIN = 1, FUN = function(s) {
        sample(colnames(nodes), size = 1, prob = s)
      })
    }
    
    #Predict the n0 terminal nodes gj from f(Xmis).
    #Construct n0 sets Zj of all cases at node gj, each containing dj candidate donors.
    #Draw one donor ij from Zj randomly for j=1,…,n0
    #Calculate imputations ˙yj=yij for j=1,…,n0. 
    
    
    i <- i + 1
    
  }
  
}

mis <- is.na(data1$X3)
data_y_mis <- data1[mis,]
data_y_obs <- data1[!mis,]

  
fit <- rpart::rpart(X3 ~ ., data = data_y_obs, 
                    method = "anova", control = rpart::rpart.control(minbucket = 5, 
                                                                     cp = 1e-04))
leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, ])))

fit$frame$yval <- as.numeric(row.names(fit$frame))     #? wofür brauchen wir das überhaupt?
nodes <- predict(object = fit, newdata = data_y_mis)   # predictions für die, wo y fehlt
donor <- lapply(nodes, function(s) data_y_obs$X3[leafnr == s]) #liste mit donors für alle missing ys
impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 
                                                      1), numeric(1))
