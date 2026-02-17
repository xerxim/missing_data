### Data Generator

generate_data_nonlinear_weak <- function(n, seed = NA) {

  if(!is.na(seed)) set.seed(seed)
  
  X1 <- rnorm(n, 8, 3)
  X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
  # Variablen centern, damit der Interaktionseffekt kontrollierbarer ist
  X1 <- X1 - mean(X1)
  X2 <- X2 - mean(X2)
  
  X3 <- 5 + 0.6 * X1 + 0.5 * X2 + 0.1 * X1*X2 + rnorm(n, 0, sqrt(2))
  
  as.data.frame(cbind(X1, X2, X3))
}

generate_data_nonlinear_moderate <- function(n, seed = NA) {
  
  if(!is.na(seed)) set.seed(seed)
  
  X1 <- rnorm(n, 8, 3)
  X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
  # Variablen centern, damit der Interaktionseffekt kontrollierbarer ist
  X1 <- X1 - mean(X1)
  X2 <- X2 - mean(X2)
  
  X3 <- 5 + 0.6 * X1 + 0.5 * X2 + 0.25 * X1*X2 + rnorm(n, 0, sqrt(2))
  
  as.data.frame(cbind(X1, X2, X3))
}


generate_data_nonlinear_strong <- function(n, seed = NA) {
  
  if(!is.na(seed)) set.seed(seed)
  
  X1 <- rnorm(n, 8, 3)
  X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
  # Variablen centern, damit der Interaktionseffekt kontrollierbarer ist
  X1 <- X1 - mean(X1)
  X2 <- X2 - mean(X2)
  
  X3 <- 5 + 0.6 * X1 + 0.5 * X2 + 0.45 * X1*X2 + rnorm(n, 0, sqrt(2))
  
  as.data.frame(cbind(X1, X2, X3))
}


### Anwendung

nonlinear_weak_interaction <- mc$mc_study(
  methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
  true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
  data_generator = generate_data_nonlinear_weak,
  n = 500, cycles = 100, miss_vars = "X3", true_means = c("X3" = 4.55),
  miss = "MCAR", miss_rates =  0.3, miss_aux = NULL, seed = NA
)

nonlinear_moderate_interaction <- mc$mc_study(
  methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
  true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.25), 
  data_generator = generate_data_nonlinear_moderate,
  n = 500, cycles = 100, miss_vars = "X3", true_means = c("X3" = 3.875),
  miss = "MCAR", miss_rates =  0.3, miss_aux = NULL, seed = NA
)

nonlinear_strong_interaction <- mc$mc_study(
  methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
  true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.45), 
  data_generator = generate_data_nonlinear_strong,
  n = 500, cycles = 100, miss_vars = "X3", true_means = c("X3" = 2.975),
  miss = "MCAR", miss_rates =  0.3, miss_aux = NULL, seed = NA
)


### Plotten

bias_boxplot(nonlinear_weak_interaction,
             xticks =  c("β0", "β1", "β2", "β1*β2", "μ(X3)"),
             title = "Schwacher Interaktionseffekt")


bias_boxplot(nonlinear_moderate_interaction,
             xticks =  c("β0", "β1", "β2", "β1*β2", "μ(X3)"),
             title = "Medium Interaktionseffekt")


bias_boxplot(nonlinear_strong_interaction,
             xticks =  c("β0", "β1", "β2", "β1*β2", "μ(X3)"),
             title = "Starker Interaktionseffekt")

save(nonlinear_weak_interaction, nonlinear_moderate_interaction, nonlinear_strong_interaction,
     file = "project/dev/alice/interaction_mcar.RData")



### ohne pmm

#nonlinear_moderate_interaction_nopmm <- nonlinear_moderate_interaction %>%
#  filter(method != "pmm")

#bias_boxplot(nonlinear_moderate_interaction_nopmm,
#             xticks =  c("β0", "β1", "β2", "β1*β2", "μ(X3)"),
#             title = "Starker Interaktionseffekt")

 