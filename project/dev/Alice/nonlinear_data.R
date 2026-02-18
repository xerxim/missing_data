
### Anwendung

dfs_mcar_X3_miss_nonlin <- list()
i <- 1

for (mis_rate in c(0.1, 0.2, 0.3, 0.4, 0.5)) {
  mc_cycles <- 5
  
  dfs_mcar_X3_miss_nonlin[[i]] <- mc$mc_study(
    methods = c("cart", "cart_boot", "pmm"), m = 30, formula = "X3 ~ X1*X2",
    true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X1:X2"=0.1), 
    data_generator = generate_data_nonlinear_weak,
    n = 500, cycles = mc_cycles, miss_vars = "X3", true_means = c("X3" = 4.55),
    miss = "MCAR", miss_rates =  0.3, miss_aux = NULL, seed = NA
  )
  
  i <- i+1
  
}

#coverage_plot_int(dfs_mcar_X3_miss_nonlin, miss_perc = c(10, 20, 30, 40, 50), 
              #plot_names = c("β0", "β1", "β2", "β1*β2", "μ(X3)"), xlab = "Missing in X3 (%)", 
              #ylab = "MCAR,\n only missings in X3")


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

 