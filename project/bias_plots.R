# Imports:
## Libraries.
pacman::p_load(patchwork,tidyverse, dplyr, cowplot, gridExtra)
## Eigener Code:
plots <- new.env()
sys.source("project/src/graph_functions.R", envir = plots)

#data loading
load("project/dta/main_data.RData")



# Tick labels

xticks_linear <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(mu[3])
)

#xticks_linear <- c("β0", "β1", "β2", "μ(X3)")

xticks_nonlinear <- c(
  expression(beta[0]),
  expression(beta[1]),
  expression(beta[2]),
  expression(beta[3]),
  expression(mu[3])
)

# die Nummer entspricht den gewünschten percentages, also 3 => 30% etc.
dfs_linear <- c(full_output[["1c"]][3], full_output[["1d"]][3],
                full_output[["1a"]][3], full_output[["1b"]][3] )

dfs_nonlinear <- c(full_output[["2c"]][3], full_output[["2d"]][3],
                   full_output[["2a"]][3], full_output[["2b"]][3])

#Muss potentiell angepasst werden, je nachdem wie dann die Reihenfolge ist:
subtitles_lin <- c("MAR, 30 % Missings in X3 (Linear)",
                   "MAR, Missings: X1 - 20%, X2 - 50%, X3 - 30% (Linear)",
                   "MCAR, 30% Missings in X3 (Linear)",
                   "MCAR, X1 - 20%, X2 - 50%, X3 - 30% (Linear)")

#subtitles_nonlin <- c("MAR, 30 % Missings in X3 (Nonlinear)",
#                   "MAR, Missings: X1 - 20%, X2 - 50%, X3 - 30% (Nonlinear)",
#                   "MCAR, 30% Missings in X3 (Nonlinear)",
#                   "MCAR, X1 - 20%, X2 - 50%, X3 - 30% (Nonlinear)")


subtitles_nonlin <- c("MCAR",
                   "MCAR with Chained Equations",
                   "MAR",
                   "MAR with Chained Equations")

subtitles_lin <- subtitles_nonlin

boxplots_nonlinear <- list()
boxplots_linear <- list()
i <- 1
j <- 1

for (df in dfs_linear) {
  boxplot <- plots$bias_boxplot(df, title = subtitles_lin[[i]],
                          xticks = xticks_linear,
                        ylim = c(-0.35,0.42))
  boxplots_linear[[i]] <- boxplot
  i <- i +1
}

for (df in dfs_nonlinear) {
  boxplot <- plots$bias_boxplot(df, title = subtitles_nonlin[[j]],
                          xticks = xticks_nonlinear,
                        ylim = c(-1,0.65))
  boxplots_nonlinear[[j]] <- boxplot
  j <- j +1
}


# optional: boxplots combinen

comb_boxplot_row_lin <- boxplots_linear[[1]] + boxplots_linear[[2]] +
  boxplots_linear[[3]] + boxplots_linear[[4]] +
  plot_layout(
    ncol = 4,
    guides = "collect",
    axis_titles = "collect"
  )

comb_boxplot_square_lin <- boxplots_linear[[1]] + boxplots_linear[[2]] +
  boxplots_linear[[3]] + boxplots_linear[[4]] +
  plot_layout(
    ncol = 2,
    guides = "collect",axis_titles = "collect"
  ) +plot_annotation(
    title = expression("30 % Missings in "* X[3]),
    theme = theme(
      plot.title = element_text(
        size = 20, face = "bold", hjust = 0.5
      )
    )
  )

comb_boxplot_square_lin

comb_boxplot_row_nonlin <- boxplots_nonlinear[[1]] + boxplots_nonlinear[[2]] +
  boxplots_nonlinear[[3]] + boxplots_nonlinear[[4]] +
  plot_layout(
    ncol = 4,
    guides = "collect",
    axis_titles = "collect"
  )

comb_boxplot_square_nonlin <- boxplots_nonlinear[[1]] + boxplots_nonlinear[[2]] +
  boxplots_nonlinear[[3]] + boxplots_nonlinear[[4]] +
  plot_layout(
    ncol = 2,
    guides = "collect",
    axis_titles = "collect"
  ) +plot_annotation(
    title = expression("30 % Missings in "* X[3]),
    theme = theme(
      plot.title = element_text(
        size = 20, face = "bold", hjust = 0.5
      )
    )
  )



comb_boxplot_square_nonlin

### Prinzipieller Code zum Speichern der kombinierten Plots:
#ggsave("project/plots/bias_row_lin.png", comb_boxplot_row_lin)

ggsave("project/plots/bias_square_lin.pdf", comb_boxplot_square_lin, width = 15, height = 10, limitsize = F)


#ggsave("project/plots/bias_row_nonlin.png", comb_boxplot_row_nonlin)
ggsave("project/plots/bias_square_nonlin.pdf", comb_boxplot_square_nonlin, width = 15, height = 10, limitsize = F)
# da muss aber vermutlich eh noch mit der Größe etc. experimentiert werden...
# ich finds fast angenehmer, die plots ausm plotpane zu exportieren, da sieht
# man die Größe besser - aber idk wie das bei Positron ist
#ggsave("project/plots/biasplots_square.png")



# speichern der einzelnen Plots

sapply(1:length(boxplots_linear), function(i) ggsave(
  filename = paste0("project/plots/biasplot_linear ",i,".png"), 
  plot = boxplots_nonlinear[[i]], 
  width = 15, height = 9
))

sapply(1:length(boxplots_nonlinear), function(i) ggsave(
  filename = paste0("project/plots/biasplot_nonlinear ",i,".png"), 
  plot = boxplots_nonlinear[[i]], 
  width = 15, height = 9
))





