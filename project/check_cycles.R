# A small cycle analysis.

# Imports:
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
plots <- new.env()
sys.source("project/dev/Alice/graph_functions.R", envir = plots)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.
dta_path <- "project/dta/cycle_study.RData"
# Activate multithreading.
future::plan(future::multisession, workers = future::availableCores()) 
# Activate progress bar.
progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_cli(
  format = "{cli::pb_bar} {cli::pb_percent} | ETA {cli::pb_eta}"
))

# Generate only once, then load from dta.
generate_data <- FALSE
cycles <- c(10, 50, 100, 200, 300, 500, 1000, 2000, 3000)
if(generate_data) {
  out <- vector("list", length(cycles))
  for(i in 1:length(cycles)) {
    cy <- cycles[[i]]
    # Make mc study.
    print(glue::glue("Study {i}/{length(cycles)} - cycles = {cycles[[i]]}:"))
    df <- mc$mc_study_furrr(
      methods = c("cart", "pmm", "cart_boot"), m = 30, formula = "X3 ~ X1 + X2",
      true_vals = c("(Intercept)"=5, "X1"=0.6, "X2"=0.5), n = 500, cycles = cy,
       miss_vars = c("X1", "X2", "X3"), true_means = c("X3" = 12.8),
      miss = c("MCAR", "MCAR", "MAR"), miss_rates = c(0.2, 0.3, 0.3),
       miss_aux = c(NULL, NULL, "X2"), seed = 161
      )
    # Fill list.
    out[[i]] <- df
    print("Finished Study! Saving...")
  }
  # Name list.
  names(out) <- cycles
  # Save list.
  save(out, file = dta_path)
  print(glue::glue("Saved in {dta_path}"))
} else {
  load(dta_path)
}

# Combine to single df.
df_all <- purrr::map2_dfr(out, cycles, ~ dplyr::mutate(.x, cycles = .y))
# Plot bias.
bias_plot <- plots$bias_boxplot(df_all, title = "Relative Bias") +
  ggplot2::facet_wrap(~ cycles)
# Save plot.
ggplot2::ggsave(
  filename = glue::glue("{plot_path}cycle_study_bias.png"),
  plot = bias_plot,
  width = 20,
  height = 12,
  dpi = 300
)

# Get coverages.
g <- df_all |>
  dplyr::group_by(method, cycles, term) |>
  dplyr::summarise(coverage = mean(cover, na.rm = FALSE))
print(g,n = 50)

# Plot coverages.
coverage_plot <- ggplot2::ggplot(
  g,
  ggplot2::aes(x = cycles, y = coverage, color = method, group = method)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ term) +
  ggplot2::geom_hline(yintercept = 0.95, linetype = 2) +
  ggplot2::labs(
    title = "Coverage by cycles",
    x = "Number of cycles",
    y = "Coverage",
    color = "Method"
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_log10(
    breaks = sort(unique(g$cycles)),
    labels = sort(unique(g$cycles))
  )
# Save plot.
ggplot2::ggsave(
  filename = glue::glue("{plot_path}cycle_study_coverage.png"),
  plot = coverage_plot,
  width = 12,
  height = 6,
  dpi = 300
)
