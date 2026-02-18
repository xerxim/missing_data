check_args <- function(data, vars, methods, rates, aux = NULL, seed = NULL){

  # data must be a data.frame
  if(!is.data.frame(data))
    stop("`data` must be a data.frame.")


  # vars: column names to modify
  # must exist, no duplicates, at least one variable
  if(missing(vars) || length(vars) == 0)
    stop("`vars` must contain at least one column name.")

  if(anyDuplicated(vars))
    stop("Each variable in `vars` may appear only once.")

  if(!all(vars %in% names(data)))
    stop("All `vars` must exist in `data`.")

  p <- length(vars)


  # methods: length 1 or length(vars)
  # allowed values: MCAR, MAR, NMAR (case-insensitive)
  if(length(methods) == 1)
    methods <- rep(methods, p)

  if(length(methods) != p)
    stop("`methods` must have length 1 or length(vars).")

  methods <- tolower(methods)

  if(!all(methods %in% c("mcar","mar","nmar")))
    stop("`methods` must be one of: 'MCAR', 'MAR', 'NMAR' (case insensitive).")


  # rates: length 1 or length(vars), numeric in (0,1)
  if(length(rates) == 1)
    rates <- rep(rates, p)

  if(length(rates) != p)
    stop("`rates` must have length 1 or length(vars).")

  if(!is.numeric(rates) || any(rates <= 0 | rates >= 1))
    stop("`rates` must be numeric values strictly between 0 and 1.")


  # aux (auxiliary variables for MAR)
  # allowed: NULL, single value, or vector length(vars)
  # only required for variables with methods == MAR
  if(!is.null(aux)){

    if(length(aux) == 1)
      aux <- rep(aux, p)

    if(length(aux) != p)
      stop("`aux` must be NULL, length 1, or length(vars).")

    aux_check <- aux[methods == "mar"]

    if(length(aux_check) > 0){
      if(any(is.na(aux_check)))
        stop("For MAR variables, `aux` cannot be NA.")

      if(!all(aux_check %in% names(data)))
        stop("All auxiliary variables must exist in `data`.")
    }
  }


  # seed must be single numeric value if provided
  if(!is.na(seed)){
    if(length(seed) != 1 || !is.numeric(seed))
      stop("`seed` must be a single numeric value.")
  }


  # return cleaned arguments
  list(methods = methods, rates = rates, aux = aux)
}

make_missing <- function(
  data, vars, methods, rates, aux = NULL, seed = NA
) {
  # Check paras.
  args <- check_args(data, vars, methods, rates, aux, seed)
  methods <- args$methods
  rates <- args$rates
  aux <- args$aux
  # Set seed.
  if(!is.na(seed)) set.seed(seed)
  # Set constants.
  logit <- function(x) 1/(1+exp(-x))
  data_org <- data
  # Iterate over vars.
  for(i in seq_along(vars)) {
    # Set iteration vars.
    var <- vars[[i]]
    y <- data[[var]]
    method <- methods[[i]]
    rate <- rates[[i]]
    n <- length(y)
    alpha <- qlogis(rate)

    # MCAR.
    if(method == "mcar"){
      p <- rep(rate, n)
    }

    # MAR.
    if(method == "mar"){
      if(is.null(aux)) stop("Aux needs to be defined for MAR!")
      a <- aux[[i]]
      x <- scale(data_org[[a]])
      p <- logit(alpha + 1 * x)
    }

    # NMAR.
    if(method == "nmar"){
      y_std <- scale(y)
      p <- logit(alpha + 1 * y_std)
    }

    # Make missings.
    R <- rbinom(n, 1, p)
    data[R == 1, var] <- NA
  }
  data
}

generate_data <- function(n, seed = NA) {
  # Set seed.
  if(!is.na(seed)) set.seed(seed)
  
  # Create fixed variables.
  X1 <- rnorm(n, 8, 3)
  X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
  X3 <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))

  as.data.frame(cbind(X1, X2, X3))
}

### Data Generator nicht-lineare Daten

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



rename_coef_levels <- function(x) {
  # Rename coef names by rules:
  # - "(Intercept)" -> "b0"
  # - "x<d>" / "X<d>" -> "b<d>"
  # - "<letters><d>" (excluding x/X) -> "b(d-1)"
  # - Otherwise -> "beta(<original>)"

  z <- as.character(x)

  is_int <- stringr::str_detect(z, stringr::regex("^\\(intercept\\)$", ignore_case = TRUE))
  is_xd  <- stringr::str_detect(z, stringr::regex("^[x]\\d+$", ignore_case = TRUE))
  is_ad  <- stringr::str_detect(z, "^[A-Za-z]+\\d+$") & !is_xd

  out <- z
  out[is_int] <- "b0"
  out[is_xd]  <- glue::glue("b{as.integer(stringr::str_extract(z[is_xd], '\\\\d+'))}") |> as.character()
  out[is_ad]  <- glue::glue("b{as.integer(stringr::str_extract(z[is_ad], '\\\\d+')) - 1L}") |> as.character()

  idx <- !(is_int | is_xd | is_ad)
  out[idx] <- glue::glue("beta({z[idx]})") |> as.character()

  if (is.factor(x)) { levels(x) <- out; x } else out
}

# Function that feed on a vector with list names (each list should contain dfs that are an MC result) 
# and displays coverage plots - Tom

make_coverages_plot <- function(list_names,
                                  plot_names,
                                  miss_perc = c(10, 20, 30, 40, 50),
                                  row_labels = c("a","b","c","d"),
                                  xlab = expression("Missing % in " * X[3])) {
  
  
  # To store the final rows of plots (one per list in list_names)
  all_plots <- vector("list", length(list_names))
  
  # Loop over each list name
  for (L in seq_along(list_names)) {
    
    # Retrieve the actual list from the environment
    df_list <- get(list_names[L], envir = .GlobalEnv)
    
    # --- 1) Summaries per df in the list ---
    raw_list <- lapply(seq_along(df_list), function(k) {
      df_list[[k]] %>%
        group_by(method, term) %>%
        summarise(
          coverage = mean(as.numeric(cover), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(missperc = miss_perc[k])
    })
    
    # --- 2) Combine ---
    combined <- bind_rows(raw_list, .id = "source_id")
    
    # --- 3) Split by parameter ---
    param_list <- combined %>% group_split(term, .keep = TRUE)
    
    # --- 4) Build plots for this list ---
    plots <- vector("list", length(param_list))
    
    for (i in seq_along(param_list)) {
      plots[[i]] <- ggplot(param_list[[i]]) +
        geom_line(aes(missperc, coverage, color = method), alpha = 0.4) +
        geom_point(aes(missperc, coverage, color = method)) +
        geom_hline(yintercept = 0.9, linetype = 2) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(title = NULL, x = xlab, y = NULL, color = "Imputation Method") +
        theme_classic()

      #First row gets titles
      if(L == 1){
        plots[[i]] <- plots[[i]]+
          labs(title = plot_names[i] )
      }
    }
    
    # --- 5) First plot gets y-label ---
    plots[[1]] <- plots[[1]] + labs(y = "Coverage")
    
    # --- 6) Last plot gets right-side annotation ---
    last_idx <- length(plots)
    plots[[last_idx]] <- plots[[last_idx]] +
      scale_y_continuous(
        name = NULL,
        sec.axis = dup_axis(name = row_labels[L])
      ) +
      theme(
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right  = element_blank(),
        axis.title.y.right = element_text(margin = margin(l = 8))
      )
    
    # --- 7) Store in general plotlist
    
    all_plots[[L]] <- plots
  }
  
# --- 8) Combine all plots ---

  n_rows <- length(all_plots)
  n_cols <- unique(vapply(all_plots, length, integer(1)))

  # Flatten list-of-lists into a single list of plots (row-major order)
  flat_plots <- unlist(all_plots, recursive = FALSE)

  # Arrange as a grid: columns = number of parameters, rows = number of lists
  combined_plot <-
    wrap_plots(flat_plots, ncol = n_cols) +
    plot_layout(
      ncol = n_cols,
      nrow = n_rows,
      guides = "collect",
      axis_titles = "collect"
    ) &
    theme(legend.position = "bottom")

    
      return(combined_plot)
}