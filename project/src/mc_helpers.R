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
