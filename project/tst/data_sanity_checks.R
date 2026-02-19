load("project/dta/main_data.RData")

# Check if coverages match real parameters.
i_parameters <- c(
  "mu(X3)" = 12.8, "b0" = 5, "b1" = 0.6, 
  "b2" = 0.5
)

checks <- c(TRUE)
for(i in 1:4){
  dfs <- full_output[[i]]
  # Every df in experiment.
  for(j in 1:length(dfs)){
    df <- dfs[[j]]
    # Every output parameter.
    for(para_name in names(i_parameters)){
      sub_df <- df[df$term == para_name, ]
      para <- i_parameters[[para_name]]
      # Calculate coverage.
      cover_new <- para >= sub_df$ci_l & para <= sub_df$ci_u
      # CMatc agaunst
      match <- cover_new == sub_df$cover
      checks <- c(checks, all(match))
      if(!all(match)){
        error = list(sub_df, para, cover_new, match)
        print(glue::glue("Not matching for: {names(full_output)[[i]]} {names(dfs)[[j]]} {para_name}"))
      }
    }
  } 
}

if(all(checks)){
  print("OK!")
}


ii_parameters <- c(
  "mu(X3)" = 3.875, "b0" = 5, "b1" = 0.6, 
  "b2" = 0.5, "beta(X1:X2)" = 0.25
)

checks <- c(TRUE)
for(i in 5:8){
  dfs <- full_output[[i]]
  # Every df in experiment.
  for(j in 1:length(dfs)){
    df <- dfs[[j]]
    # Every output parameter.
    for(para_name in names(ii_parameters)){
      sub_df <- df[df$term == para_name, ]
      para <- ii_parameters[[para_name]]
      # Calculate coverage.
      cover_new <- para >= sub_df$ci_l & para <= sub_df$ci_u
      # CMatc agaunst
      match <- cover_new == sub_df$cover
      checks <- c(checks, all(match))
      if(!all(match)){
        error = list(sub_df, para, cover_new, match)
        print(glue::glue("Not matching for: {names(full_output)[[i]]} {names(dfs)[[j]]} {para_name}"))
      }
    }
  } 
}

if(all(checks)){
  print("OK!")
}


# Check if singles == full.
