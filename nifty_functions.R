# Flexible function to combine National and Milwaukee items from any dataset
combine_items <- function(national_vars, milwaukee_vars, data) {
  
  # Ensure input vectors are the same length
  if (length(national_vars) != length(milwaukee_vars)) {
    stop("National and Milwaukee variable lists must have the same length.")
  }
  
  # Extract the first two characters of the National variable name (e.g., "RA1")
  national_prefix <- substr(national_vars, 1, 3)
  
  # Merge National and Milwaukee variables
  merged_items <- mapply(function(nat, mke) {
    ifelse(!is.na(data[[nat]]), data[[nat]], data[[mke]])
  }, national_vars, milwaukee_vars, SIMPLIFY = FALSE)
  
  # Convert list to dataframe
  merged_items <- as.data.frame(merged_items)
  
  # Generate dynamic column names
  new_colnames <- paste0(national_prefix, milwaukee_vars)
  colnames(merged_items) <- new_colnames
  
  # Clean common missing codes
  merged_items <- merged_items %>%
    mutate(across(everything(), ~ ifelse(. %in% c(8, 98, -1, 999), NA, .))) %>%
    mutate(across(everything(), as.numeric))
  
  return(merged_items)
}



# Function to reverse code selected variables, allowing for both in-place and object assignment
reverse_code <- function(df, reverse_vars) {
  
  # If the input is a single variable (not a dataframe), reverse just that variable
  if (is.character(reverse_vars) && length(reverse_vars) == 1) {
    if (!(reverse_vars %in% colnames(df))) {
      stop(paste("Variable", reverse_vars, "not found in dataframe"))
    }
    x <- df[[reverse_vars]]
    if (all(is.na(x))) return(x)  # If all values are NA, return as is
    scale_max <- max(x, na.rm = TRUE)  
    scale_min <- min(x, na.rm = TRUE)  
    return(scale_max + scale_min - x)  # Reverse-code and return the single variable
  }
  
  # If multiple variables, check if they exist in the dataframe
  missing_vars <- setdiff(reverse_vars, colnames(df))
  if (length(missing_vars) > 0) {
    stop(paste("These variables are not in the dataframe:", paste(missing_vars, collapse = ", ")))
  }
  
  # Apply reverse coding to each specified variable in the dataframe
  df[, reverse_vars] <- lapply(df[, reverse_vars], function(x) {
    if (all(is.na(x))) return(x)  # If all values are NA, return as is
    scale_max <- max(x, na.rm = TRUE)  
    scale_min <- min(x, na.rm = TRUE)  
    scale_max + scale_min - x  # Apply reverse coding formula
  })
  
  return(df)  # Return the updated dataframe
}

# Function to compute alpha
compute_alpha <- function(df) {
  alpha_result <- alpha(df)
  print(alpha_result)
  return(alpha_result)
}

# Function to generate regression plots in the desired format
generate_regression_plot <- function(model, terms, title, xlab, ylab, color_mode = "color", save_path = NULL) {
  # Check the color mode
  if (color_mode == "bw") {
    # Black-and-white plot
    plot <- plot_model(
      model,
      type = "pred",
      terms = terms,
      colors = "bw"  # Black-and-white styling
    ) +
      theme_classic() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.key.size = unit(1, "cm")
      ) +
      labs(
        title = title,
        x = xlab,
        y = ylab,
        linetype = "Race"
      )
  } else {
    # Color plot
    plot <- plot_model(
      model,
      type = "pred",
      terms = terms
    ) +
      theme_classic() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.key.size = unit(1, "cm")
      ) +
      labs(
        title = title,
        x = xlab,
        y = ylab,
        color = "Race",
        linetype = "Race"
      )
  }
  
  # Save the plot if a save path is provided
  if (!is.null(save_path)) {
    ggsave(save_path, plot = plot, width = 10, height = 8, dpi = 300)
  }
  
  return(plot)
}

# Merge MIDUS data

merge_midus_data <- function(dfs,
                             join_key          = "M2ID",
                             coalesce_prefixes = NULL,
                             mke_flag          = TRUE) {
  # 1. Merge all data frames in the list using a full_join on the join_key
  merged <- reduce(dfs, full_join, by = join_key)
  
  # 2. Define default prefixes that always get coalesced
  #    and add any user-supplied prefixes
  default_prefixes <- c("SAMPLMAJ", "M2FAMNUM")
  all_prefixes <- unique(c(default_prefixes, coalesce_prefixes))
  
  # 3. For each prefix, coalesce all columns that start with it into one
  for (prefix in all_prefixes) {
    # Collect all columns that start with this prefix
    cols <- names(select(merged, starts_with(prefix)))
    
    # If no columns match this prefix, skip
    if (length(cols) < 1) next
    
    # Create a single coalesced column named exactly the prefix
    merged <- merged %>%
      mutate(
        "{prefix}" := coalesce(!!!syms(cols))
      ) %>%
      # Remove the duplicated columns, but keep the newly coalesced column
      select(-all_of(cols[cols != prefix]))
  }
  
  # 4. If mke_flag == TRUE, create the MKE variable (1 if SAMPLMAJ == 13, else 0)
  if (mke_flag) {
    merged <- merged %>%
      mutate(MKE = if_else(SAMPLMAJ == 13, 1, 0))
  }
  
  merged
}

run_lagged_model <- function(wb_outcome, wb_baseline, relig_var, data, 
                             interact_with = NULL, color_mode = "color") {
  # Build formula string
  if (!is.null(interact_with)) {
    formula_str <- paste0(wb_outcome, " ~ ", wb_baseline, " + age_m2 + female_m2 + eduBA_m2 + ",
                          "married_m2 + working_m2 + physhealth_m2 + ",
                          relig_var, " * ", interact_with)
  } else {
    formula_str <- paste0(wb_outcome, " ~ ", wb_baseline, " + age_m2 + female_m2 + eduBA_m2 + ",
                          "married_m2 + working_m2 + nhwb + physhealth_m2 + ", relig_var)
  }

  model_formula <- as.formula(formula_str)
  
  # Fit model
  model <- lm(model_formula, data = data)
  
  # Print summary
  print(summary(model))
  
  # Plot
  if (!is.null(interact_with)) {
    plot <- plot_model(model,
                       type = "int",
                       terms = c(relig_var, interact_with),
                       show.data = FALSE) +
      labs(x = gsub("_", " ", relig_var),
           y = toupper(wb_outcome),
           title = paste(toupper(wb_outcome), "by", relig_var, "×", interact_with)) +
      theme(legend.position = "bottom")
  } else {
    plot <- generate_regression_plot(model,
                                     terms = relig_var,
                                     title = paste(toupper(wb_outcome), "by", relig_var),
                                     xlab = gsub("_", " ", relig_var),
                                     ylab = toupper(wb_outcome),
                                     color_mode = color_mode)
  }
  
  print(plot)
  
  invisible(model)
}
