# Function to combine items from National and Milwaukee datasets
combine_items <- function(national_vars, milwaukee_vars) {
  
  # Ensure input vectors are the same length
  if (length(national_vars) != length(milwaukee_vars)) {
    stop("National and Milwaukee variable lists must have the same length.")
  }
  
  # Extract the first two characters of the National variable name
  national_prefix <- substr(national_vars, 1, 2)  # Gets "B1" or "C1" from "B1SE1E" or "C1ASH1E"
  
  # Merge National and Milwaukee variables
  merged_items <- mapply(function(nat, mke) {
    ifelse(!is.na(d2[, nat]), d2[, nat], d2[, mke])  # Use National if available, otherwise use Milwaukee
  }, national_vars, milwaukee_vars, SIMPLIFY = FALSE)
  
  # Convert list to dataframe
  merged_items <- as.data.frame(merged_items)
  
  # Generate new column names dynamically based on the first two letters of the National variable
  new_colnames <- paste0(national_prefix, milwaukee_vars)  
  colnames(merged_items) <- new_colnames
  
  # Handle missing values (keep NA if both National and Milwaukee were NA)
  merged_items <- merged_items %>%
    mutate_all(~ ifelse(. %in% c(8, -1), NA, .)) %>%
    mutate_all(as.numeric)
  
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
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
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
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
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
  
  # Print the plot
  print(plot)
  
  # Save the plot if a save path is provided
  if (!is.null(save_path)) {
    ggsave(save_path, plot = plot, width = 10, height = 8, dpi = 300)
  }
  
  return(plot)
}
