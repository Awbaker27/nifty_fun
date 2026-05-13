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
  df <- df %>% mutate(across(everything(), as.numeric))
  suppressWarnings({
    alpha_result <- psych::alpha(df, use = "pairwise.complete.obs", check.keys = FALSE)
  })
  print(alpha_result$total)
  invisible(alpha_result)
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
  summary_output <- summary(model)
  print(summary_output)
  
  # Get coefficients
  coefs <- coef(summary_output)
  
  # Identify term
  if (!is.null(interact_with)) {
    # Match interaction term robustly
    pattern <- paste0(relig_var, ":", interact_with, "|", interact_with, ":", relig_var)
    matched_terms <- grep(pattern, rownames(coefs), value = TRUE)
    term_name <- if (length(matched_terms) > 0) matched_terms[1] else NA
  } else {
    term_name <- relig_var
  }

  # Check if term exists and p-value is < 0.05
  if (!is.na(term_name) && term_name %in% rownames(coefs)) {
    pval <- coefs[term_name, "Pr(>|t|)"]
    if (!is.na(pval) && pval < 0.05) {
      # Generate plot
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
    } else {
      message("Key coefficient not significant; plot not generated.")
    }
  } else {
    message("Key term not found in coefficients; plot not generated.")
  }
  
  invisible(model)
}

#' Plot LPA/LTA profiles as a radar chart
#'
#' @param means   Data frame with at least: class_col, var_col, value_col.
#' @param indicators Character vector of variable names to include (in order).
#' @param class_col Name of the class column (default "Class").
#' @param var_col   Name of the indicator/variable column (default "variable").
#' @param value_col Name of the value column (default "z_mean").
#' @param indicator_labels Optional named vector for prettier axis labels.
#'                         Names = original variable names, values = labels.
#' @param palette   RColorBrewer palette name (default "Set2").
#' @param main      Plot title.
#' @param legend_pos Base R legend position (e.g., "topright").
#' @param rescale_01 Logical; if TRUE, rescale each indicator 0–1 across classes.
#'
#' @return Invisibly returns the data frame sent to radarchart().
#' @details Requires fmsb, RColorBrewer, dplyr, tidyr, scales.
plot_radar_profiles <- function(means,
                                indicators,
                                class_col   = "Class",
                                var_col     = "variable",
                                value_col   = "z_mean",
                                indicator_labels = NULL,
                                palette     = "Set2",
                                main        = NULL,
                                legend_pos  = "topright",
                                rescale_01  = TRUE) {

  # --- packages ---
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)
  requireNamespace("fmsb", quietly = TRUE)
  requireNamespace("RColorBrewer", quietly = TRUE)

  df <- means

  # Ensure class is ordered nicely
  df[[class_col]] <- as.numeric(df[[class_col]])
  K_current <- length(unique(df[[class_col]]))

  # Default title
  if (is.null(main)) {
    main <- paste0(K_current, "-Class Solution")
  }

  # Filter + reshape to wide: one row per class, columns = indicators
  df_wide <- df |>
    dplyr::filter(.data[[var_col]] %in% indicators) |>
    dplyr::select(
      Class    = dplyr::all_of(class_col),
      variable = dplyr::all_of(var_col),
      value    = dplyr::all_of(value_col)
    ) |>
    tidyr::pivot_wider(
      names_from  = "variable",
      values_from = "value"
    ) |>
    dplyr::arrange(Class)

  # Optionally rescale each indicator 0–1 across classes
  if (rescale_01) {
    df_scaled <- df_wide |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(indicators),
          ~ scales::rescale(.x, to = c(0, 1))
        )
      )
    max_row <- rep(1, length(indicators))
    min_row <- rep(0, length(indicators))
  } else {
    # Use raw scale: set max/min from data (nice if you want z-scale axes)
    vals    <- dplyr::select(df_wide, dplyr::all_of(indicators))
    max_row <- apply(vals, 2, max, na.rm = TRUE)
    min_row <- apply(vals, 2, min, na.rm = TRUE)
    df_scaled <- df_wide
  }

  radar_core <- df_scaled[, indicators, drop = FALSE]

  max_min <- rbind(max_row, min_row)
  colnames(max_min) <- indicators

  radar_data <- rbind(
    max_min,
    radar_core
  )

  # Build colors
  n_cols <- max(K_current, 3)
  pal_all <- RColorBrewer::brewer.pal(n_cols, palette)
  colors  <- pal_all[seq_len(K_current)]

  # Axis labels: optional pretty labels
  if (!is.null(indicator_labels)) {
    stopifnot(all(indicators %in% names(indicator_labels)))
    axis_labels <- unname(indicator_labels[indicators])
  } else {
    axis_labels <- indicators
  }

  # Plot
  op <- par(mfrow = c(1, 1), mar = c(1, 2, 3, 1))

  fmsb::radarchart(
    radar_data,
    axistype = if (rescale_01) 1 else 2,
    pcol     = colors,
    plwd     = 2,
    plty     = 1,
    cglcol   = "grey80",
    cglty    = 1,
    cglwd    = 0.8,
    vlcex    = 0.9,
    vlabels  = axis_labels,
    title    = main
  )

  legend(
    legend_pos,
    legend = paste0("Class ", df_wide$Class),
    col    = colors,
    lty    = 1,
    lwd    = 2,
    bty    = "n",
    cex    = 0.8
  )

  par(op)

  invisible(radar_data)
}



# MPLUS RELATED FUNCTIONS 
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Mplus syntax helpers ----------------------------------------------------
aux_lines_each <- function(vars, tag) {
  if (!length(vars)) return("")
  paste0("AUXILIARY = ", vars, " (", tag, ");", collapse = "\n")
}

wrap_list <- function(keyword, vec, end = ";", width = 78, indent = 2) {
  if (length(vec) == 0) return("")
  pad  <- paste0(strrep(" ", indent))
  line <- paste0(keyword, vec[1])
  out  <- character(0)

  for (v in vec[-1]) {
    cand <- paste(line, v)
    if (nchar(cand) > width) {
      out <- c(out, line)
      line <- paste0(pad, v)
    } else {
      line <- cand
    }
  }

  out <- c(out, paste0(line, " ", end))
  paste(out, collapse = "\n")
}

wrap_names_exact <- function(nms) wrap_list("NAMES ARE ", nms)
wrap_usevars     <- function(ind) wrap_list("USEVARIABLES ARE ", ind)

# ---- U-drive-safe Mplus runner ----------------------------------------------
run_one <- function(inp, exe = MPLUS_EXE, overwrite = FALSE) {
  out <- sub("\\.inp$", ".out", inp, ignore.case = TRUE)

  if (!overwrite && file.exists(out)) {
    message("Skipping (exists): ", basename(out))
    return(out)
  }

  wd   <- dirname(inp)
  cmd  <- "C:\\Windows\\System32\\cmd.exe"
  args <- c("/c", "cd", "/d", shQuote(wd), "&&", shQuote(exe), shQuote(basename(inp)))

  message("Running: ", paste(c(cmd, args), collapse = " "))
  status <- system2(command = cmd, args = args, stdout = TRUE, stderr = TRUE)

  if (!file.exists(out)) {
    warning("No .out created for: ", inp, "\nOutput:\n", paste(status, collapse = "\n"))
  }

  out
}

# ---- Basic text utilities ----------------------------------------------------
read_txt <- function(p) readLines(p, warn = FALSE)

norm_key <- function(s) {
  s |>
    toupper() |>
    gsub("[^A-Z0-9]+", "_", x = _) |>
    gsub("^_+|_+$", "", x = _)
}

make_name_map <- function(vars) {
  full <- toupper(gsub("[^A-Za-z0-9]+", "_", vars))
  tr8  <- substr(full, 1, 8)
  nou  <- gsub("_", "", full)

  keys <- c(full, tr8, nou)
  vals <- c(vars, vars, vars)

  setNames(vals, keys)
}

# ---- Minimal fit extraction --------------------------------------------------
extract_fit_text <- function(path) {
  txt <- read_txt(path)

  grab_num <- function(pattern, last = TRUE) {
    line <- grep(pattern, txt, value = TRUE, ignore.case = TRUE)
    if (!length(line)) return(NA_real_)
    nums <- stringr::str_extract_all(line[1], "-?\\d+\\.\\d+|-?\\d+")[[1]]
    if (!length(nums)) return(NA_real_)
    as.numeric(if (last) tail(nums, 1) else nums[1])
  }

  grab_p_after <- function(pattern) {
    idx <- grep(pattern, txt, ignore.case = TRUE)
    if (!length(idx)) return(NA_real_)

    window <- txt[idx[1]:min(length(txt), idx[1] + 12)]
    p_line <- grep("P-Value", window, value = TRUE, ignore.case = TRUE)
    if (!length(p_line)) return(NA_real_)

    nums <- stringr::str_extract_all(p_line[length(p_line)], "\\d+\\.\\d+")[[1]]
    if (!length(nums)) return(NA_real_)

    as.numeric(nums[1])
  }

  grab_blrt_p <- function() {
    tech14 <- grep("TECHNICAL 14 OUTPUT", txt, ignore.case = TRUE)
    if (!length(tech14)) return(NA_real_)

    seg <- txt[tech14[1]:length(txt)]
    p_line <- grep("Approximate P-Value", seg, value = TRUE, ignore.case = TRUE)
    if (!length(p_line)) return(NA_real_)

    nums <- stringr::str_extract_all(p_line[1], "\\d+\\.\\d+")[[1]]
    if (!length(nums)) return(NA_real_)

    as.numeric(nums[1])
  }

  grab_blrt_warning <- function() {
    tech14 <- grep("TECHNICAL 14 OUTPUT", txt, ignore.case = TRUE)
    if (!length(tech14)) return(FALSE)

    seg <- txt[tech14[1]:length(txt)]

    any(grepl(
      "P-VALUE MAY NOT BE TRUSTWORTHY|LOCAL MAXIMA|NOT A REPLICATED BEST LOGLIKELIHOOD",
      seg,
      ignore.case = TRUE
    ))
  }

  tibble(
    file_type = if_else(grepl("BCH", basename(path), ignore.case = TRUE), "BCH", "DCAT"),
    Classes = as.integer(stringr::str_match(basename(path), "(?i)k(\\d+)")[, 2]),
    observations = grab_num("^\\s*Number of observations"),
    ll = grab_num("^\\s*H0 Value"),
    aic = grab_num("^\\s*Akaike"),
    bic = grab_num("^\\s*Bayesian"),
    abic = grab_num("Sample-Size Adjusted BIC"),
    entropy = grab_num("^\\s*Entropy"),
    lmr_p = grab_p_after("Lo-Mendell-Rubin Adjusted LRT Test"),
    blrt_p = grab_blrt_p(),
    blrt_warning = grab_blrt_warning(),
    filename = path
  )
}

# ---- BCH parser --------------------------------------------------------------
get_bch_tables <- function(out_path, cont_vars) {
  txt <- read_txt(out_path)

  b1 <- grep(
    "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES|EQUALITY TESTS OF MEANS ACROSS CLASSES",
    txt,
    ignore.case = TRUE
  )

  empty <- list(
    means = tibble(Variable = character(), Class = integer(), Mean = numeric(), SE = numeric()),
    tests = tibble(Variable = character(), Contrast = character(), ChiSq = numeric(), P = numeric(), df = integer(), Kind = character())
  )

  if (!length(b1)) return(empty)

  seg <- txt[b1[1]:length(txt)]

  nmap <- make_name_map(cont_vars)

  anchor_idx <- which(vapply(
    trimws(seg),
    function(x) norm_key(x) %in% names(nmap),
    logical(1)
  ))

  if (!length(anchor_idx)) return(empty)

  blocks <- purrr::map2(
    anchor_idx,
    c(anchor_idx[-1] - 1, length(seg)),
    ~ list(
      var_full = unname(nmap[[norm_key(seg[.x])]]),
      text = seg[.x:.y]
    )
  )

  parse_means <- function(block) {
    ln <- block$text

    rx2 <- "^\\s*Class\\s*(\\d+)\\s+([-\\.0-9]+)\\s+([-\\.0-9]+)\\s+Class\\s*(\\d+)\\s+([-\\.0-9]+)\\s+([-\\.0-9]+)"
    rx1 <- "^\\s*Class\\s*(\\d+)\\s+([-\\.0-9]+)\\s+([-\\.0-9]+)\\s*$"

    two <- str_match(ln, rx2)
    one <- str_match(ln, rx1)

    pieces <- list()

    if (any(!is.na(two[, 1]))) {
      two <- two[!is.na(two[, 1]), , drop = FALSE]
      pieces <- c(pieces, list(
        tibble(Class = as.integer(two[, 2]), Mean = as.numeric(two[, 3]), SE = as.numeric(two[, 4])),
        tibble(Class = as.integer(two[, 5]), Mean = as.numeric(two[, 6]), SE = as.numeric(two[, 7]))
      ))
    }

    if (any(!is.na(one[, 1]))) {
      one <- one[!is.na(one[, 1]), , drop = FALSE]
      pieces <- c(pieces, list(
        tibble(Class = as.integer(one[, 2]), Mean = as.numeric(one[, 3]), SE = as.numeric(one[, 4]))
      ))
    }

    if (!length(pieces)) return(NULL)

    bind_rows(pieces) %>%
      distinct() %>%
      arrange(Class) %>%
      mutate(Variable = block$var_full, .before = 1)
  }

  parse_tests <- function(block) {
    ln <- block$text

    rx <- "^\\s*(Overall test|Class\\s*\\d+\\s*vs\\.\\s*\\d+)\\s+([-\\.0-9]+)\\s+([-\\.0-9]+)(?:\\s+(\\d+))?"
    m <- str_match(ln, rx)
    m <- m[!is.na(m[, 1]), , drop = FALSE]

    if (!nrow(m)) return(NULL)

    tibble(
  Variable = block$var_full,
  Contrast = str_trim(m[, 2]),
  ChiSq = as.numeric(m[, 3]),
  P = as.numeric(m[, 4]),
  df = suppressWarnings(as.integer(m[, 5])),
  Kind = if_else(grepl("^Overall", Contrast, ignore.case = TRUE), "overall", "pairwise")
) %>%
  arrange(Variable, Contrast, desc(!is.na(df))) %>%
  distinct(Variable, Contrast, .keep_all = TRUE)
  }

  list(
    means = map_dfr(blocks, parse_means),
    tests = map_dfr(blocks, parse_tests)
  )
}

# ---- DCAT parser -------------------------------------------------------------
get_dcat_tables <- function(out_path, cat_vars) {
  txt <- read_txt(out_path)

  d1 <- grep("EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES", txt, ignore.case = TRUE)
  if (!length(d1)) {
    return(list(probs = tibble(), tests = tibble()))
  }

  seg <- txt[d1[1]:length(txt)]

  nmap <- make_name_map(cat_vars)
  all_caps_idx <- which(grepl("^[A-Z][A-Z0-9_]*\\s*$", trimws(seg)))
  keep_idx <- all_caps_idx[vapply(
    all_caps_idx,
    \(i) norm_key(seg[i]) %in% names(nmap),
    logical(1)
  )]

  if (!length(keep_idx)) {
    return(list(probs = tibble(), tests = tibble()))
  }

  blocks <- purrr::map2(
    keep_idx,
    c(keep_idx[-1] - 1, length(seg)),
    ~ list(
      var_full = unname(nmap[[norm_key(seg[.x])]]),
      text     = seg[.x:.y]
    )
  )

  parse_dcat_probs <- function(block) {
    ln <- block$text

    head_idx <- grep("\\bProb\\b\\s+S\\.E\\.", ln, ignore.case = TRUE)[1]
    if (is.na(head_idx)) return(NULL)

    chi_idx <- grep("\\bChi-Square\\b\\s+\\bP-Value\\b", ln, ignore.case = TRUE)
    stop_idx <- if (length(chi_idx)) chi_idx[1] - 1 else min(length(ln), head_idx + 200)

    seg2 <- ln[(head_idx + 1):stop_idx]

    out <- list()
    cur_class <- NA_integer_

    for (row in seg2) {
      mc <- stringr::str_match(row, "^\\s*Class\\s*(\\d+)\\s*$")
      if (!all(is.na(mc))) {
        cur_class <- as.integer(mc[, 2])
        next
      }

      m <- stringr::str_match(
        row,
        "^\\s*Category\\s*(\\d+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)"
      )

      if (!all(is.na(m)) && !is.na(cur_class)) {
        out[[length(out) + 1]] <- tibble(
          Variable = block$var_full,
          Class    = cur_class,
          Category = as.integer(m[, 2]),
          Prob     = as.numeric(m[, 3]),
          SE       = as.numeric(m[, 4]),
          OR       = as.numeric(m[, 5]),
          OR_SE    = as.numeric(m[, 6]),
          CI_low   = as.numeric(m[, 7]),
          CI_high  = as.numeric(m[, 8])
        )
      }
    }

    if (!length(out)) return(NULL)
    bind_rows(out)
  }

  parse_dcat_tests <- function(block) {
    ln <- block$text

    chi_idx <- grep("\\bChi-Square\\b\\s+\\bP-Value\\b", ln, ignore.case = TRUE)
    if (!length(chi_idx)) return(NULL)

    seg2 <- ln[(chi_idx[1] + 1):min(length(ln), chi_idx[1] + 80)]

    m <- stringr::str_match(
      seg2,
      "^\\s*(Overall test|Class\\s*\\d+\\s*vs\\.\\s*\\d+)\\s+([-.0-9]+)\\s+([-.0-9]+)\\s+(\\d+)\\s*$"
    )
    m <- m[!is.na(m[, 1]), , drop = FALSE]

    if (!nrow(m)) return(NULL)

    tibble(
      Variable = block$var_full,
      Contrast = stringr::str_trim(m[, 2]),
      ChiSq    = as.numeric(m[, 3]),
      P        = as.numeric(m[, 4]),
      df       = as.integer(m[, 5])
    ) |>
      mutate(Kind = if_else(grepl("^Overall", Contrast, ignore.case = TRUE), "overall", "pairwise"))
  }

  list(
    probs = map_dfr(blocks, parse_dcat_probs),
    tests = map_dfr(blocks, parse_dcat_tests)
  )
}
