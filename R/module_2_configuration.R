# ============================================================================
# Module 2: Configuration Module
# ============================================================================
# Purpose: Define and configure item ordering methods, reduction methods,
#          parameters, and generate valid method combinations
# 
# Note: Ordering methods behave differently based on questionnaire type:
#   - Unidimensional: Methods operate globally across all items
#   - Multi-construct: Methods operate within each construct separately
#     (except "domain_blocked" which also optimizes construct order)
# ============================================================================

#' Define Available Item Ordering Methods
#'
#' @return A list containing ordering method definitions and parameters
#' @export
define_ordering_methods <- function() {
  list(
    original = list(
      name = "original",
      display_name = "Original Order",
      description = "Preserve questionnaire's native item sequence",
      requires_training = FALSE,
      parameters = list()
    ),
    
    auc = list(
      name = "auc",
      display_name = "Item-level AUC",
      description = "Order by AUC: globally for unidimensional, within-construct for multi-construct",
      requires_training = TRUE,
      parameters = list()
    ),
    
    incremental_auc = list(
      name = "incremental_auc",
      display_name = "Incremental AUC",
      description = "Order by additional predictive value: globally for unidimensional, within-construct for multi-construct",
      requires_training = TRUE,
      parameters = list()
    ),
    
    correlation = list(
      name = "correlation",
      display_name = "Correlation-based",
      description = "Order by correlation: globally for unidimensional, within-construct for multi-construct",
      requires_training = TRUE,
      parameters = list()
    ),
    
    forward_stepwise = list(
      name = "forward_stepwise",
      display_name = "Forward Stepwise Regression",
      description = "Select by partial correlation: globally for unidimensional, within-construct for multi-construct",
      requires_training = TRUE,
      parameters = list()
    ),
    
    irt = list(
      name = "irt",
      display_name = "IRT-based Ordering",
      description = "Order by information function: globally for unidimensional, within-construct for multi-construct",
      requires_training = TRUE,
      parameters = list(
        irt_model = "graded"  # graded response model by default # Fix: (Changed "grm" to "graded")
      )
    ),
    
    domain_blocked = list(
      name = "domain_blocked",
      display_name = "Domain-blocked Ordering",
      description = "Optimize construct presentation order, then items within each construct (multi-construct only)",
      requires_training = TRUE,
      parameters = list(
        within_block_method = "auc",  # method to use within blocks
        construct_order_method = "max_auc"  # how to order constructs
      )
    )
  )
}

#' Define Available Reduction Methods
#'
#' @return A list containing reduction method definitions and parameters
#' @export
define_reduction_methods <- function() {
  list(
    none = list(
      name = "none",
      display_name = "No Reduction",
      description = "Full questionnaire administration (baseline)",
      requires_training = FALSE,
      parameters = list()
    ),
    
    dc = list(
      name = "dc",
      display_name = "Deterministic Curtailment",
      description = "Stop when classification cannot change",
      requires_training = TRUE,
      parameters = list()
    ),
    
    sc_ep = list(
      name = "sc_ep",
      display_name = "Stochastic Curtailment - Empirical Proportions",
      description = "Stop based on empirical conditional probabilities",
      requires_training = TRUE,
      parameters = list(
        gamma_0 = 0.95,  # probability threshold for low-risk classification
        gamma_1 = 0.95   # probability threshold for high-risk classification
      )
    ),
    
    sc_sor = list(
      name = "sc_sor",
      display_name = "Stochastic Curtailment - Simple Ordinal Regression",
      description = "Stop based on regression model predictions",
      requires_training = TRUE,
      parameters = list(
        gamma_0 = 0.95,
        gamma_1 = 0.95
      )
    ),
    
    sc_mor = list(
      name = "sc_mor",
      display_name = "Stochastic Curtailment - Multiple Ordinal Regression",
      description = "Stop based on item-specific regression weights",
      requires_training = TRUE,
      parameters = list(
        gamma_0 = 0.95,
        gamma_1 = 0.95
      )
    ),
    
    irt_cct = list(
      name = "irt_cct",
      display_name = "IRT-based CCT",
      description = "Use latent trait estimation for classification",
      requires_training = TRUE,
      parameters = list(
        irt_model = "graded", # Fix: (Changed "grm" to "graded")
        se_threshold = 0.3  # standard error threshold
      )
    )
  )
}

#' Create Analysis Configuration
#'
#' @param ordering_methods Character vector of ordering methods to test: 
#'   c("original", "auc", "incremental_auc", "correlation", "forward_stepwise", "irt", "domain_blocked")
#' @param reduction_methods Character vector of reduction methods to test: 
#'   c("none", "dc", "sc_ep", "sc_sor", "sc_mor", "irt_cct")
#' @param two_step_mode Logical, whether to use two-step screening process
#' @param top_candidates Number of method combinations for detailed evaluation
#' @param screening_weights Named list with weights for composite score (w1_fnr, w2_accuracy, w3_efficiency)
#' @param utility_weights Named list with weights for final selection (alpha_fnr, beta_accuracy, gamma_efficiency)
#' @param cutoff_method Method for determining cutoff: "roc", "fixed", "percentile"
#' @param cutoff_value Fixed cutoff value (if cutoff_method = "fixed")
# #' @param fnr_threshold Maximum acceptable false negative rate
#' @param constraints List of constraints (stop_low_only, min_items_per_construct, etc.)
#' @param method_parameters List of method-specific parameter overrides
#' @return A validated configuration list
#' @export
create_analysis_config <- function(
    questionnaire_type = NULL,
    ordering_methods = c("original", "auc", "incremental_auc", "correlation", 
                         "forward_stepwise", "irt", "domain_blocked"),
    reduction_methods = c("none", "dc", "sc_ep", "sc_sor", "sc_mor", "irt_cct"),
    two_step_mode = TRUE,
    top_candidates = 10,
    screening_weights = list(w1_fnr = 0.5, w2_accuracy = 0.3, w3_efficiency = 0.2),
    utility_weights = list(alpha_fnr = 0.5, beta_accuracy = 0.3, gamma_efficiency = 0.2),
    cutoff_method = "roc",
    cutoff_value = NULL,
    # fnr_threshold = 0.05,
    constraints = list(
      stop_low_only = FALSE,
      min_items_per_construct = 1,
      complete_triggered_constructs = FALSE
    ),
    method_parameters = list()
) {
  
  # Get method definitions
  available_ordering <- define_ordering_methods()
  available_reduction <- define_reduction_methods()
  
  # Validate method selections
  invalid_ordering <- setdiff(ordering_methods, names(available_ordering))
  if (length(invalid_ordering) > 0) {
    stop(paste("Invalid ordering methods:", paste(invalid_ordering, collapse = ", ")))
  }
  
  invalid_reduction <- setdiff(reduction_methods, names(available_reduction))
  if (length(invalid_reduction) > 0) {
    stop(paste("Invalid reduction methods:", paste(invalid_reduction, collapse = ", ")))
  }
  
  # Validate weights sum to 1
  if (!isTRUE(all.equal(sum(unlist(screening_weights)), 1, tolerance = 0.001))) {
    warning("Screening weights do not sum to 1. Normalizing...")
    total <- sum(unlist(screening_weights))
    screening_weights <- lapply(screening_weights, function(x) x / total)
  }
  
  if (!isTRUE(all.equal(sum(unlist(utility_weights)), 1, tolerance = 0.001))) {
    warning("Utility weights do not sum to 1. Normalizing...")
    total <- sum(unlist(utility_weights))
    utility_weights <- lapply(utility_weights, function(x) x / total)
  }
  
  # # Validate cutoff configuration
  # if (cutoff_method == "fixed" && is.null(cutoff_value)) {
  #   stop("cutoff_value must be provided when cutoff_method = 'fixed'")
  # }
  # Fix:/Updated:
  # Validate cutoff configuration
  if (cutoff_method == "fixed") {
    if (is.null(questionnaire_type)) {
      warning("questionnaire_type not specified.",
              "cutoff_value must be provided when questionnaire_type = 'unidimensional' and cutoff_method = 'fixed'")
    } else if (questionnaire_type == "unidimensional" && is.null(cutoff_value)) {
      stop("cutoff_value must be provided when questionnaire_type = 'unidimensional' and cutoff_method = 'fixed'")
    } else if (questionnaire_type == "multi-construct" && !is.null(cutoff_value)) {
      warning("cutoff_value is ignored when questionnaire_type = 'multi-construct' and cutoff_method = 'fixed'",
              "Construct-specific cutoffs from data preparation module configuration (config$cutoffs) will be used.")
    }
  }
  
  # Create configuration object
  config <- list(
    questionnaire_type = questionnaire_type,
    ordering_methods = ordering_methods,
    reduction_methods = reduction_methods,
    two_step_mode = two_step_mode,
    top_candidates = top_candidates,
    screening_weights = screening_weights,
    utility_weights = utility_weights,
    cutoff_method = cutoff_method,
    cutoff_value = cutoff_value,
    # fnr_threshold = fnr_threshold,
    constraints = constraints,
    method_parameters = method_parameters,
    method_definitions = list(
      ordering = available_ordering[ordering_methods],
      reduction = available_reduction[reduction_methods]
    )
  )
  
  class(config) <- c("curtailment_config", "list")
  return(config)
}

#' Generate Method Combinations
#'
#' @param config A curtailment configuration object
#' @param gamma_values Optional list of gamma value combinations to test for SC methods
#' @return A data frame with all valid method combinations and their parameters
#' @export
generate_method_combinations <- function(config, gamma_values = NULL) {
  
  # Default gamma values if not provided
  if (is.null(gamma_values)) {
    gamma_values <- list(
      symmetric = list(
        list(gamma_0 = 0.90, gamma_1 = 0.90),
        list(gamma_0 = 0.95, gamma_1 = 0.95),
        list(gamma_0 = 0.99, gamma_1 = 0.99)
      ),
      asymmetric_conservative = list(
        list(gamma_0 = 0.90, gamma_1 = 0.99),
        list(gamma_0 = 0.95, gamma_1 = 0.99)
      )
    )
    gamma_values <- unlist(gamma_values, recursive = FALSE)
  }
  
  combinations <- expand.grid(
    ordering = config$ordering_methods,
    reduction = config$reduction_methods,
    stringsAsFactors = FALSE
  )
  
  # Expand combinations for methods with gamma parameters
  sc_methods <- c("sc_ep", "sc_sor", "sc_mor")
  expanded_combinations <- list()
  
  for (i in seq_len(nrow(combinations))) {
    row <- combinations[i, ]
    
    if (row$reduction %in% sc_methods) {
      # Create combinations for each gamma value set
      for (gamma_set in gamma_values) {
        combo <- row
        combo$gamma_0 <- gamma_set$gamma_0
        combo$gamma_1 <- gamma_set$gamma_1
        # combo$method_id <- sprintf("%s_%s_g0%.2f_g1%.2f", 
        #                            row$ordering, row$reduction,
        #                            gamma_set$gamma_0, gamma_set$gamma_1)
        # Fix: (Added underscore between g0/g1 and gamma_0/gamma_1 value  to improve readability)
        combo$method_id <- sprintf("%s_%s_g0_%.2f_g1_%.2f", 
                                   row$ordering, row$reduction,
                                   gamma_set$gamma_0, gamma_set$gamma_1)
        expanded_combinations[[length(expanded_combinations) + 1]] <- combo
      }
    } else {
      # Non-SC methods
      row$gamma_0 <- NA
      row$gamma_1 <- NA
      row$method_id <- sprintf("%s_%s", row$ordering, row$reduction)
      expanded_combinations[[length(expanded_combinations) + 1]] <- row
    }
  }
  
  # Convert to data frame
  combinations_df <- do.call(rbind, expanded_combinations)
  
  # Apply method-specific parameter overrides
  if (length(config$method_parameters) > 0) {
    for (method_id in names(config$method_parameters)) {
      idx <- combinations_df$method_id == method_id
      if (any(idx)) {
        params <- config$method_parameters[[method_id]]
        for (param_name in names(params)) {
          if (param_name %in% names(combinations_df)) {
            combinations_df[idx, param_name] <- params[[param_name]]
          }
        }
      }
    }
  }
  
  # Add display names
  combinations_df$ordering_display <- sapply(combinations_df$ordering, function(x) {
    config$method_definitions$ordering[[x]]$display_name
  })
  
  combinations_df$reduction_display <- sapply(combinations_df$reduction, function(x) {
    config$method_definitions$reduction[[x]]$display_name
  })
  
  # Add flags for special handling
  combinations_df$requires_training <- apply(combinations_df, 1, function(row) {
    ordering_req <- config$method_definitions$ordering[[row["ordering"]]]$requires_training
    reduction_req <- config$method_definitions$reduction[[row["reduction"]]]$requires_training
    ordering_req || reduction_req
  })
  
  # Fix:/Update: (Commented out to allow testing all gamma combinations even with stop_low_only)
  # # Apply constraints filter if stop_low_only is TRUE
  # if (config$constraints$stop_low_only) {
  #   # Remove rows where gamma_1 < 1 for SC methods
  #   sc_rows <- combinations_df$reduction %in% sc_methods
  #   combinations_df <- combinations_df[!sc_rows | combinations_df$gamma_1 == 1, ]
  # }
  
  return(combinations_df)
}

#' Validate Configuration Completeness
#'
#' @param config A curtailment configuration object
#' @param data_config The data configuration from Module 1
#' @return A list with validation results
#' @export
validate_configuration <- function(config, data_config = NULL) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  # Check if at least one ordering and reduction method selected
  if (length(config$ordering_methods) == 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   "At least one ordering method must be selected")
  }
  
  if (length(config$reduction_methods) == 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   "At least one reduction method must be selected")
  }
  
  # Check two-step configuration
  if (config$two_step_mode && config$top_candidates < 1) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   "top_candidates must be >= 1 when two_step_mode is TRUE")
  }
  
  # # Check FNR threshold
  # if (config$fnr_threshold < 0 || config$fnr_threshold > 1) {
  #   validation_results$valid <- FALSE
  #   validation_results$errors <- c(validation_results$errors,
  #                                  "fnr_threshold must be between 0 and 1")
  # }
  
  # Check constraints
  if (config$constraints$min_items_per_construct < 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   "min_items_per_construct must be >= 0")
  }
  
  # # Warnings for potentially suboptimal configurations
  # if (config$fnr_threshold > 0.1) {
  #   validation_results$warnings <- c(validation_results$warnings,
  #                                    "FNR threshold > 0.1 may lead to high misclassification of positive cases")
  # }
  
  if ("none" %in% config$reduction_methods && length(config$reduction_methods) == 1) {
    validation_results$warnings <- c(validation_results$warnings,
                                     "Only 'none' reduction selected - no curtailment will be performed")
  }
  
  # Check compatibility with data configuration if provided
  if (!is.null(data_config)) {
    if (data_config$questionnaire_type == "multi-construct" && 
        config$constraints$min_items_per_construct > 0) {
      # Check if minimum items constraint is feasible
      min_items_available <- min(sapply(data_config$constructs, length))
      if (config$constraints$min_items_per_construct > min_items_available) {
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors,
                                       sprintf("min_items_per_construct (%d) exceeds smallest construct size (%d)",
                                               config$constraints$min_items_per_construct,
                                               min_items_available))
      }
    }
    
    # Warn if domain_blocked is used with unidimensional questionnaire
    if (data_config$questionnaire_type == "unidimensional" && 
        "domain_blocked" %in% config$ordering_methods) {
      validation_results$warnings <- c(validation_results$warnings,
                                       "domain_blocked ordering is designed for multi-construct questionnaires and will behave like 'auc' for unidimensional data")
    }
    
    # Fix:/Update: (Added warning for base rate mismatch between training sample and reference sample)
    # Check for base rate mismatches
    if (!is.null(data_config$base_rates)) {
      sample_rates <- data_config$sample_base_rates
      ref_rates <- data_config$reference_base_rates
      
      if (!is.null(sample_rates) && !is.null(ref_rates)) {
        # Check for large discrepancies
        for (name in names(ref_rates)) {
          if (name %in% names(sample_rates)) {
            diff <- abs(ref_rates[[name]] - sample_rates[[name]])
            if (diff > 0.2) {  # 20% difference
              validation_results$warnings <- c(validation_results$warnings,
                                               sprintf("Large base rate difference for %s: sample=%.1f%%, reference=%.1f%%",
                                                       name, sample_rates[[name]]*100, ref_rates[[name]]*100))
            }
          }
        }
      }
    }
    
  }
  
  return(validation_results)
}

#' Print Configuration Summary
#'
#' @param x A curtailment configuration object
#' @param ... Additional arguments (not used)
#' @export
print.curtailment_config <- function(x, ...) {
  cat("----------------------------------\n")
  cat("Curtailment Analysis Configuration\n")
  cat("----------------------------------\n\n")
  
  cat("Ordering Methods:\n")
  for (method in x$ordering_methods) {
    cat(sprintf("  - %s: %s\n", 
                x$method_definitions$ordering[[method]]$display_name,
                x$method_definitions$ordering[[method]]$description))
  }
  
  cat("\nReduction Methods:\n")
  for (method in x$reduction_methods) {
    cat(sprintf("  - %s: %s\n", 
                x$method_definitions$reduction[[method]]$display_name,
                x$method_definitions$reduction[[method]]$description))
  }
  
  cat("\nTwo-Step Process:", ifelse(x$two_step_mode, "Enabled", "Disabled"), "\n")
  if (x$two_step_mode) {
    cat("  Top candidates for detailed analysis:", x$top_candidates, "\n")
    cat("  Screening weights:\n")
    cat(sprintf("    - FNR weight: %.2f\n", x$screening_weights$w1_fnr))
    cat(sprintf("    - Accuracy weight: %.2f\n", x$screening_weights$w2_accuracy))
    cat(sprintf("    - Efficiency weight: %.2f\n", x$screening_weights$w3_efficiency))
  }
  
  cat("\nUtility Function Weights:\n")
  cat(sprintf("  - FNR (alpha): %.2f\n", x$utility_weights$alpha_fnr))
  cat(sprintf("  - Accuracy (beta): %.2f\n", x$utility_weights$beta_accuracy))
  cat(sprintf("  - Efficiency (gamma): %.2f\n", x$utility_weights$gamma_efficiency))
  
  cat("\nCutoff Configuration:\n")
  cat("  Method:", x$cutoff_method, "\n")
  if (!is.null(x$cutoff_value)) {
    cat("  Value:", x$cutoff_value, "\n")
  }
  
  cat("\nConstraints:\n")
  # cat("  FNR threshold:", x$fnr_threshold, "\n")
  cat("  Stop-low only:", x$constraints$stop_low_only, "\n")
  cat("  Min items per construct:", x$constraints$min_items_per_construct, "\n")
  cat("  Complete triggered constructs:", x$constraints$complete_triggered_constructs, "\n")
  
  invisible(x)
}

#' Get Method Parameter Grid for Optimization
#'
#' @param method_name Name of the method (ordering or reduction)
#' @param method_type Type of method ("ordering" or "reduction")
#' @param grid_density Density of parameter grid ("coarse", "medium", "fine")
#' @return A list of parameter combinations to test
#' @export
get_parameter_grid <- function(method_name, method_type, grid_density = "medium") {
  
  grids <- list(
    coarse = list(
      gamma = c(0.90, 0.95, 0.99),
      se_threshold = c(0.2, 0.3, 0.4)
    ),
    medium = list(
      gamma = c(0.85, 0.90, 0.925, 0.95, 0.975, 0.99),
      se_threshold = c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
    ),
    fine = list(
      gamma = seq(0.80, 0.99, by = 0.01),
      se_threshold = seq(0.1, 0.5, by = 0.05)
    )
  )
  
  grid_values <- grids[[grid_density]]
  
  # Method-specific parameter grids
  if (method_name %in% c("sc_ep", "sc_sor", "sc_mor")) {
    # Create all combinations of gamma_0 and gamma_1
    gamma_combos <- expand.grid(
      gamma_0 = grid_values$gamma,
      gamma_1 = grid_values$gamma
    )
    # Convert to list format
    param_grid <- split(gamma_combos, seq(nrow(gamma_combos)))
  } else if (method_name == "irt_cct") {
    param_grid <- lapply(grid_values$se_threshold, function(x) list(se_threshold = x))
  } else {
    param_grid <- list(list())  # No parameters
  }
  
  return(param_grid)
}



#' Check if constructs have adequate items and samples for regression methods
#'
#' @param data_config Data configuration with construct definitions
#' @param n_samples Number of training samples
#' @param method Reduction method ("sc_sor" or "sc_mor")
#' @return Data frame with adequacy assessment
#' @export
check_construct_adequacy <- function(data_config, n_samples, method = "sc_sor") {
  
  results <- data.frame(
    construct = character(),
    n_items = integer(),
    n_samples = integer(),
    samples_per_item = numeric(),
    adequacy = character(),
    recommendation = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate samples per construct (assuming even distribution)
  n_constructs <- length(data_config$constructs)
  samples_per_construct <- n_samples / n_constructs
  
  for (construct_name in names(data_config$constructs)) {
    n_items <- length(data_config$constructs[[construct_name]])
    
    if (method == "sc_sor") {
      # Simple regression - need reasonable sum score variability
      if (n_items < 3) {
        adequacy <- "Poor"
        recommendation <- "Too few items for meaningful curtailment"
      } else if (n_items < 6) {
        adequacy <- "Marginal"
        recommendation <- "May work but consider sc_ep or dc methods"
      } else if (n_items < 10) {
        adequacy <- "Good"
        recommendation <- "Well-suited for sc_sor"
      } else {
        adequacy <- "Excellent"
        recommendation <- "Ideal for sc_sor"
      }
      
    } else if (method == "sc_mor") {
      # Multiple regression - need adequate samples per predictor
      samples_per_item <- samples_per_construct / n_items
      
      if (samples_per_item < 5) {
        adequacy <- "Poor"
        recommendation <- "Insufficient samples; use sc_sor or sc_ep"
      } else if (samples_per_item < 10) {
        adequacy <- "Marginal"
        recommendation <- "May have convergence issues; monitor carefully"
      } else if (samples_per_item < 20) {
        adequacy <- "Good"
        recommendation <- "Suitable for sc_mor with stable estimates"
      } else {
        adequacy <- "Excellent"
        recommendation <- "Well-suited for sc_mor"
      }
    }
    
    results <- rbind(results, data.frame(
      construct = construct_name,
      n_items = n_items,
      n_samples = round(samples_per_construct),
      samples_per_item = round(samples_per_construct / n_items, 1),
      adequacy = adequacy,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' Assess regression method suitability for the dataset
#'
#' @param analysis_config Analysis configuration from create_analysis_config
#' @param data_config Data configuration with construct definitions
#' @param n_samples Number of training samples
#' @return List with assessment results and recommendations
#' @export
assess_regression_methods <- function(analysis_config, data_config, n_samples) {
  
  # Check if regression methods are being used
  regression_methods <- c("sc_sor", "sc_mor")
  used_regression <- intersect(analysis_config$reduction_methods, regression_methods)
  
  if (length(used_regression) == 0) {
    return(list(
      uses_regression = FALSE,
      assessment = NULL,
      summary = "\nNo regression-based reduction methods selected.\n"
    ))
  }
  
  # Perform adequacy assessment for each method
  assessments <- list()
  for (method in used_regression) {
    assessments[[method]] <- check_construct_adequacy(data_config, n_samples, method)
  }
  
  # Generate summary statistics
  construct_sizes <- sapply(data_config$constructs, length)
  min_items <- min(construct_sizes)
  max_items <- max(construct_sizes)
  avg_items <- mean(construct_sizes)
  samples_per_construct <- n_samples / length(data_config$constructs)
  
  # Overall recommendations
  recommendations <- character()
  warnings <- character()
  
  if ("sc_sor" %in% used_regression) {
    sor_adequacy <- assessments[["sc_sor"]]$adequacy
    poor_constructs <- sum(sor_adequacy == "Poor")
    marginal_constructs <- sum(sor_adequacy == "Marginal")
    
    if (poor_constructs > 0) {
      warnings <- c(warnings, sprintf("%d constructs have too few items for reliable sc_sor", poor_constructs))
    }
    if (marginal_constructs > 0) {
      recommendations <- c(recommendations, sprintf("%d constructs are marginal for sc_sor - monitor performance", marginal_constructs))
    }
  }
  
  if ("sc_mor" %in% used_regression) {
    mor_adequacy <- assessments[["sc_mor"]]$adequacy
    poor_constructs <- sum(mor_adequacy == "Poor")
    marginal_constructs <- sum(mor_adequacy == "Marginal")
    
    if (poor_constructs > 0) {
      warnings <- c(warnings, sprintf("%d constructs have insufficient samples for sc_mor", poor_constructs))
      recommendations <- c(recommendations, "Consider removing sc_mor or increasing training data")
    }
    if (marginal_constructs > 0) {
      recommendations <- c(recommendations, sprintf("%d constructs are marginal for sc_mor - expect some convergence warnings", marginal_constructs))
    }
  }
  
  return(list(
    uses_regression = TRUE,
    assessments = assessments,
    summary = list(
      min_items = min_items,
      avg_items = avg_items,
      max_items = max_items,
      n_samples = n_samples,
      samples_per_construct = round(samples_per_construct, 1),
      avg_samples_per_item = round(samples_per_construct / avg_items, 1)
    ),
    recommendations = recommendations,
    warnings = warnings
  ))
}

#' Print regression method assessment results
#'
#' @param assessment Results from assess_regression_methods
#' @param verbose Whether to print detailed construct-level results
#' @export
print_regression_assessment <- function(assessment, verbose = FALSE) {
  if (!assessment$uses_regression) {
    cat(assessment$summary, "\n")
    return(invisible())
  }
  
  cat("\n----------------------------------------\n")
  cat("Regression Method Suitability Assessment\n")
  cat("----------------------------------------\n\n")
  
  # Summary statistics
  cat(sprintf("Dataset characteristics:\n"))
  cat(sprintf("  - Items per construct: %d to %d (avg: %.1f)\n", 
              assessment$summary$min_items, 
              assessment$summary$max_items, 
              assessment$summary$avg_items))
  cat(sprintf("  - Training samples: %d total\n", assessment$summary$n_samples))
  cat(sprintf("  - Samples per construct: ~%.0f\n", assessment$summary$samples_per_construct))
  cat(sprintf("  - Samples per item: ~%.0f\n", assessment$summary$avg_samples_per_item))
  cat("\n")
  
  # Method-specific assessments
  for (method in names(assessment$assessments)) {
    method_display <- ifelse(method == "sc_sor", 
                             "SC-SOR (Simple Ordinal Regression)",
                             "SC-MOR (Multiple Ordinal Regression)")
    cat(sprintf("Assessment for %s:\n", method_display))
    
    adequacy_table <- table(assessment$assessments[[method]]$adequacy)
    for (level in c("Excellent", "Good", "Marginal", "Poor")) {
      if (level %in% names(adequacy_table)) {
        cat(sprintf("  - %s: %d constructs\n", level, adequacy_table[level]))
      }
    }
    cat("\n")
    
    # Print detailed results if verbose
    if (verbose && any(assessment$assessments[[method]]$adequacy %in% c("Poor", "Marginal"))) {
      problematic <- assessment$assessments[[method]][
        assessment$assessments[[method]]$adequacy %in% c("Poor", "Marginal"), 
      ]
      if (nrow(problematic) > 0) {
        cat(sprintf("  Constructs needing attention for %s:\n", method))
        for (i in 1:nrow(problematic)) {
          cat(sprintf("    - %s: %d items, %s (%s)\n",
                      problematic$construct[i],
                      problematic$n_items[i],
                      problematic$adequacy[i],
                      problematic$recommendation[i]))
        }
        cat("\n")
      }
    }
  }
  
  # Warnings and recommendations
  if (length(assessment$warnings) > 0) {
    cat("⚠️  Warnings:\n")
    for (warning in assessment$warnings) {
      cat(sprintf("  - %s\n", warning))
    }
    cat("\n")
  }
  
  if (length(assessment$recommendations) > 0) {
    cat("💡 Recommendations:\n")
    for (rec in assessment$recommendations) {
      cat(sprintf("  - %s\n", rec))
    }
    cat("\n")
  }
  
  # Overall recommendation
  has_poor <- any(sapply(assessment$assessments, function(x) any(x$adequacy == "Poor")))
  has_marginal <- any(sapply(assessment$assessments, function(x) any(x$adequacy == "Marginal")))
  
  if (has_poor) {
    cat("⚠️  Overall: Some constructs may have reliability issues with regression methods.\n")
    cat("   Consider using sc_ep or dc for problematic constructs.\n")
  } else if (has_marginal) {
    cat("✓ Overall: Regression methods should work, but monitor convergence warnings.\n")
  } else {
    cat("✅ Overall: Dataset is well-suited for regression-based curtailment methods.\n")
  }
}



# ============================================================================
# Example Usage
# ============================================================================

# # Example 1: Basic configuration with two-step process
# config <- create_analysis_config(
#   ordering_methods = c("original", "auc", "correlation"),
#   reduction_methods = c("dc", "sc_ep"),
#   two_step_mode = TRUE,
#   top_candidates = 10,
# #   fnr_threshold = 0.05
# )
# 
# # Generate method combinations
# combinations <- generate_method_combinations(config)
# print(paste("Total combinations:", nrow(combinations)))
# 
# # Validate configuration
# validation <- validate_configuration(config)
# if (!validation$valid) {
#   stop(paste("Configuration errors:", paste(validation$errors, collapse = "\n")))
# }
# 
# # Example 2: Configuration for stop-low only with custom gamma values
# config_stop_low <- create_analysis_config(
#   ordering_methods = c("auc", "incremental_auc"),
#   reduction_methods = c("sc_ep", "sc_mor"),
#   constraints = list(
#     stop_low_only = TRUE,
#     min_items_per_construct = 2,
#     complete_triggered_constructs = TRUE
#   ),
# #   fnr_threshold = 0.03
# )
# 
# # Custom gamma values for stop-low
# gamma_values_stop_low <- list(
#   list(gamma_0 = 0.90, gamma_1 = 1.00),
#   list(gamma_0 = 0.95, gamma_1 = 1.00),
#   list(gamma_0 = 0.99, gamma_1 = 1.00)
# )
# 
# # Generate combinations with custom gamma values
# combinations_stop_low <- generate_method_combinations(
#   config_stop_low, 
#   gamma_values = gamma_values_stop_low
# )
# 
# # Validate configuration
# validation_stop_low <- validate_configuration(config_stop_low)
# if (!validation_stop_low$valid) {
#   stop(paste("Configuration errors:", paste(validation_stop_low$errors, collapse = "\n")))
# }
#
# # Note on ordering method behavior:
# # - For unidimensional questionnaires: All ordering methods work globally across all items
# # - For multi-construct questionnaires: Ordering methods work within each construct
# #   (constructs are presented in the order defined in the data configuration)
# # - Exception: "domain_blocked" optimizes both construct order and within-construct order