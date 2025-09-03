# ============================================================================
# Module 7: Deployment Module (ENHANCED WITH PATTERN-SPECIFIC SC-EP)
# ============================================================================
# Purpose: Generate deployment-ready outputs including stopping boundary tables
#          and SurveyJS JSON configuration for clinical implementation
#          NOW WITH: Full pattern-specific boundary support for SC-EP method
# ============================================================================

# Note on required packages:
# - jsonlite

# Required packages
required_packages <- c("jsonlite")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is required for JSON generation"))
  }
}

#' Validate Item Definitions
#'
#' @param item_definitions List of item definitions
#' @param ordered_items Vector of ordered item IDs
#' @return List with validation results
validate_item_definitions <- function(item_definitions, ordered_items) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  # Check if item_definitions is a list
  if (!is.list(item_definitions)) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                   "item_definitions must be a list")
    return(validation_results)
  }
  
  # Check if item_definitions is empty
  if (length(item_definitions) == 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                   "item_definitions cannot be empty")
    return(validation_results)
  }
  
  # Required fields for each item definition
  required_fields <- c("item_id", "item_text", "rateValues")
  
  # Check each item in ordered_items has a definition
  missing_definitions <- setdiff(ordered_items, names(item_definitions))
  if (length(missing_definitions) > 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   paste("Missing item definitions for:", 
                                         paste(missing_definitions, collapse = ", ")))
  }
  
  # Check each item definition
  for (item_name in names(item_definitions)) {
    item_def <- item_definitions[[item_name]]
    
    # Check if it's a list
    if (!is.list(item_def)) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors,
                                     paste("Definition for", item_name, "must be a list"))
      next
    }
    
    # Check required fields
    missing_fields <- setdiff(required_fields, names(item_def))
    if (length(missing_fields) > 0) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors,
                                     paste("Item", item_name, "missing required fields:", 
                                           paste(missing_fields, collapse = ", ")))
    }
    
    # Validate item_id matches the key
    if ("item_id" %in% names(item_def) && item_def$item_id != item_name) {
      validation_results$warnings <- c(validation_results$warnings,
                                       paste("Item key", item_name, "doesn't match item_id", 
                                             item_def$item_id, ". Using key as item_id."))
    }
    
    # Validate item_text is character
    if ("item_text" %in% names(item_def) && !is.character(item_def$item_text)) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors,
                                     paste("item_text for", item_name, "must be character"))
    }
    
    # Validate rateValues
    if ("rateValues" %in% names(item_def)) {
      if (!is.list(item_def$rateValues)) {
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors,
                                       paste("rateValues for", item_name, "must be a list"))
      } else {
        # Check each rate value
        for (i in seq_along(item_def$rateValues)) {
          rv <- item_def$rateValues[[i]]
          if (!is.list(rv) || !all(c("value", "text") %in% names(rv))) {
            validation_results$valid <- FALSE
            validation_results$errors <- c(validation_results$errors,
                                           paste("Each rateValue for", item_name, 
                                                 "must have 'value' and 'text' fields"))
            break
          }
          if (!is.numeric(rv$value)) {
            validation_results$valid <- FALSE
            validation_results$errors <- c(validation_results$errors,
                                           paste("rateValue 'value' for", item_name, 
                                                 "must be numeric"))
          }
          if (!is.character(rv$text)) {
            validation_results$valid <- FALSE
            validation_results$errors <- c(validation_results$errors,
                                           paste("rateValue 'text' for", item_name, 
                                                 "must be character"))
          }
        }
      }
    }
  }
  
  # Check for extra definitions not in ordered_items
  extra_definitions <- setdiff(names(item_definitions), ordered_items)
  if (length(extra_definitions) > 0) {
    validation_results$warnings <- c(validation_results$warnings,
                                     paste("Extra item definitions provided for items not in survey:", 
                                           paste(extra_definitions, collapse = ", ")))
  }
  
  return(validation_results)
}

#' Generate Deployment Package (ENHANCED)
#'
#' @param evaluation_results Results from Module 5 evaluation (optional if optimization_results provided)
#' @param all_combination_results All method combination results from Module 4 (optional if optimization_results provided)
#' @param optimization_results Results from Module 6 optimization (optional)
#' @param prepared_data Prepared data from Module 1
#' @param method_id Specific method ID to deploy (NULL = use recommended/optimized)
#' @param item_definitions List of item definitions with item_id, item_text, and rateValues
#' @param survey_config List with survey configuration (title, description, autoGenerate, displayMode)
#' @param use_pattern_boundaries For SC-EP, whether to use pattern-specific boundaries (default: TRUE)
#' @param output_dir Output directory for deployment files
#' @return List containing deployment artifacts
#' @export
generate_deployment_package <- function(
    evaluation_results = NULL,
    all_combination_results = NULL,
    optimization_results = NULL,
    prepared_data,
    method_id = NULL,
    item_definitions,
    survey_config = list(
      title = "Questionnaire",
      description = "Please answer all questions",
      autoGenerate = FALSE,
      displayMode = "buttons"
    ),
    use_pattern_boundaries = TRUE,  # NEW PARAMETER
    output_dir = "deployment"
) {
  
  # Validate inputs - need either optimization_results OR (evaluation_results + all_combination_results)
  if (is.null(optimization_results)) {
    if (is.null(evaluation_results) || is.null(all_combination_results)) {
      stop("Must provide either optimization_results OR both evaluation_results and all_combination_results")
    }
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Generating deployment package...\n")
  
  # Determine which method to deploy
  if (!is.null(optimization_results)) {
    # Use optimized method
    cat("  Using optimized method from optimization results\n")
    method_results <- optimization_results$optimized_method
    selected_method_id <- paste0(optimization_results$original_method_id, "_optimized")
    is_optimized <- TRUE
  } else {
    # Use method from evaluation results
    if (is.null(method_id)) {
      # Use recommended method
      selected_method_id <- evaluation_results$recommended_method$method_id
      cat("  Using recommended method:", selected_method_id, "\n")
    } else {
      # Use specified method
      selected_method_id <- method_id
      cat("  Using specified method:", selected_method_id, "\n")
    }
    
    # Get method results from all_combination_results
    method_results <- all_combination_results[[selected_method_id]]
    is_optimized <- FALSE
  }
  
  # Validate method results
  if (is.null(method_results)) {
    stop(paste("Method results not found for:", selected_method_id))
  }
  
  if ("error" %in% names(method_results)) {
    stop(paste("Selected method had errors:", method_results$error))
  }
  
  if (is.null(method_results$reduction_result)) {
    stop("Selected method has no reduction results")
  }
  
  if (!is.null(method_results$reduction_result$error)) {
    stop(paste("Selected method has reduction errors:", 
               method_results$reduction_result$error))
  }
  
  # Check for regression model failures
  reduction_method <- method_results$combination$reduction
  if (reduction_method %in% c("sc_sor", "sc_mor")) {
    training_params <- method_results$reduction_result$training_params
    
    if (!is.null(training_params)) {
      if (!is.null(training_params$model_result)) {
        if (!is.null(training_params$model_result$success) &&
            !training_params$model_result$success) {
          stop(paste("Selected method has model convergence failures:", 
                     training_params$model_result$error_message))
        }
      } else if (!is.null(training_params$models)) {
        if (all(sapply(training_params$models, is.null))) {
          stop("Selected method has no valid regression models")
        }
      }
    }
  }
  
  # Verify classifications are not all NA
  if (!is.null(method_results$reduction_result$classifications)) {
    if (all(is.na(method_results$reduction_result$classifications))) {
      stop("Selected method produced all NA classifications - invalid for deployment")
    }
  }
  
  # Get ordered items
  ordered_items <- method_results$ordering_result$ordered_items
  
  if (is.null(ordered_items) || length(ordered_items) == 0) {
    stop("Selected method has no ordered items")
  }
  
  # Validate item definitions
  cat("  Validating item definitions...\n")
  validation <- validate_item_definitions(item_definitions, ordered_items)
  
  if (!validation$valid) {
    stop(paste("Item definition validation failed:\n", 
               paste(validation$errors, collapse = "\n")))
  }
  
  if (length(validation$warnings) > 0) {
    cat("  Warnings:\n")
    for (w in validation$warnings) {
      cat(paste("    -", w, "\n"))
    }
  }
  
  # Check if we should use pattern-specific boundaries for SC-EP
  pattern_rules <- NULL
  if (reduction_method == "sc_ep" && use_pattern_boundaries) {
    cat("  Generating pattern-specific boundaries for SC-EP...\n")
    pattern_rules <- generate_pattern_rules(
      method_results,
      prepared_data,
      output_dir
    )
  }
  
  # Generate stopping boundary tables
  cat("  Generating stopping boundary tables...\n")
  boundary_tables <- generate_stopping_boundaries(
    method_results,
    prepared_data,
    output_dir
  )
  
  # Generate administration sequence
  cat("  Generating administration sequence...\n")
  admin_sequence <- generate_administration_sequence(
    method_results,
    prepared_data,
    item_definitions,
    output_dir
  )
  
  # Generate SurveyJS JSON (with pattern support if applicable)
  cat("  Generating SurveyJS JSON...\n")
  surveyjs_json <- generate_surveyjs_json(
    method_results,
    prepared_data,
    boundary_tables,
    admin_sequence,
    item_definitions,
    survey_config,
    pattern_rules,  # Pass pattern rules if available
    output_dir
  )
  
  # Save implementation parameters
  cat("  Saving implementation parameters...\n")
  implementation_params <- list(
    method_id = selected_method_id,
    is_optimized = is_optimized,
    method_combination = method_results$combination,
    ordering_result = method_results$ordering_result,
    training_params = method_results$reduction_result$training_params,
    boundary_tables = boundary_tables,
    pattern_rules = pattern_rules,  # Include pattern rules
    use_pattern_boundaries = use_pattern_boundaries && reduction_method == "sc_ep",
    admin_sequence = admin_sequence,
    data_config = prepared_data$config,
    optimization_info = if(is_optimized) optimization_results$optimization_analysis else NULL,
    timestamp = Sys.time()
  )
  saveRDS(implementation_params, file.path(output_dir, "implementation_params.rds"))
  
  # Generate human-readable deployment guide
  cat("  Generating deployment guide...\n")
  generate_deployment_guide(
    method_results,
    prepared_data,
    boundary_tables,
    admin_sequence,
    item_definitions,
    output_dir,
    is_optimized,
    optimization_results,
    pattern_rules
  )
  
  cat("\nDeployment package generated successfully!\n")
  cat("Output files in:", output_dir, "\n")
  
  return(list(
    method_id = selected_method_id,
    is_optimized = is_optimized,
    boundary_tables = boundary_tables,
    pattern_rules = pattern_rules,
    admin_sequence = admin_sequence,
    surveyjs_json = surveyjs_json,
    implementation_params = implementation_params
  ))
}

#' Generate Pattern-Specific Rules for SC-EP (NEW FUNCTION)
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param output_dir Output directory
#' @return List of pattern rules by construct
generate_pattern_rules <- function(method_results, prepared_data, output_dir) {
  
  # Extract method components
  is_optimized <- isTRUE(method_results$combination$optimized)
  has_construct_gammas <- !is.null(method_results$combination$construct_gammas)
  
  # Get training parameters
  training_params <- method_results$reduction_result$training_params
  
  # Get ordered items
  ordered_items <- method_results$ordering_result$ordered_items
  
  # Initialize pattern rules storage
  all_pattern_rules <- list()
  
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    # Unidimensional case
    gamma_0 <- method_results$combination$gamma_0
    gamma_1 <- method_results$combination$gamma_1
    
    pattern_rules <- generate_pattern_boundary_table(
      ordered_items = ordered_items,
      training_params = training_params,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      construct_name = "total"
    )
    
    all_pattern_rules[["total"]] <- pattern_rules
    
  } else {
    # Multi-construct case
    for (construct_name in names(prepared_data$config$constructs)) {
      # Get items for this construct
      construct_items <- prepared_data$config$constructs[[construct_name]]
      construct_ordered <- ordered_items[ordered_items %in% construct_items]
      
      # Get construct-specific parameters
      if (is.list(training_params) && !is.null(training_params[[construct_name]])) {
        construct_training_params <- training_params[[construct_name]]
      } else {
        construct_training_params <- training_params
      }
      
      # Get gamma values
      if (is_optimized && has_construct_gammas && 
          construct_name %in% names(method_results$combination$construct_gammas)) {
        construct_gamma_0 <- method_results$combination$construct_gammas[[construct_name]]$gamma_0
        construct_gamma_1 <- method_results$combination$construct_gammas[[construct_name]]$gamma_1
      } else {
        construct_gamma_0 <- method_results$combination$gamma_0
        construct_gamma_1 <- method_results$combination$gamma_1
      }
      
      # Generate pattern rules for this construct
      pattern_rules <- generate_pattern_boundary_table(
        ordered_items = construct_ordered,
        training_params = construct_training_params,
        gamma_0 = construct_gamma_0,
        gamma_1 = construct_gamma_1,
        construct_name = construct_name
      )
      
      all_pattern_rules[[construct_name]] <- pattern_rules
    }
  }
  
  # Save pattern rules
  saveRDS(all_pattern_rules, file.path(output_dir, "pattern_rules.rds"))
  
  # Also save as JSON for external use
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    pattern_json <- jsonlite::toJSON(all_pattern_rules, pretty = TRUE, auto_unbox = TRUE)
    writeLines(pattern_json, file.path(output_dir, "pattern_rules.json"))
  }
  
  # Generate human-readable pattern rules summary
  generate_pattern_rules_summary(all_pattern_rules, output_dir)
  
  return(all_pattern_rules)
}

#' Generate Pattern-Specific Boundary Table (FIXED - NO AGGREGATION)
#'
#' @param ordered_items Ordered items
#' @param training_params Training parameters with lookup tables
#' @param gamma_0 Low-risk threshold
#' @param gamma_1 High-risk threshold
#' @param construct_name Name of construct
#' @return List with pattern rules for each position
generate_pattern_boundary_table <- function(ordered_items, training_params, 
                                            gamma_0, gamma_1, 
                                            construct_name = NULL) {
  
  n_items <- length(ordered_items)
  
  # Get lookup tables
  lookup_tables <- training_params$lookup_tables
  
  if (is.null(lookup_tables)) {
    warning(paste("No lookup tables found for SC-EP method",
                  if(!is.null(construct_name)) paste("for construct", construct_name)))
    return(NULL)
  }
  
  # Create pattern rules for each position
  pattern_rules <- list()
  
  for (k in 1:n_items) {
    if (k <= length(lookup_tables) && !is.null(lookup_tables[[k]])) {
      
      low_risk_patterns <- list()
      high_risk_patterns <- list()
      neutral_patterns <- list()
      
      # Check each pattern INDIVIDUALLY - NO AGGREGATION BY SUM SCORE
      for (pattern_str in names(lookup_tables[[k]])) {
        probs <- lookup_tables[[k]][[pattern_str]]
        
        # Parse pattern
        scores <- as.numeric(strsplit(pattern_str, "_")[[1]])
        
        pattern_info <- list(
          pattern = pattern_str,
          scores = scores,
          sum_score = sum(scores),
          prob_low = probs["prob_low"],
          prob_high = probs["prob_high"]
        )
        
        # Classify pattern based on ITS OWN probabilities, not aggregated
        if (!is.na(probs["prob_low"]) && probs["prob_low"] >= gamma_0) {
          low_risk_patterns[[pattern_str]] <- pattern_info
        } else if (!is.na(probs["prob_high"]) && probs["prob_high"] >= gamma_1) {
          high_risk_patterns[[pattern_str]] <- pattern_info
        } else {
          neutral_patterns[[pattern_str]] <- pattern_info
        }
      }
      
      pattern_rules[[k]] <- list(
        position = k,
        items_included = ordered_items[1:k],
        low_risk_patterns = low_risk_patterns,
        high_risk_patterns = high_risk_patterns,
        neutral_patterns = neutral_patterns,
        total_patterns = length(lookup_tables[[k]]),
        construct = construct_name
      )
    }
  }
  
  return(pattern_rules)
}

#' Generate Pattern Rules Summary (NEW FUNCTION)
#'
#' @param all_pattern_rules All pattern rules
#' @param output_dir Output directory
generate_pattern_rules_summary <- function(all_pattern_rules, output_dir) {
  
  summary_file <- file.path(output_dir, "pattern_rules_summary.txt")
  
  summary_text <- paste0(
    "PATTERN-SPECIFIC STOPPING RULES SUMMARY\n",
    "=======================================\n\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
  )
  
  for (construct_name in names(all_pattern_rules)) {
    rules <- all_pattern_rules[[construct_name]]
    
    if (!is.null(rules)) {
      summary_text <- paste0(summary_text,
                             "Construct: ", construct_name, "\n",
                             "-------------------\n")
      
      for (k in seq_along(rules)) {
        if (!is.null(rules[[k]])) {
          pos_rules <- rules[[k]]
          n_low <- length(pos_rules$low_risk_patterns)
          n_high <- length(pos_rules$high_risk_patterns)
          n_neutral <- length(pos_rules$neutral_patterns)
          
          summary_text <- paste0(summary_text,
                                 "Position ", k, " (", 
                                 paste(pos_rules$items_included, collapse=", "), "):\n",
                                 "  Low-risk patterns: ", n_low, "\n",
                                 "  High-risk patterns: ", n_high, "\n",
                                 "  Neutral patterns: ", n_neutral, "\n")
          
          # Show example patterns
          if (n_low > 0) {
            example_low <- names(pos_rules$low_risk_patterns)[1]
            summary_text <- paste0(summary_text,
                                   "    Example low-risk: ", example_low, "\n")
          }
          if (n_high > 0) {
            example_high <- names(pos_rules$high_risk_patterns)[1]
            summary_text <- paste0(summary_text,
                                   "    Example high-risk: ", example_high, "\n")
          }
        }
      }
      summary_text <- paste0(summary_text, "\n")
    }
  }
  
  writeLines(summary_text, summary_file)
}

#' Enhanced Generate Stopping Boundaries (WITH INTEGRATED VALIDATION)
#'
#' @param method_results Selected method results
#' @param prepared_data Prepared data
#' @param output_dir Output directory
#' @return List of boundary tables
generate_stopping_boundaries <- function(method_results, prepared_data, output_dir) {
  
  # Extract method components
  ordering <- method_results$combination$ordering
  reduction <- method_results$combination$reduction
  
  # Check if this is an optimized method with construct-specific gammas
  is_optimized <- isTRUE(method_results$combination$optimized)
  has_construct_gammas <- !is.null(method_results$combination$construct_gammas)
  
  # Get training parameters
  training_params <- method_results$reduction_result$training_params
  
  # Debug: Check training params structure
  if (is.null(training_params)) {
    warning("No training parameters found in reduction_result")
  }
  
  # Get ordered items (with interleaving applied)
  ordered_items <- method_results$ordering_result$ordered_items
  
  # Check constraints
  constraints <- method_results$reduction_result$constraints_applied %||% list()
  stop_low_only <- constraints$stop_low_only %||% FALSE
  
  # Initialize results
  boundary_tables <- list()
  
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    # For unidimensional, use global gammas (which may have been optimized)
    gamma_0 <- method_results$combination$gamma_0
    gamma_1 <- method_results$combination$gamma_1
    
    # Generate single boundary table
    boundary_table <- generate_single_boundary_table(
      ordered_items,
      prepared_data$config,
      training_params,
      reduction,
      gamma_0,
      gamma_1,
      stop_low_only
    )
    
    boundary_tables[["total"]] <- boundary_table
    
    # Save as CSV
    write.csv(boundary_table,
              file.path(output_dir, "stopping_boundaries.csv"),
              row.names = FALSE)
    
  } else {
    # Multi-construct case
    for (construct_name in names(prepared_data$config$constructs)) {
      # Get items for this construct in administration order
      construct_items <- prepared_data$config$constructs[[construct_name]]
      construct_ordered <- ordered_items[ordered_items %in% construct_items]
      
      # Get construct-specific training params
      if (is.list(training_params) && !is.null(training_params[[construct_name]])) {
        construct_training_params <- training_params[[construct_name]]
      } else {
        # For methods that don't have construct-specific training params
        construct_training_params <- training_params
      }
      
      # Determine gamma values for this construct
      if (is_optimized && has_construct_gammas && 
          construct_name %in% names(method_results$combination$construct_gammas)) {
        # Use optimized construct-specific gammas
        construct_gamma_0 <- method_results$combination$construct_gammas[[construct_name]]$gamma_0
        construct_gamma_1 <- method_results$combination$construct_gammas[[construct_name]]$gamma_1
      } else {
        # Use global gammas
        construct_gamma_0 <- method_results$combination$gamma_0
        construct_gamma_1 <- method_results$combination$gamma_1
      }
      
      # Generate boundary table
      boundary_table <- generate_single_boundary_table(
        construct_ordered,
        prepared_data$config,
        construct_training_params,
        reduction,
        construct_gamma_0,
        construct_gamma_1,
        stop_low_only,
        construct_name
      )
      
      boundary_tables[[construct_name]] <- boundary_table
    }
    
    # Save combined CSV
    combined_table <- do.call(rbind, lapply(names(boundary_tables), function(cn) {
      tbl <- boundary_tables[[cn]]
      tbl$construct <- cn
      tbl <- tbl[, c("construct", names(tbl)[names(tbl) != "construct"])]
      return(tbl)
    }))
    
    write.csv(combined_table,
              file.path(output_dir, "stopping_boundaries_all_constructs.csv"),
              row.names = FALSE)
    
    # Save individual construct tables
    for (construct_name in names(boundary_tables)) {
      write.csv(boundary_tables[[construct_name]],
                file.path(output_dir, paste0("stopping_boundaries_", construct_name, ".csv")),
                row.names = FALSE)
    }
  }
  
  # INTEGRATED VALIDATION STEP
  cat("  Validating boundary tables...\n")
  validation_results <- validate_boundary_tables(boundary_tables, output_dir)
  
  if (validation_results$has_issues) {
    cat("  ⚠️  WARNING: Boundary validation found issues!\n")
    cat("     See boundary_validation_report.txt for details\n")
    
    # Print critical issues to console
    critical_issues <- validation_results$issues[grepl("Inverted boundaries", validation_results$issues)]
    if (length(critical_issues) > 0) {
      cat("     Critical issues:\n")
      for (issue in critical_issues) {
        cat("       ", issue, "\n")
      }
    }
    
    # Print summary statistics
    total_conflicts <- sum(sapply(validation_results$statistics, function(s) s$positions_with_conflicts))
    total_same <- sum(sapply(validation_results$statistics, function(s) s$positions_with_same_boundary))
    
    cat("     Summary: ", total_conflicts, " inverted boundaries, ", total_same, " same boundaries\n")
    
  } else {
    cat("  ✅ Boundary validation passed - no conflicts detected\n")
  }
  
  # Also save as HTML for better viewing  
  generate_boundary_tables_html(boundary_tables, prepared_data, output_dir)
  
  return(boundary_tables)
}

#' Generate Single Boundary Table (FIXED VERSION)
#'
#' @param ordered_items Ordered items for this construct/total
#' @param config Data configuration
#' @param training_params Training parameters
#' @param reduction Reduction method
#' @param gamma_0 Low-risk threshold
#' @param gamma_1 High-risk threshold
#' @param stop_low_only Whether only low-risk stopping is allowed
#' @param construct_name Name of construct (NULL for unidimensional)
#' @return Data frame with boundary information
generate_single_boundary_table <- function(ordered_items, config, training_params,
                                           reduction, gamma_0, gamma_1, stop_low_only,
                                           construct_name = NULL) {
  
  n_items <- length(ordered_items)
  
  # Get cutoff
  if (!is.null(construct_name)) {
    cutoff <- config$cutoffs[[construct_name]]
  } else {
    cutoff <- config$cutoffs[["total"]] %||% config$cutoffs[[1]]
  }
  
  # Initialize boundary table with updated structure
  boundary_df <- data.frame(
    items_administered = 1:n_items,
    items_included = NA_character_,
    low_risk_boundary = NA_character_,
    high_risk_boundary = NA_character_,
    method_notes = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Check if gamma values are valid (not strings like "construct_specific")
  gamma_0_valid <- is.numeric(gamma_0)
  gamma_1_valid <- is.numeric(gamma_1)
  
  # Calculate boundaries based on reduction method
  if (reduction == "none") {
    # No reduction - all items must be administered
    boundary_df$low_risk_boundary <- "N/A"
    boundary_df$high_risk_boundary <- "N/A"
    boundary_df$method_notes <- "No early stopping - all items required"
    boundary_df$low_risk_boundary[n_items] <- paste0("Xk < ", cutoff)
    boundary_df$high_risk_boundary[n_items] <- paste0("Xk >= ", cutoff)
    boundary_df$method_notes[n_items] <- "Final classification"
    
  } else if (reduction == "dc") {
    # Deterministic curtailment
    boundaries <- calculate_dc_boundaries(ordered_items, training_params, cutoff)
    
    for (k in 1:n_items) {
      boundary_df$items_included[k] <- paste(ordered_items[1:k], collapse = ", ")
      
      if (!is.na(boundaries$low_boundary[k])) {
        boundary_df$low_risk_boundary[k] <- paste0("Xk <= ", boundaries$low_boundary[k])
      }
      
      if (!is.na(boundaries$high_boundary[k]) && !stop_low_only) {
        boundary_df$high_risk_boundary[k] <- paste0("Xk >= ", boundaries$high_boundary[k])
      }
      
      boundary_df$method_notes[k] <- "Deterministic boundaries (exact sum scores)"
    }
    
  } else if (reduction == "sc_ep" && gamma_0_valid && gamma_1_valid) {
    # SC-EP: Pattern-specific - DO NOT GENERATE SUM-SCORE BOUNDARIES
    # The pattern rules handle everything, so we just provide informational text
    
    for (k in 1:n_items) {
      boundary_df$items_included[k] <- paste(ordered_items[1:k], collapse = ", ")
      boundary_df$low_risk_boundary[k] <- "Pattern-dependent"
      boundary_df$high_risk_boundary[k] <- ifelse(stop_low_only || gamma_1 >= 1.0, 
                                                  "N/A", 
                                                  "Pattern-dependent")
      boundary_df$method_notes[k] <- paste0("SC-EP pattern-specific (γ₀=", gamma_0, 
                                            ", γ₁=", gamma_1, ") - see pattern_rules.json")
    }
    
  } else if (reduction %in% c("sc_sor", "sc_mor") && gamma_0_valid && gamma_1_valid) {
    # Stochastic curtailment with regression - keep existing logic
    boundaries <- calculate_sc_boundaries(
      ordered_items, training_params, reduction,
      gamma_0, gamma_1, cutoff
    )
    
    for (k in 1:n_items) {
      boundary_df$items_included[k] <- paste(ordered_items[1:k], collapse = ", ")
      
      if (!is.na(boundaries$low_boundary[k])) {
        boundary_df$low_risk_boundary[k] <- paste0("Xk <= ", boundaries$low_boundary[k])
      }
      
      if (!is.na(boundaries$high_boundary[k]) && !stop_low_only && gamma_1 < 1.0) {
        boundary_df$high_risk_boundary[k] <- paste0("Xk >= ", boundaries$high_boundary[k])
      }
      
      # Method-specific notes
      if (reduction == "sc_sor") {
        boundary_df$method_notes[k] <- paste0("SC-SOR regression-based (γ₀=", gamma_0, ", γ₁=", gamma_1, ")")
      } else if (reduction == "sc_mor") {
        boundary_df$method_notes[k] <- paste0("SC-MOR approximate boundaries (γ₀=", gamma_0, ", γ₁=", gamma_1, ")")
      }
    }
    
  } else if (reduction == "irt_cct") {
    # IRT-based CCT - convert theta to sum scores
    boundary_df$low_risk_boundary <- "Based on theta estimate"
    boundary_df$high_risk_boundary <- ifelse(stop_low_only, "N/A", "Based on theta estimate")
    boundary_df$method_notes <- "IRT-based classification (theta estimates)"
    boundary_df$items_included <- sapply(1:n_items, function(k) {
      paste(ordered_items[1:k], collapse = ", ")
    })
  } else {
    # Invalid gamma values or unknown reduction method
    boundary_df$low_risk_boundary <- "N/A"
    boundary_df$high_risk_boundary <- "N/A"
    boundary_df$method_notes <- "Invalid parameters or unsupported method"
  }
  
  # Clean up items_included column if still NA
  if (all(is.na(boundary_df$items_included))) {
    boundary_df$items_included <- sapply(1:n_items, function(k) {
      paste(ordered_items[1:k], collapse = ", ")
    })
  }
  
  # Replace remaining NAs with appropriate values
  boundary_df$low_risk_boundary[is.na(boundary_df$low_risk_boundary)] <- "N/A"
  boundary_df$high_risk_boundary[is.na(boundary_df$high_risk_boundary)] <- "N/A"
  boundary_df$method_notes[is.na(boundary_df$method_notes)] <- ""
  
  return(boundary_df)
}

#' Calculate Deterministic Curtailment Boundaries (CORRECTED VERSION)
#'
#' @param ordered_items Ordered items
#' @param training_params Training parameters
#' @param cutoff Classification cutoff
#' @return List with low and high boundaries
calculate_dc_boundaries <- function(ordered_items, training_params, cutoff) {
  
  n_items <- length(ordered_items)
  low_boundary <- rep(NA, n_items)
  high_boundary <- rep(NA, n_items)
  
  # Get item ranges
  item_ranges <- training_params$item_ranges
  
  for (k in 1:n_items) {
    if (k < n_items) {
      # Calculate min/max possible scores from remaining items
      remaining_items <- ordered_items[(k+1):n_items]
      
      # CORRECTED: For low risk boundary: current + max_remaining < cutoff
      # So current_score <= cutoff - max_remaining - 1
      max_remaining <- sum(sapply(remaining_items, function(x) {
        if (!is.null(item_ranges[[x]])) item_ranges[[x]][2] else 3
      }))
      low_boundary[k] <- cutoff - max_remaining - 1
      
      # CORRECTED: For high risk boundary: current + min_remaining >= cutoff
      # So current_score >= cutoff - min_remaining
      min_remaining <- sum(sapply(remaining_items, function(x) {
        if (!is.null(item_ranges[[x]])) item_ranges[[x]][1] else 0
      }))
      high_boundary[k] <- cutoff - min_remaining
      
      # Validate boundaries
      if (low_boundary[k] < 0) low_boundary[k] <- NA
      if (high_boundary[k] > k * 3) high_boundary[k] <- NA  # Assuming max item score is 3
      
      # VALIDATION: Check for logical consistency
      if (!is.na(low_boundary[k]) && !is.na(high_boundary[k])) {
        if (low_boundary[k] >= high_boundary[k]) {
          # This indicates the cutoff might be impossible to achieve with early stopping
          warning(paste("DC boundaries logically inconsistent at position", k,
                        ": cutoff =", cutoff, 
                        ", max_remaining =", max_remaining,
                        ", min_remaining =", min_remaining,
                        ", computed low_boundary =", low_boundary[k],
                        ", computed high_boundary =", high_boundary[k]))
          
          # In this case, early stopping is not beneficial at this position
          low_boundary[k] <- NA
          high_boundary[k] <- NA
        }
      }
      
    } else {
      # Last item - simple cutoff
      low_boundary[k] <- cutoff - 1
      high_boundary[k] <- cutoff
    }
  }
  
  return(list(low_boundary = low_boundary, high_boundary = high_boundary))
}

#' Calculate Stochastic Curtailment Boundaries (SUM-SCORE VERSION - KEPT FOR COMPATIBILITY)
#'
#' @param ordered_items Ordered items
#' @param training_params Training parameters
#' @param method SC method type
#' @param gamma_0 Low-risk threshold
#' @param gamma_1 High-risk threshold
#' @param cutoff Classification cutoff
#' @return List with low and high boundaries
calculate_sc_boundaries <- function(ordered_items, training_params, method,
                                    gamma_0, gamma_1, cutoff) {
  
  n_items <- length(ordered_items)
  low_boundary <- rep(NA, n_items)
  high_boundary <- rep(NA, n_items)
  
  # Check if training_params is NULL
  if (is.null(training_params)) {
    warning("Training parameters are NULL - cannot calculate boundaries")
    return(list(low_boundary = low_boundary, high_boundary = high_boundary))
  }
  
  # Check for model failures in regression-based methods
  if (method %in% c("sc_sor", "sc_mor")) {
    if (!is.null(training_params$model_result)) {
      if (!is.null(training_params$model_result$success) &&
          !training_params$model_result$success) {
        # Return all NAs if models failed
        return(list(low_boundary = low_boundary, high_boundary = high_boundary))
      }
    }
  }
  
  if (method == "sc_ep") {
    # Use empirical lookup tables with proper conflict resolution
    lookup_tables <- training_params$lookup_tables
    
    if (is.null(lookup_tables)) {
      warning("No lookup tables found in training parameters for sc_ep method")
      return(list(low_boundary = low_boundary, high_boundary = high_boundary))
    }
    
    for (k in 1:n_items) {
      if (k <= length(lookup_tables) && !is.null(lookup_tables[[k]])) {
        
        # Step 1: Group patterns by sum score and collect their probabilities
        sum_score_groups <- list()
        patterns <- names(lookup_tables[[k]])
        
        for (pattern in patterns) {
          probs <- lookup_tables[[k]][[pattern]]
          scores <- as.numeric(strsplit(pattern, "_")[[1]])
          sum_score <- sum(scores)
          key <- as.character(sum_score)
          
          if (is.null(sum_score_groups[[key]])) {
            sum_score_groups[[key]] <- list(
              sum_score = sum_score,
              patterns = c(),
              prob_lows = c(),
              prob_highs = c(),
              n_patterns = 0
            )
          }
          
          sum_score_groups[[key]]$patterns <- c(sum_score_groups[[key]]$patterns, pattern)
          sum_score_groups[[key]]$prob_lows <- c(sum_score_groups[[key]]$prob_lows, probs["prob_low"])
          sum_score_groups[[key]]$prob_highs <- c(sum_score_groups[[key]]$prob_highs, probs["prob_high"])
          sum_score_groups[[key]]$n_patterns <- sum_score_groups[[key]]$n_patterns + 1
        }
        
        # Step 2: Determine which sum scores qualify for stopping using CONSERVATIVE aggregation
        # Only allow stopping if there's strong consensus among patterns with the same sum
        
        low_candidates <- c()
        high_candidates <- c()
        
        for (key in names(sum_score_groups)) {
          group <- sum_score_groups[[key]]
          sum_score <- group$sum_score
          
          # CONSERVATIVE RULE: Use minimum probability across patterns with same sum
          # This ensures we only stop when even the least confident pattern supports the decision
          min_prob_low <- min(group$prob_lows, na.rm = TRUE)
          min_prob_high <- min(group$prob_highs, na.rm = TRUE)
          
          # Check if this sum score qualifies for stopping
          if (!is.na(min_prob_low) && min_prob_low >= gamma_0) {
            low_candidates <- c(low_candidates, sum_score)
          }
          
          if (!is.na(min_prob_high) && min_prob_high >= gamma_1) {
            high_candidates <- c(high_candidates, sum_score)
          }
        }
        
        # Step 3: Set preliminary boundaries
        if (length(low_candidates) > 0) {
          low_boundary[k] <- max(low_candidates)  # Highest sum for low-risk stopping
        }
        
        if (length(high_candidates) > 0) {
          high_boundary[k] <- min(high_candidates)  # Lowest sum for high-risk stopping
        }
        
        # Step 4: CONFLICT RESOLUTION
        if (!is.na(low_boundary[k]) && !is.na(high_boundary[k])) {
          
          if (low_boundary[k] > high_boundary[k]) {
            # Inverted boundaries - resolve by keeping the more conservative boundary
            warning(paste("SC-EP inverted boundaries at position", k,
                          "- low ≤", low_boundary[k], "> high ≥", high_boundary[k]))
            
            # Strategy: Keep boundary that allows more item administration (more conservative)
            # Count supporting evidence for each boundary
            low_support_strength <- length(low_candidates)
            high_support_strength <- length(high_candidates)
            
            # Also consider how extreme the boundaries are relative to possible scores
            max_possible_at_k <- k * 3  # Assuming max item score is 3
            low_extremity <- low_boundary[k] / max_possible_at_k
            high_extremity <- high_boundary[k] / max_possible_at_k
            
            # Resolution logic: prefer less extreme boundary with more support
            if (low_support_strength > high_support_strength) {
              # More evidence for low boundary
              high_boundary[k] <- NA
              cat("  Resolved: kept low boundary ≤", low_boundary[k], "\n")
            } else if (high_support_strength > low_support_strength) {
              # More evidence for high boundary
              low_boundary[k] <- NA
              cat("  Resolved: kept high boundary ≥", high_boundary[k], "\n")
            } else {
              # Equal support - choose based on extremity (keep less extreme)
              if (low_extremity <= high_extremity) {
                high_boundary[k] <- NA
                cat("  Resolved: kept less extreme low boundary ≤", low_boundary[k], "\n")
              } else {
                low_boundary[k] <- NA
                cat("  Resolved: kept less extreme high boundary ≥", high_boundary[k], "\n")
              }
            }
            
          } else if (low_boundary[k] == high_boundary[k]) {
            # Same boundary value - check if this is a genuine conflict or valid extreme case
            shared_sum <- low_boundary[k]
            shared_key <- as.character(shared_sum)
            
            if (shared_key %in% names(sum_score_groups)) {
              group <- sum_score_groups[[shared_key]]
              
              # Check if patterns genuinely support conflicting decisions
              n_low_support <- sum(group$prob_lows >= gamma_0, na.rm = TRUE)
              n_high_support <- sum(group$prob_highs >= gamma_1, na.rm = TRUE)
              
              if (n_low_support > 0 && n_high_support > 0 && group$n_patterns > 1) {
                # Genuine conflict - different patterns with same sum support different decisions
                warning(paste("SC-EP conflicting patterns at sum score", shared_sum,
                              "position", k, "- removing boundaries to avoid conflicts"))
                low_boundary[k] <- NA
                high_boundary[k] <- NA
              } else {
                # Valid case - all patterns with this sum support the same extreme decision
                # This can happen with very clear-cut cases, so keep both boundaries
                cat("  Valid same boundary at position", k, "sum score", shared_sum, 
                    "- all patterns strongly support immediate classification\n")
              }
            }
          }
        }
        
        # Step 5: Final validation - ensure boundaries make clinical sense
        if (!is.na(low_boundary[k])) {
          # Low boundary shouldn't be higher than the cutoff (doesn't make sense)
          if (low_boundary[k] >= cutoff) {
            warning(paste("SC-EP low boundary", low_boundary[k], 
                          "≥ cutoff", cutoff, "at position", k, "- may be too aggressive"))
          }
        }
        
        if (!is.na(high_boundary[k])) {
          # High boundary shouldn't be much lower than cutoff
          if (high_boundary[k] < cutoff * 0.5) {
            warning(paste("SC-EP high boundary", high_boundary[k], 
                          "very low relative to cutoff", cutoff, "at position", k))
          }
        }
      }
    }
    
  } else if (method == "sc_sor") {
    # SC-SOR: Use regression models (existing logic with added conflict resolution)
    if (!is.null(training_params$model_result)) {
      models <- training_params$model_result$models
    } else {
      models <- training_params$models
    }
    
    if (is.null(models)) {
      warning("No models found in training parameters for sc_sor method")
      return(list(low_boundary = low_boundary, high_boundary = high_boundary))
    }
    
    for (k in 1:n_items) {
      if (k <= length(models) && !is.null(models[[k]])) {
        
        # Search for boundary where P(low) >= gamma_0 and P(high) >= gamma_1
        max_possible_score <- k * 3  # Assuming max item score is 3
        
        low_candidates <- c()
        high_candidates <- c()
        
        for (sum_score in 0:max_possible_score) {
          tryCatch({
            pred_prob <- predict(models[[k]],
                                 newdata = data.frame(sum_score = sum_score),
                                 type = "response")
            
            prob_high <- as.numeric(pred_prob)
            prob_low <- 1 - prob_high
            
            if (!is.na(prob_low) && prob_low >= gamma_0) {
              low_candidates <- c(low_candidates, sum_score)
            }
            
            if (!is.na(prob_high) && prob_high >= gamma_1) {
              high_candidates <- c(high_candidates, sum_score)
            }
            
          }, error = function(e) {
            # Skip problematic predictions
          })
        }
        
        # Set boundaries
        if (length(low_candidates) > 0) {
          low_boundary[k] <- max(low_candidates)
        }
        if (length(high_candidates) > 0) {
          high_boundary[k] <- min(high_candidates)
        }
        
        # Conflict resolution for SC-SOR
        if (!is.na(low_boundary[k]) && !is.na(high_boundary[k])) {
          if (low_boundary[k] >= high_boundary[k]) {
            warning(paste("SC-SOR boundaries overlap at position", k,
                          "- gamma values may be too aggressive for this model"))
            
            # For regression models, prefer the boundary with more model confidence
            # Remove the more extreme boundary
            mean_cutoff_score <- cutoff * k / n_items  # Expected score at position k
            
            if (abs(low_boundary[k] - mean_cutoff_score) > abs(high_boundary[k] - mean_cutoff_score)) {
              # Low boundary is more extreme, remove it
              low_boundary[k] <- NA
            } else {
              # High boundary is more extreme, remove it
              high_boundary[k] <- NA
            }
          }
        }
      }
    }
    
  } else if (method == "sc_mor") {
    # SC-MOR is indeed complex - acknowledge this honestly
    # For pattern-dependent methods, boundaries are approximate
    warning("SC-MOR boundaries are pattern-dependent - returning simplified approximations")
    return(list(low_boundary = low_boundary, high_boundary = high_boundary))
  }
  
  return(list(low_boundary = low_boundary, high_boundary = high_boundary))
}

#' Validate Boundary Tables After Generation (ENHANCED VERSION)
#'
#' @param boundary_tables List of boundary tables by construct
#' @param output_dir Output directory for validation report
#' @return List with validation results
validate_boundary_tables <- function(boundary_tables, output_dir) {
  
  validation_report <- c()
  has_issues <- FALSE
  boundary_statistics <- list()
  
  for (construct_name in names(boundary_tables)) {
    boundary_table <- boundary_tables[[construct_name]]
    
    construct_stats <- list(
      total_positions = nrow(boundary_table),
      positions_with_low_boundary = 0,
      positions_with_high_boundary = 0,
      positions_with_conflicts = 0,
      positions_with_same_boundary = 0
    )
    
    for (i in 1:nrow(boundary_table)) {
      low_boundary_text <- boundary_table$low_risk_boundary[i]
      high_boundary_text <- boundary_table$high_risk_boundary[i]
      
      # Skip if either is N/A
      if (low_boundary_text == "N/A" && high_boundary_text == "N/A") next
      
      # Count boundaries
      if (low_boundary_text != "N/A") {
        construct_stats$positions_with_low_boundary <- construct_stats$positions_with_low_boundary + 1
      }
      if (high_boundary_text != "N/A") {
        construct_stats$positions_with_high_boundary <- construct_stats$positions_with_high_boundary + 1
      }
      
      # Extract numeric values for validation
      low_val <- NA
      high_val <- NA
      
      if (grepl("Xk <= ", low_boundary_text)) {
        low_val <- as.numeric(gsub("Xk <= ", "", low_boundary_text))
      }
      if (grepl("Xk >= ", high_boundary_text)) {
        high_val <- as.numeric(gsub("Xk >= ", "", high_boundary_text))
      }
      
      # Check for issues
      if (!is.na(low_val) && !is.na(high_val)) {
        if (low_val == high_val) {
          construct_stats$positions_with_same_boundary <- construct_stats$positions_with_same_boundary + 1
          issue_msg <- paste("ISSUE:", construct_name, "position", i, 
                             "- Same boundary:", low_val)
          validation_report <- c(validation_report, issue_msg)
          has_issues <- TRUE
        }
        
        if (low_val > high_val) {
          construct_stats$positions_with_conflicts <- construct_stats$positions_with_conflicts + 1
          issue_msg <- paste("ISSUE:", construct_name, "position", i, 
                             "- Inverted boundaries: low ≤", low_val, 
                             "> high ≥", high_val)
          validation_report <- c(validation_report, issue_msg)
          has_issues <- TRUE
        }
      }
    }
    
    boundary_statistics[[construct_name]] <- construct_stats
  }
  
  # Generate enhanced validation report
  if (length(validation_report) > 0 || length(boundary_statistics) > 0) {
    report_text <- c(
      "BOUNDARY VALIDATION REPORT",
      "=========================",
      paste("Generated:", Sys.time()),
      "",
      "SUMMARY STATISTICS:",
      "==================="
    )
    
    # Add statistics for each construct
    for (construct_name in names(boundary_statistics)) {
      stats <- boundary_statistics[[construct_name]]
      report_text <- c(report_text,
                       paste0("Construct: ", construct_name),
                       paste0("  Total positions: ", stats$total_positions),
                       paste0("  Positions with low boundaries: ", stats$positions_with_low_boundary),
                       paste0("  Positions with high boundaries: ", stats$positions_with_high_boundary),
                       paste0("  Positions with conflicts: ", stats$positions_with_conflicts),
                       paste0("  Positions with same boundaries: ", stats$positions_with_same_boundary),
                       "")
    }
    
    if (length(validation_report) > 0) {
      report_text <- c(report_text,
                       "DETAILED ISSUES:",
                       "===============",
                       validation_report)
    } else {
      report_text <- c(report_text, "No boundary conflicts detected!")
    }
    
    writeLines(report_text, file.path(output_dir, "boundary_validation_report.txt"))
  }
  
  return(list(
    has_issues = has_issues,
    issues = validation_report,
    statistics = boundary_statistics
  ))
}

#' Generate Administration Sequence
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param item_definitions List of item definitions
#' @param output_dir Output directory
#' @return Data frame with administration sequence
generate_administration_sequence <- function(method_results, prepared_data, item_definitions, output_dir) {
  
  # Get ordered items
  ordered_items <- method_results$ordering_result$ordered_items
  
  # Create sequence data frame
  admin_seq <- data.frame(
    admin_order = 1:length(ordered_items),
    item_id = ordered_items,
    original_position = as.numeric(gsub("^q", "", ordered_items)),
    stringsAsFactors = FALSE
  )
  
  # Add item text from item_definitions
  admin_seq$item_text <- NA_character_
  for (i in 1:nrow(admin_seq)) {
    item_id <- admin_seq$item_id[i]
    if (item_id %in% names(item_definitions)) {
      admin_seq$item_text[i] <- item_definitions[[item_id]]$item_text
    } else {
      # This shouldn't happen if validation passed
      admin_seq$item_text[i] <- paste("Question", admin_seq$original_position[i])
    }
  }
  
  # Add construct information for multi-construct
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    admin_seq$construct <- NA_character_
    
    for (construct_name in names(prepared_data$config$constructs)) {
      construct_items <- prepared_data$config$constructs[[construct_name]]
      admin_seq$construct[admin_seq$item_id %in% construct_items] <- construct_name
    }
    
    # Add within-construct order
    admin_seq$within_construct_order <- NA_integer_
    for (construct_name in unique(admin_seq$construct)) {
      construct_rows <- which(admin_seq$construct == construct_name)
      admin_seq$within_construct_order[construct_rows] <- 1:length(construct_rows)
    }
    
    # Reorder columns as specified
    admin_seq <- admin_seq[, c("admin_order", "item_id", "original_position",
                               "item_text", "construct", "within_construct_order")]
  } else {
    # For unidimensional, no construct columns needed
    admin_seq <- admin_seq[, c("admin_order", "item_id", "original_position", "item_text")]
  }
  
  # Save administration sequence
  write.csv(admin_seq,
            file.path(output_dir, "administration_sequence.csv"),
            row.names = FALSE)
  
  return(admin_seq)
}

#' #' Generate Pattern-Based Visibility Condition (NEW FUNCTION)
#' #'
#' #' @param prev_items Previous items administered
#' #' @param pattern_rules Pattern rules for current position
#' #' @param stop_low_only Whether only low-risk stopping is allowed
#' #' @return SurveyJS visibility condition string
#' generate_pattern_visibility_condition <- function(prev_items, pattern_rules, 
#'                                                   stop_low_only = FALSE) {
#'   
#'   if (is.null(pattern_rules)) {
#'     return(NULL)  # No pattern rules for this position
#'   }
#'   
#'   # Get pattern info
#'   low_patterns <- pattern_rules$low_risk_patterns
#'   high_patterns <- pattern_rules$high_risk_patterns
#'   
#'   if (length(low_patterns) == 0 && length(high_patterns) == 0) {
#'     return(NULL)  # No stopping patterns at this position
#'   }
#'   
#'   # Build conditions for continuing (NOT stopping)
#'   continue_conditions <- character()
#'   
#'   # Low-risk patterns - continue if NOT matching any low-risk pattern
#'   if (length(low_patterns) > 0) {
#'     low_conditions <- character()
#'     
#'     for (pattern_name in names(low_patterns)) {
#'       pattern_info <- low_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       # Create condition for this specific pattern
#'       pattern_checks <- character()
#'       for (i in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[i], "} == ", scores[i]))
#'       }
#'       
#'       # This pattern would trigger LOW risk stop
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       low_conditions <- c(low_conditions, pattern_condition)
#'     }
#'     
#'     # Continue if NOT matching any low-risk pattern
#'     if (length(low_conditions) > 0) {
#'       not_low_risk <- paste0("!(", paste(low_conditions, collapse = " or "), ")")
#'       continue_conditions <- c(continue_conditions, not_low_risk)
#'     }
#'   }
#'   
#'   # High-risk patterns - continue if NOT matching any high-risk pattern
#'   if (!stop_low_only && length(high_patterns) > 0) {
#'     high_conditions <- character()
#'     
#'     for (pattern_name in names(high_patterns)) {
#'       pattern_info <- high_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       pattern_checks <- character()
#'       for (i in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[i], "} == ", scores[i]))
#'       }
#'       
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       high_conditions <- c(high_conditions, pattern_condition)
#'     }
#'     
#'     # Continue if NOT matching any high-risk pattern  
#'     if (length(high_conditions) > 0) {
#'       not_high_risk <- paste0("!(", paste(high_conditions, collapse = " or "), ")")
#'       continue_conditions <- c(continue_conditions, not_high_risk)
#'     }
#'   }
#'   
#'   # Combine all continue conditions
#'   if (length(continue_conditions) > 0) {
#'     return(paste(continue_conditions, collapse = " and "))
#'   }
#'   
#'   return(NULL)
#' }

#' #' Generate Pattern-Based Visibility Condition (FIXED FOR MULTI-CONSTRUCT)
#' #'
#' #' @param prev_items Previous items administered FROM THE SAME CONSTRUCT
#' #' @param pattern_rules Pattern rules for current position
#' #' @param stop_low_only Whether only low-risk stopping is allowed
#' #' @return SurveyJS visibility condition string
#' generate_pattern_visibility_condition <- function(prev_items, pattern_rules, 
#'                                                   stop_low_only = FALSE) {
#'   
#'   if (is.null(pattern_rules) || length(prev_items) == 0) {
#'     return(NULL)  # No pattern rules for this position
#'   }
#'   
#'   # Get pattern info
#'   low_patterns <- pattern_rules$low_risk_patterns
#'   high_patterns <- pattern_rules$high_risk_patterns
#'   
#'   if (length(low_patterns) == 0 && length(high_patterns) == 0) {
#'     return(NULL)  # No stopping patterns at this position
#'   }
#'   
#'   # Build conditions for continuing (NOT stopping)
#'   continue_conditions <- character()
#'   
#'   # Low-risk patterns - continue if NOT matching any low-risk pattern
#'   if (length(low_patterns) > 0) {
#'     low_conditions <- character()
#'     
#'     for (pattern_name in names(low_patterns)) {
#'       pattern_info <- low_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       # CRITICAL: Only check as many items as we have available
#'       if (length(scores) != length(prev_items)) {
#'         next  # Skip if pattern length doesn't match
#'       }
#'       
#'       # Create condition for this specific pattern
#'       pattern_checks <- character()
#'       for (i in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[i], "} == ", scores[i]))
#'       }
#'       
#'       # This pattern would trigger LOW risk stop
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       low_conditions <- c(low_conditions, pattern_condition)
#'     }
#'     
#'     # Continue if NOT matching any low-risk pattern
#'     if (length(low_conditions) > 0) {
#'       not_low_risk <- paste0("!(", paste(low_conditions, collapse = " or "), ")")
#'       continue_conditions <- c(continue_conditions, not_low_risk)
#'     }
#'   }
#'   
#'   # High-risk patterns - continue if NOT matching any high-risk pattern
#'   if (!stop_low_only && length(high_patterns) > 0) {
#'     high_conditions <- character()
#'     
#'     for (pattern_name in names(high_patterns)) {
#'       pattern_info <- high_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       # CRITICAL: Only check patterns that match our item count
#'       if (length(scores) != length(prev_items)) {
#'         next
#'       }
#'       
#'       pattern_checks <- character()
#'       for (i in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[i], "} == ", scores[i]))
#'       }
#'       
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       high_conditions <- c(high_conditions, pattern_condition)
#'     }
#'     
#'     # Continue if NOT matching any high-risk pattern  
#'     if (length(high_conditions) > 0) {
#'       not_high_risk <- paste0("!(", paste(high_conditions, collapse = " or "), ")")
#'       continue_conditions <- c(continue_conditions, not_high_risk)
#'     }
#'   }
#'   
#'   # Combine all continue conditions
#'   if (length(continue_conditions) > 0) {
#'     return(paste(continue_conditions, collapse = " and "))
#'   }
#'   
#'   return(NULL)
#' }

#' #' Generate Pattern Visibility that Checks Item Existence
#' generate_pattern_visibility_with_existence_check <- function(prev_items, pattern_rules, 
#'                                                              stop_low_only = FALSE) {
#'   
#'   if (length(prev_items) == 0 || is.null(pattern_rules)) {
#'     return(NULL)
#'   }
#'   
#'   conditions <- character()
#'   
#'   # First, ensure the previous item exists (was shown)
#'   # In SurveyJS, we check if it has a valid response value
#'   if (length(prev_items) > 1) {
#'     last_item <- prev_items[length(prev_items)]
#'     # Check that the last item has a valid value (0, 1, 2, or 3)
#'     existence_check <- paste0(
#'       "({", last_item, "} == 0 or {", last_item, "} == 1 or {", 
#'       last_item, "} == 2 or {", last_item, "} == 3)"
#'     )
#'     conditions <- c(conditions, existence_check)
#'   }
#'   
#'   # Now check stopping patterns
#'   stop_conditions <- character()
#'   
#'   # Only check patterns where all previous items were shown
#'   # (if they weren't shown, we wouldn't be here)
#'   if (length(pattern_rules$low_risk_patterns) > 0) {
#'     for (pattern_name in names(pattern_rules$low_risk_patterns)) {
#'       pattern_info <- pattern_rules$low_risk_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       if (length(scores) != length(prev_items)) next
#'       
#'       pattern_checks <- character()
#'       for (j in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[j], "} == ", scores[j]))
#'       }
#'       
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       stop_conditions <- c(stop_conditions, pattern_condition)
#'     }
#'   }
#'   
#'   # Similar for high-risk patterns
#'   if (!stop_low_only && length(pattern_rules$high_risk_patterns) > 0) {
#'     for (pattern_name in names(pattern_rules$high_risk_patterns)) {
#'       pattern_info <- pattern_rules$high_risk_patterns[[pattern_name]]
#'       scores <- pattern_info$scores
#'       
#'       if (length(scores) != length(prev_items)) next
#'       
#'       pattern_checks <- character()
#'       for (j in seq_along(scores)) {
#'         pattern_checks <- c(pattern_checks, 
#'                             paste0("{", prev_items[j], "} == ", scores[j]))
#'       }
#'       
#'       pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'       stop_conditions <- c(stop_conditions, pattern_condition)
#'     }
#'   }
#'   
#'   # Add the stopping check
#'   if (length(stop_conditions) > 0) {
#'     conditions <- c(conditions, paste0("!(", paste(stop_conditions, collapse = " or "), ")"))
#'   }
#'   
#'   # Combine: item is visible if previous item was shown AND pattern doesn't stop
#'   if (length(conditions) > 0) {
#'     if (length(conditions) == 1) {
#'       return(conditions[1])
#'     } else {
#'       return(paste0("(", conditions[1], ") and (", conditions[2], ")"))
#'     }
#'   }
#'   
#'   return(NULL)
#' }

#' Generate Pattern Visibility with Affirmative Logic
generate_pattern_visibility_affirmative <- function(prev_items, pattern_rules, 
                                                    stop_low_only = FALSE) {
  
  if (length(prev_items) == 0 || is.null(pattern_rules)) {
    return(NULL)
  }
  
  # Get all possible patterns for this position
  all_patterns <- character()
  n_items <- length(prev_items)
  
  # Generate all possible patterns (0-3 for each item)
  for (i in 0:(4^n_items - 1)) {
    pattern <- numeric(n_items)
    temp <- i
    for (j in 1:n_items) {
      pattern[j] <- temp %% 4
      temp <- temp %/% 4
    }
    all_patterns <- c(all_patterns, paste(pattern, collapse="_"))
  }
  
  # Identify which patterns trigger stopping
  stop_patterns <- character()
  
  # Add low-risk stop patterns
  if (length(pattern_rules$low_risk_patterns) > 0) {
    stop_patterns <- c(stop_patterns, names(pattern_rules$low_risk_patterns))
  }
  
  # Add high-risk stop patterns if not stop_low_only
  if (!stop_low_only && length(pattern_rules$high_risk_patterns) > 0) {
    stop_patterns <- c(stop_patterns, names(pattern_rules$high_risk_patterns))
  }
  
  # Continuation patterns are all patterns that DON'T trigger stopping
  continuation_patterns <- setdiff(all_patterns, stop_patterns)
  
  if (length(continuation_patterns) == 0) {
    # All patterns trigger stopping - item should never be shown
    return("false")
  }
  
  # Build affirmative conditions for each continuation pattern
  continuation_conditions <- character()
  
  for (pattern_str in continuation_patterns) {
    scores <- as.numeric(strsplit(pattern_str, "_")[[1]])
    
    pattern_checks <- character()
    for (j in seq_along(scores)) {
      pattern_checks <- c(pattern_checks, 
                          paste0("{", prev_items[j], "} == ", scores[j]))
    }
    
    # This pattern allows continuation
    pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
    continuation_conditions <- c(continuation_conditions, pattern_condition)
  }
  
  # Item is visible if ANY continuation pattern matches
  # If an item wasn't shown, {item} == value will be false, making the whole pattern false
  return(paste0("(", paste(continuation_conditions, collapse = " or "), ")"))
}

#' Generate SurveyJS JSON Configuration (ENHANCED WITH PATTERN SUPPORT)
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param boundary_tables Boundary tables
#' @param admin_sequence Administration sequence
#' @param item_definitions List of item definitions
#' @param survey_config Survey configuration
#' @param pattern_rules Pattern-specific rules (if applicable)
#' @param output_dir Output directory
#' @return List containing SurveyJS configuration
generate_surveyjs_json <- function(method_results, prepared_data, boundary_tables,
                                   admin_sequence, item_definitions, survey_config,
                                   pattern_rules = NULL, output_dir) {
  
  # Initialize survey structure
  survey <- list(
    title = survey_config$title,
    description = survey_config$description,
    pages = list()
  )
  
  # Get method details
  reduction_method <- method_results$combination$reduction
  stop_low_only <- method_results$reduction_result$constraints_applied$stop_low_only %||% FALSE
  use_patterns <- !is.null(pattern_rules) && reduction_method == "sc_ep"
  
  # Process based on questionnaire type
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    # Single construct - simpler logic
    if (use_patterns) {
      survey$pages <- generate_unidimensional_pages_patterns(
        admin_sequence,
        pattern_rules[["total"]],
        item_definitions,
        survey_config,
        reduction_method,
        stop_low_only
      )
    } else {
      survey$pages <- generate_unidimensional_pages(
        admin_sequence,
        boundary_tables[["total"]],
        item_definitions,
        survey_config,
        reduction_method,
        stop_low_only
      )
    }
  } else {
    # Multi-construct - complex continuation logic
    if (use_patterns) {
      # survey$pages <- generate_multi_construct_pages_patterns(
      # survey$pages <- generate_multi_construct_pages_patterns_fixed(
      survey$pages <- generate_multi_construct_pages_patterns_fixed_v2(
        admin_sequence,
        pattern_rules,
        item_definitions,
        survey_config,
        prepared_data$config,
        method_results,
        reduction_method,
        stop_low_only
      )
    } else {
      survey$pages <- generate_multi_construct_pages(
        admin_sequence,
        boundary_tables,
        item_definitions,
        survey_config,
        prepared_data$config,
        method_results,
        reduction_method,
        stop_low_only
      )
    }
  }
  
  # Add completion page
  survey$pages[[length(survey$pages) + 1]] <- list(
    name = "completion",
    elements = list(
      list(
        type = "html",
        name = "completion_message",
        html = "<h3>Thank you for completing the assessment</h3>"
      )
    )
  )
  
  # Add survey-level settings
  survey$questionErrorLocation = "bottom"
  survey$showProgressBar = TRUE
  survey$progressBarLocation = "belowheader"
  survey$progressBarType = "questions"
  survey$autoAdvanceEnabled = TRUE
  survey$autoAdvanceAllowComplete = FALSE
  survey$widthMode = "responsive"
  survey$headerView = "advanced"
  
  # Convert to JSON
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json_output <- jsonlite::toJSON(survey, pretty = TRUE, auto_unbox = TRUE)
    
    # Save JSON file
    writeLines(json_output, file.path(output_dir, "surveyjs_config.json"))
    
    # Also save a version with example HTML wrapper
    generate_surveyjs_html(json_output, output_dir)
  } else {
    warning("jsonlite package not available. JSON output not generated.")
    json_output <- NULL
  }
  
  return(survey)
}

#' Generate Pages for Unidimensional with Pattern Rules (NEW FUNCTION)
#'
#' @param admin_sequence Administration sequence
#' @param pattern_rules Pattern rules for this questionnaire
#' @param item_definitions Item definitions
#' @param survey_config Survey configuration
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @return List of pages
generate_unidimensional_pages_patterns <- function(admin_sequence, pattern_rules,
                                                   item_definitions, survey_config,
                                                   reduction_method, stop_low_only) {
  pages <- list()
  
  for (i in 1:nrow(admin_sequence)) {
    item_id <- admin_sequence$item_id[i]
    original_pos <- admin_sequence$original_position[i]
    
    # Create page for this item
    page <- list(
      name = as.character(original_pos),
      elements = list()
    )
    
    # Initialize question with proper field ordering
    question <- list()
    
    # 1. type (always first)
    question$type <- "rating"
    
    # 2. name
    question$name <- item_id
    
    # 3. visibleIf (if applicable) - PATTERN-BASED
    visibility_condition <- NULL
    
    # Generate pattern-based visibility condition for non-first items
    if (i > 1 && reduction_method == "sc_ep" && !is.null(pattern_rules)) {
      prev_items <- admin_sequence$item_id[1:(i-1)]
      
      # Get pattern rules for previous position
      if (i-1 <= length(pattern_rules)) {
        visibility_condition <- generate_pattern_visibility_condition(
          prev_items = prev_items,
          pattern_rules = pattern_rules[[i-1]],
          stop_low_only = stop_low_only
        )
      }
    }
    
    # Add visibleIf if condition exists
    if (!is.null(visibility_condition)) {
      question$visibleIf <- visibility_condition
    }
    
    # 4. title
    question$title <- item_definitions[[item_id]]$item_text
    
    # 5. requiredIf (mirrors visibleIf for conditional items)
    if (!is.null(visibility_condition)) {
      question$requiredIf <- visibility_condition
    }
    
    # 6. isRequired
    # Only the first item is unconditionally required
    if (i == 1) {
      question$isRequired <- TRUE
    }
    
    # 7. autoGenerate
    question$autoGenerate <- survey_config$autoGenerate %||% FALSE
    
    # 8. rateValues
    question$rateValues <- item_definitions[[item_id]]$rateValues
    
    # 9. displayMode
    question$displayMode <- survey_config$displayMode %||% "buttons"
    
    page$elements[[1]] <- question
    pages[[i]] <- page
  }
  
  return(pages)
}

#' #' Generate Pages for Multi-construct with Pattern Rules (NEW FUNCTION)
#' #'
#' #' @param admin_sequence Administration sequence
#' #' @param pattern_rules Pattern rules by construct
#' #' @param item_definitions Item definitions
#' #' @param survey_config Survey configuration
#' #' @param data_config Data configuration
#' #' @param method_results Method results
#' #' @param reduction_method Reduction method
#' #' @param stop_low_only Stop low only flag
#' #' @return List of pages
#' generate_multi_construct_pages_patterns <- function(admin_sequence, pattern_rules,
#'                                                     item_definitions, survey_config,
#'                                                     data_config, method_results,
#'                                                     reduction_method, stop_low_only) {
#'   pages <- list()
#'   
#'   # Track which items from each construct have been presented
#'   construct_item_counts <- list()
#'   for (cn in names(data_config$constructs)) {
#'     construct_item_counts[[cn]] <- 0
#'   }
#'   
#'   # Get min_items_per_construct
#'   min_items_per_construct <- method_results$reduction_result$constraints_applied$min_items_per_construct %||% 
#'     data_config$constraints$min_items_per_construct %||% 
#'     0
#'   
#'   for (i in 1:nrow(admin_sequence)) {
#'     item_id <- admin_sequence$item_id[i]
#'     original_pos <- admin_sequence$original_position[i]
#'     item_construct <- admin_sequence$construct[i]
#'     
#'     # Update construct item count
#'     construct_item_counts[[item_construct]] <- construct_item_counts[[item_construct]] + 1
#'     within_construct_pos <- construct_item_counts[[item_construct]]
#'     
#'     # Create page
#'     page <- list(
#'       name = as.character(original_pos),
#'       elements = list()
#'     )
#'     
#'     # Initialize question
#'     question <- list()
#'     question$type <- "rating"
#'     question$name <- item_id
#'     
#'     # Generate pattern-based visibility condition
#'     visibility_condition <- NULL
#'     
#'     if (within_construct_pos > 1 && reduction_method == "sc_ep" && 
#'         !is.null(pattern_rules[[item_construct]])) {
#'       # Get previous items FROM THE SAME CONSTRUCT
#'       prev_construct_items <- admin_sequence$item_id[
#'         admin_sequence$construct == item_construct &
#'           1:nrow(admin_sequence) < i
#'       ]
#'       
#'       # Get pattern rules for this construct at this position
#'       construct_pattern_rules <- pattern_rules[[item_construct]]
#'       
#'       if (length(prev_construct_items) > 0 && 
#'           length(prev_construct_items) <= length(construct_pattern_rules)) {
#'         visibility_condition <- generate_pattern_visibility_condition(
#'           prev_items = prev_construct_items,
#'           pattern_rules = construct_pattern_rules[[length(prev_construct_items)]],
#'           stop_low_only = stop_low_only
#'         )
#'       }
#'     }
#'     
#'     # Add visibleIf if condition exists (but not for unconditionally required items)
#'     if (i == 1 || (min_items_per_construct > 0 && within_construct_pos <= min_items_per_construct)) {
#'       # No visibleIf for unconditionally required items
#'     } else if (!is.null(visibility_condition)) {
#'       question$visibleIf <- visibility_condition
#'     }
#'     
#'     # Rest of question setup
#'     question$title <- item_definitions[[item_id]]$item_text
#'     
#'     if (!is.null(question$visibleIf)) {
#'       question$requiredIf <- question$visibleIf
#'     }
#'     
#'     if (i == 1 || (min_items_per_construct > 0 && 
#'                    within_construct_pos <= min_items_per_construct)) {
#'       question$isRequired <- TRUE
#'     }
#'     
#'     question$autoGenerate <- survey_config$autoGenerate %||% FALSE
#'     question$rateValues <- item_definitions[[item_id]]$rateValues
#'     question$displayMode <- survey_config$displayMode %||% "buttons"
#'     
#'     page$elements[[1]] <- question
#'     pages[[i]] <- page
#'   }
#'   
#'   return(pages)
#' }

#' #' Generate Cumulative Pattern Visibility Condition (FIXED)
#' #'
#' #' Creates a visibility condition that checks if we should have stopped at any previous position
#' #'
#' #' @param construct_items_so_far All items from this construct seen so far
#' #' @param pattern_rules Pattern rules for this construct
#' #' @param stop_low_only Whether only low-risk stopping is allowed
#' #' @return SurveyJS visibility condition string
#' generate_cumulative_pattern_visibility <- function(construct_items_so_far, 
#'                                                    pattern_rules, 
#'                                                    stop_low_only = FALSE) {
#'   
#'   if (length(construct_items_so_far) == 0 || is.null(pattern_rules)) {
#'     return(NULL)
#'   }
#'   
#'   # We need to check if we SHOULD HAVE STOPPED at any previous position
#'   # This means checking each position k with ONLY the first k items
#'   
#'   stop_conditions <- character()
#'   
#'   # Check each previous position
#'   for (k in 1:length(construct_items_so_far)) {
#'     if (k > length(pattern_rules)) next
#'     
#'     pos_rules <- pattern_rules[[k]]
#'     if (is.null(pos_rules)) next
#'     
#'     # Get ONLY the first k items for checking position k
#'     items_at_k <- construct_items_so_far[1:k]
#'     
#'     # Collect patterns that would have stopped at position k
#'     position_stop_conditions <- character()
#'     
#'     # Low-risk patterns at position k
#'     if (!is.null(pos_rules$low_risk_patterns)) {
#'       for (pattern_name in names(pos_rules$low_risk_patterns)) {
#'         pattern_info <- pos_rules$low_risk_patterns[[pattern_name]]
#'         scores <- pattern_info$scores
#'         
#'         # CRITICAL: Only check patterns that match the number of items at position k
#'         if (length(scores) == k) {
#'           checks <- character()
#'           for (j in 1:k) {
#'             checks <- c(checks, paste0("{", items_at_k[j], "} == ", scores[j]))
#'           }
#'           pattern_cond <- paste0("(", paste(checks, collapse = " and "), ")")
#'           position_stop_conditions <- c(position_stop_conditions, pattern_cond)
#'         }
#'       }
#'     }
#'     
#'     # High-risk patterns at position k (if not stop_low_only)
#'     if (!stop_low_only && !is.null(pos_rules$high_risk_patterns)) {
#'       for (pattern_name in names(pos_rules$high_risk_patterns)) {
#'         pattern_info <- pos_rules$high_risk_patterns[[pattern_name]]
#'         scores <- pattern_info$scores
#'         
#'         # CRITICAL: Only check patterns that match the number of items at position k
#'         if (length(scores) == k) {
#'           checks <- character()
#'           for (j in 1:k) {
#'             checks <- c(checks, paste0("{", items_at_k[j], "} == ", scores[j]))
#'           }
#'           pattern_cond <- paste0("(", paste(checks, collapse = " and "), ")")
#'           position_stop_conditions <- c(position_stop_conditions, pattern_cond)
#'         }
#'       }
#'     }
#'     
#'     # Add all stop conditions for this position
#'     if (length(position_stop_conditions) > 0) {
#'       stop_conditions <- c(stop_conditions, position_stop_conditions)
#'     }
#'   }
#'   
#'   # Return condition to CONTINUE (none of the stop conditions were met)
#'   if (length(stop_conditions) > 0) {
#'     # Continue if NONE of the stopping conditions at ANY previous position were met
#'     return(paste0("!(", paste(stop_conditions, collapse = " or "), ")"))
#'   }
#'   
#'   return(NULL)
#' }

#' #' Generate Cumulative Pattern Visibility Condition (FIXED)
#' #'
#' #' Creates a visibility condition that checks if we should have stopped at any previous position
#' #'
#' #' @param construct_items_so_far All items from this construct seen so far
#' #' @param pattern_rules Pattern rules for this construct
#' #' @param stop_low_only Whether only low-risk stopping is allowed
#' #' @return SurveyJS visibility condition string
#' generate_cumulative_pattern_visibility <- function(construct_items_so_far, 
#'                                                    pattern_rules, 
#'                                                    stop_low_only = FALSE) {
#'   
#'   if (length(construct_items_so_far) == 0 || is.null(pattern_rules)) {
#'     return(NULL)
#'   }
#'   
#'   # We need to check if we SHOULD HAVE STOPPED at any previous position
#'   # This means checking each position k with ONLY the first k items
#'   
#'   stop_conditions <- character()
#'   
#'   # Check each previous position
#'   for (k in 1:length(construct_items_so_far)) {
#'     if (k > length(pattern_rules)) next
#'     
#'     pos_rules <- pattern_rules[[k]]
#'     if (is.null(pos_rules)) next
#'     
#'     # Get ONLY the first k items for checking position k
#'     items_at_k <- construct_items_so_far[1:k]
#'     
#'     # Collect all patterns that would trigger a stop at position k
#'     position_stop_conditions <- character()
#'     
#'     # Check low-risk patterns
#'     if (length(pos_rules$low_risk_patterns) > 0) {
#'       for (pattern_name in names(pos_rules$low_risk_patterns)) {
#'         pattern_info <- pos_rules$low_risk_patterns[[pattern_name]]
#'         scores <- pattern_info$scores
#'         
#'         # Create condition for this specific pattern
#'         pattern_checks <- character()
#'         for (j in seq_along(scores)) {
#'           if (j <= length(items_at_k)) {
#'             pattern_checks <- c(pattern_checks, 
#'                                 paste0("{", items_at_k[j], "} == ", scores[j]))
#'           }
#'         }
#'         
#'         # Only add if we have all necessary checks
#'         if (length(pattern_checks) == length(scores)) {
#'           pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'           position_stop_conditions <- c(position_stop_conditions, pattern_condition)
#'         }
#'       }
#'     }
#'     
#'     # Check high-risk patterns (if not stop_low_only)
#'     if (!stop_low_only && length(pos_rules$high_risk_patterns) > 0) {
#'       for (pattern_name in names(pos_rules$high_risk_patterns)) {
#'         pattern_info <- pos_rules$high_risk_patterns[[pattern_name]]
#'         scores <- pattern_info$scores
#'         
#'         pattern_checks <- character()
#'         for (j in seq_along(scores)) {
#'           if (j <= length(items_at_k)) {
#'             pattern_checks <- c(pattern_checks, 
#'                                 paste0("{", items_at_k[j], "} == ", scores[j]))
#'           }
#'         }
#'         
#'         if (length(pattern_checks) == length(scores)) {
#'           pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
#'           position_stop_conditions <- c(position_stop_conditions, pattern_condition)
#'         }
#'       }
#'     }
#'     
#'     # If there are stop conditions for this position, add them
#'     if (length(position_stop_conditions) > 0) {
#'       # Any of these patterns at position k would have triggered a stop
#'       position_stop <- paste0("(", paste(position_stop_conditions, collapse = " or "), ")")
#'       stop_conditions <- c(stop_conditions, position_stop)
#'     }
#'   }
#'   
#'   # The item should be visible (continue) only if NONE of the stop conditions were met
#'   if (length(stop_conditions) > 0) {
#'     # Continue if NOT matching any stopping condition at any previous position
#'     return(paste0("!(", paste(stop_conditions, collapse = " or "), ")"))
#'   }
#'   
#'   return(NULL)
#' }

#' Generate Cumulative Pattern Visibility Condition (FULLY FIXED)
#'
#' This checks if we should have stopped at ANY previous position
generate_cumulative_pattern_visibility_v2 <- function(construct_items_so_far, 
                                                      pattern_rules, 
                                                      stop_low_only = FALSE) {
  
  if (length(construct_items_so_far) == 0 || is.null(pattern_rules)) {
    return(NULL)
  }
  
  # Collect ALL stop conditions from ALL previous positions
  all_stop_conditions <- character()
  
  # Check each position from 1 to current-1
  for (k in 1:length(construct_items_so_far)) {
    if (k > length(pattern_rules)) next
    
    pos_rules <- pattern_rules[[k]]
    if (is.null(pos_rules)) next
    
    # Get ONLY the first k items for checking position k
    items_at_k <- construct_items_so_far[1:k]
    
    # Get stop conditions for this position
    position_conditions <- character()
    
    # Check low-risk patterns
    if (length(pos_rules$low_risk_patterns) > 0) {
      for (pattern_name in names(pos_rules$low_risk_patterns)) {
        pattern_info <- pos_rules$low_risk_patterns[[pattern_name]]
        scores <- pattern_info$scores
        
        # Only check patterns that match our number of items
        if (length(scores) != k) next
        
        # Build the condition for this pattern
        pattern_checks <- character()
        for (j in 1:k) {
          pattern_checks <- c(pattern_checks, 
                              paste0("{", items_at_k[j], "} == ", scores[j]))
        }
        
        # This specific pattern would trigger a stop
        pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
        position_conditions <- c(position_conditions, pattern_condition)
      }
    }
    
    # Check high-risk patterns (if not stop_low_only)
    if (!stop_low_only && length(pos_rules$high_risk_patterns) > 0) {
      for (pattern_name in names(pos_rules$high_risk_patterns)) {
        pattern_info <- pos_rules$high_risk_patterns[[pattern_name]]
        scores <- pattern_info$scores
        
        if (length(scores) != k) next
        
        pattern_checks <- character()
        for (j in 1:k) {
          pattern_checks <- c(pattern_checks, 
                              paste0("{", items_at_k[j], "} == ", scores[j]))
        }
        
        pattern_condition <- paste0("(", paste(pattern_checks, collapse = " and "), ")")
        position_conditions <- c(position_conditions, pattern_condition)
      }
    }
    
    # Add conditions from this position to the overall list
    if (length(position_conditions) > 0) {
      # Any of these patterns at position k would trigger a stop
      position_stop <- paste0("(", paste(position_conditions, collapse = " or "), ")")
      all_stop_conditions <- c(all_stop_conditions, position_stop)
    }
  }
  
  # The item should be visible only if NONE of the stop conditions were met
  if (length(all_stop_conditions) > 0) {
    return(paste0("!(", paste(all_stop_conditions, collapse = " or "), ")"))
  }
  
  return(NULL)
}

#' #' Replace the generate_multi_construct_pages_patterns function in Module 7
#' #' 
#' #' This is the corrected version with proper cumulative visibility
#' generate_multi_construct_pages_patterns <- function(admin_sequence, pattern_rules,
#'                                                               item_definitions, survey_config,
#'                                                               data_config, method_results,
#'                                                               reduction_method, stop_low_only) {
#'   pages <- list()
#'   
#'   # Track items by construct
#'   construct_items_seen <- list()
#'   for (cn in names(data_config$constructs)) {
#'     construct_items_seen[[cn]] <- character()
#'   }
#'   
#'   # Get min_items_per_construct
#'   min_items_per_construct <- method_results$reduction_result$constraints_applied$min_items_per_construct %||% 
#'     data_config$constraints$min_items_per_construct %||% 
#'     0
#'   
#'   for (i in 1:nrow(admin_sequence)) {
#'     item_id <- admin_sequence$item_id[i]
#'     original_pos <- admin_sequence$original_position[i]
#'     item_construct <- admin_sequence$construct[i]
#'     
#'     # Track this item for its construct
#'     construct_items_seen[[item_construct]] <- c(construct_items_seen[[item_construct]], item_id)
#'     within_construct_pos <- length(construct_items_seen[[item_construct]])
#'     
#'     # Create page
#'     page <- list(
#'       name = as.character(original_pos),
#'       elements = list()
#'     )
#'     
#'     # Initialize question
#'     question <- list()
#'     question$type <- "rating"
#'     question$name <- item_id
#'     
#'     # Generate CUMULATIVE visibility condition
#'     visibility_condition <- NULL
#'     
#'     # Only add visibility conditions after minimum items and for non-first items
#'     if (within_construct_pos > 1 && 
#'         within_construct_pos > min_items_per_construct &&
#'         reduction_method == "sc_ep" && 
#'         !is.null(pattern_rules[[item_construct]])) {
#'       
#'       # Get all PREVIOUS items from this construct (not including current)
#'       prev_construct_items <- construct_items_seen[[item_construct]][1:(within_construct_pos-1)]
#'       
#'       # Generate cumulative condition
#'       visibility_condition <- generate_cumulative_pattern_visibility(
#'         construct_items_so_far = prev_construct_items,
#'         pattern_rules = pattern_rules[[item_construct]],
#'         stop_low_only = stop_low_only
#'       )
#'     }
#'     
#'     # Add visibleIf only if we have a condition and item is not mandatory
#'     if (!is.null(visibility_condition)) {
#'       question$visibleIf <- visibility_condition
#'       question$requiredIf <- visibility_condition
#'     } else if (i == 1 || (min_items_per_construct > 0 && within_construct_pos <= min_items_per_construct)) {
#'       # Mandatory items are always required
#'       question$isRequired <- TRUE
#'     }
#'     
#'     # Rest of question setup
#'     question$title <- item_definitions[[item_id]]$item_text
#'     question$autoGenerate <- survey_config$autoGenerate %||% FALSE
#'     question$rateValues <- item_definitions[[item_id]]$rateValues
#'     question$displayMode <- survey_config$displayMode %||% "buttons"
#'     
#'     page$elements[[1]] <- question
#'     pages[[i]] <- page
#'   }
#'   
#'   return(pages)
#' }

#' #' Fixed Multi-Construct Pages Generation with Correct Item Tracking
#' #'
#' #' This ensures visibility conditions only reference items from the SAME construct
#' generate_multi_construct_pages_patterns_fixed <- function(admin_sequence, pattern_rules,
#'                                                           item_definitions, survey_config,
#'                                                           data_config, method_results,
#'                                                           reduction_method, stop_low_only) {
#'   pages <- list()
#'   
#'   # Track items seen by construct - CRITICAL for correct visibility
#'   construct_items_administered <- list()
#'   for (cn in names(data_config$constructs)) {
#'     construct_items_administered[[cn]] <- character()
#'   }
#'   
#'   # Get min_items_per_construct
#'   min_items_per_construct <- method_results$reduction_result$constraints_applied$min_items_per_construct %||% 
#'     data_config$constraints$min_items_per_construct %||% 
#'     0
#'   
#'   for (i in 1:nrow(admin_sequence)) {
#'     item_id <- admin_sequence$item_id[i]
#'     original_pos <- admin_sequence$original_position[i]
#'     item_construct <- admin_sequence$construct[i]
#'     
#'     # Track this item for its construct
#'     construct_items_administered[[item_construct]] <- c(construct_items_administered[[item_construct]], item_id)
#'     within_construct_pos <- length(construct_items_administered[[item_construct]])
#'     
#'     # Create page
#'     page <- list(
#'       name = as.character(original_pos),
#'       elements = list()
#'     )
#'     
#'     # Initialize question
#'     question <- list()
#'     question$type <- "rating"
#'     question$name <- item_id
#'     
#'     # Generate visibility condition based on SAME CONSTRUCT items only
#'     visibility_condition <- NULL
#'     
#'     # Only add visibility after minimum items per construct
#'     if (within_construct_pos > 1 && 
#'         within_construct_pos > min_items_per_construct &&
#'         reduction_method == "sc_ep" && 
#'         !is.null(pattern_rules[[item_construct]])) {
#'       
#'       # Get PREVIOUS items from THIS CONSTRUCT ONLY (not including current)
#'       prev_construct_items <- construct_items_administered[[item_construct]][1:(within_construct_pos-1)]
#'       
#'       # Get the pattern rules for this construct at the previous position
#'       construct_rules <- pattern_rules[[item_construct]]
#'       position_to_check <- length(prev_construct_items)
#'       
#'       if (position_to_check <= length(construct_rules) && 
#'           !is.null(construct_rules[[position_to_check]])) {
#'         
#'         # Generate visibility condition for these specific items
#'         visibility_condition <- generate_pattern_visibility_condition(
#'           prev_items = prev_construct_items,
#'           pattern_rules = construct_rules[[position_to_check]],
#'           stop_low_only = stop_low_only
#'         )
#'       }
#'     }
#'     
#'     # Add visibleIf only if we have a condition and item is not mandatory
#'     if (!is.null(visibility_condition)) {
#'       question$visibleIf <- visibility_condition
#'       question$requiredIf <- visibility_condition
#'     } else if (i == 1 || (min_items_per_construct > 0 && within_construct_pos <= min_items_per_construct)) {
#'       # Mandatory items are always required
#'       question$isRequired <- TRUE
#'     }
#'     
#'     # Rest of question setup
#'     question$title <- item_definitions[[item_id]]$item_text
#'     question$autoGenerate <- survey_config$autoGenerate %||% FALSE
#'     question$rateValues <- item_definitions[[item_id]]$rateValues
#'     question$displayMode <- survey_config$displayMode %||% "buttons"
#'     
#'     page$elements[[1]] <- question
#'     pages[[i]] <- page
#'   }
#'   
#'   return(pages)
#' }

#' Generate Multi-Construct Pages with CUMULATIVE Pattern Checking
#'
#' This version properly accumulates stop conditions across all previous positions
generate_multi_construct_pages_patterns_fixed_v2 <- function(admin_sequence, pattern_rules,
                                                             item_definitions, survey_config,
                                                             data_config, method_results,
                                                             reduction_method, stop_low_only) {
  pages <- list()
  
  # Track items seen by construct
  construct_items_administered <- list()
  for (cn in names(data_config$constructs)) {
    construct_items_administered[[cn]] <- character()
  }
  
  # Get min_items_per_construct
  min_items_per_construct <- method_results$reduction_result$constraints_applied$min_items_per_construct %||% 
    data_config$constraints$min_items_per_construct %||% 
    0
  
  for (i in 1:nrow(admin_sequence)) {
    item_id <- admin_sequence$item_id[i]
    original_pos <- admin_sequence$original_position[i]
    item_construct <- admin_sequence$construct[i]
    
    # Track this item for its construct
    construct_items_administered[[item_construct]] <- c(construct_items_administered[[item_construct]], item_id)
    within_construct_pos <- length(construct_items_administered[[item_construct]])
    
    # Create page
    page <- list(
      name = as.character(original_pos),
      elements = list()
    )
    
    # Initialize question
    question <- list()
    question$type <- "rating"
    question$name <- item_id
    
    # Generate CUMULATIVE visibility condition
    visibility_condition <- NULL
    
    # Only add visibility after minimum items per construct
    if (within_construct_pos > 1 && 
        within_construct_pos > min_items_per_construct &&
        reduction_method == "sc_ep" && 
        !is.null(pattern_rules[[item_construct]])) {
      
      # Get PREVIOUS items from THIS CONSTRUCT ONLY (not including current)
      prev_construct_items <- construct_items_administered[[item_construct]][1:(within_construct_pos-1)]
      
      # # Use the CUMULATIVE function to check ALL previous positions
      # visibility_condition <- generate_cumulative_pattern_visibility_v2(
      #   construct_items_so_far = prev_construct_items,
      #   pattern_rules = pattern_rules[[item_construct]],
      #   stop_low_only = stop_low_only
      # )
      
      # # Get the pattern rules for the current position (k-1 items)
      # position_to_check <- length(prev_construct_items)
      # if (position_to_check <= length(pattern_rules[[item_construct]])) {
      #   visibility_condition <- generate_pattern_visibility_condition(
      #     prev_items = prev_construct_items,
      #     pattern_rules = pattern_rules[[item_construct]][[position_to_check]],
      #     stop_low_only = stop_low_only
      #   )
      # }
      
      # # Get the pattern rules for the current position (k-1 items)
      # position_to_check <- length(prev_construct_items)
      # if (position_to_check <= length(pattern_rules[[item_construct]])) {
      #   visibility_condition <- generate_pattern_visibility_with_existence_check(
      #     prev_items = prev_construct_items,
      #     pattern_rules = pattern_rules[[item_construct]][[position_to_check]],
      #     stop_low_only = stop_low_only
      #   )
      # }
      
      # Get the pattern rules for the current position (k-1 items)
      position_to_check <- length(prev_construct_items)
      if (position_to_check <= length(pattern_rules[[item_construct]])) {
        visibility_condition <- generate_pattern_visibility_affirmative(
          prev_items = prev_construct_items,
          pattern_rules = pattern_rules[[item_construct]][[position_to_check]],
          stop_low_only = stop_low_only
        )
      }
      
    }
    
    # Add visibleIf only if we have a condition and item is not mandatory
    if (!is.null(visibility_condition)) {
      question$visibleIf <- visibility_condition
      question$requiredIf <- visibility_condition
    } else if (i == 1 || (min_items_per_construct > 0 && within_construct_pos <= min_items_per_construct)) {
      # Mandatory items are always required
      question$isRequired <- TRUE
    }
    
    # Rest of question setup
    question$title <- item_definitions[[item_id]]$item_text
    question$autoGenerate <- survey_config$autoGenerate %||% FALSE
    question$rateValues <- item_definitions[[item_id]]$rateValues
    question$displayMode <- survey_config$displayMode %||% "buttons"
    
    page$elements[[1]] <- question
    pages[[i]] <- page
  }
  
  return(pages)
}

# [Keep all the original page generation functions for backward compatibility]
# generate_unidimensional_pages (original)
# generate_multi_construct_pages (original)

#' Generate Pages for Unidimensional Questionnaire (ORIGINAL - KEPT FOR COMPATIBILITY)
#'
#' @param admin_sequence Administration sequence
#' @param boundary_table Boundary table
#' @param item_definitions Item definitions
#' @param survey_config Survey configuration
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @return List of pages
generate_unidimensional_pages <- function(admin_sequence, boundary_table, item_definitions,
                                          survey_config, reduction_method, stop_low_only) {
  pages <- list()
  
  for (i in 1:nrow(admin_sequence)) {
    item_id <- admin_sequence$item_id[i]
    original_pos <- admin_sequence$original_position[i]
    
    # Create page for this item
    page <- list(
      name = as.character(original_pos),
      elements = list()
    )
    
    # Initialize question with proper field ordering
    question <- list()
    
    # 1. type (always first)
    question$type <- "rating"
    
    # 2. name
    question$name <- item_id
    
    # 3. visibleIf (if applicable)
    visibility_condition <- NULL
    
    # Generate visibility condition for non-first items
    if (i > 1 && reduction_method != "none") {
      # Get previous items
      prev_items <- admin_sequence$item_id[1:(i-1)]
      
      # Extract boundary for this position
      boundary_info <- boundary_table[i-1, ]
      
      # Create visibility condition based on reduction method
      if (reduction_method == "dc") {
        # For DC: visible if not conclusively classified
        visibility_parts <- c()
        
        if (!is.na(boundary_info$low_risk_boundary) &&
            boundary_info$low_risk_boundary != "N/A") {
          # Extract numeric boundary
          low_bound <- as.numeric(gsub("Xk <= ", "", boundary_info$low_risk_boundary))
          # Continue if sum > low_bound
          if (length(prev_items) == 1) {
            visibility_parts <- c(visibility_parts, paste0("{", prev_items[1], "} > ", low_bound))
          } else {
            visibility_parts <- c(visibility_parts, paste0("sum(",
                                                           paste("{", prev_items, "}", sep = "", collapse = ", "),
                                                           ") > ", low_bound))
          }
        }
        
        # Also check high risk boundary if not stop_low_only
        if (!stop_low_only && !is.na(boundary_info$high_risk_boundary) &&
            boundary_info$high_risk_boundary != "N/A") {
          high_bound <- as.numeric(gsub("Xk >= ", "", boundary_info$high_risk_boundary))
          # Continue if sum < high_bound
          if (length(prev_items) == 1) {
            visibility_parts <- c(visibility_parts, paste0("{", prev_items[1], "} < ", high_bound))
          } else {
            visibility_parts <- c(visibility_parts, paste0("sum(",
                                                           paste("{", prev_items, "}", sep = "", collapse = ", "),
                                                           ") < ", high_bound))
          }
        }
        
        # Combine conditions with "and"
        if (length(visibility_parts) > 0) {
          visibility_condition <- paste(visibility_parts, collapse = " and ")
        }
        
      } else if (reduction_method %in% c("sc_ep", "sc_sor")) {
        # Similar logic for SC methods
        if (!is.na(boundary_info$low_risk_boundary) &&
            boundary_info$low_risk_boundary != "N/A" &&
            !grepl("Pattern-dependent", boundary_info$low_risk_boundary)) {
          low_bound <- as.numeric(gsub("Xk <= ", "", boundary_info$low_risk_boundary))
          if (length(prev_items) == 1) {
            visibility_condition <- paste0("{", prev_items[1], "} > ", low_bound)
          } else {
            visibility_condition <- paste0("sum(",
                                           paste("{", prev_items, "}", sep = "", collapse = ", "),
                                           ") > ", low_bound)
          }
        }
        
        # Add high-risk boundary check if applicable
        if (!stop_low_only && !is.na(boundary_info$high_risk_boundary) &&
            boundary_info$high_risk_boundary != "N/A" &&
            !grepl("Pattern-dependent", boundary_info$high_risk_boundary)) {
          high_bound <- as.numeric(gsub("Xk >= ", "", boundary_info$high_risk_boundary))
          if (!is.null(visibility_condition)) {
            if (length(prev_items) == 1) {
              visibility_condition <- paste0(visibility_condition, " and {", 
                                             prev_items[1], "} < ", high_bound)
            } else {
              visibility_condition <- paste0(visibility_condition, " and sum(",
                                             paste("{", prev_items, "}", sep = "", collapse = ", "),
                                             ") < ", high_bound)
            }
          }
        }
      }
    }
    
    # Add visibleIf if condition exists
    if (!is.null(visibility_condition)) {
      question$visibleIf <- visibility_condition
    }
    
    # 4. title
    question$title <- item_definitions[[item_id]]$item_text
    
    # 5. requiredIf (mirrors visibleIf for conditional items)
    if (!is.null(visibility_condition)) {
      question$requiredIf <- visibility_condition
    }
    
    # 6. isRequired
    # Only the first item is unconditionally required
    if (i == 1) {
      question$isRequired <- TRUE
    }
    
    # 7. autoGenerate
    question$autoGenerate <- survey_config$autoGenerate %||% FALSE
    
    # 8. rateValues
    question$rateValues <- item_definitions[[item_id]]$rateValues
    
    # 9. displayMode
    question$displayMode <- survey_config$displayMode %||% "buttons"
    
    page$elements[[1]] <- question
    pages[[i]] <- page
  }
  
  return(pages)
}

#' Generate Pages for Multi-construct Questionnaire (ORIGINAL - KEPT FOR COMPATIBILITY)
#'
#' @param admin_sequence Administration sequence
#' @param boundary_tables Boundary tables by construct
#' @param item_definitions Item definitions
#' @param survey_config Survey configuration
#' @param data_config Data configuration
#' @param method_results Method results containing constraints_applied
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @return List of pages
generate_multi_construct_pages <- function(admin_sequence, boundary_tables, item_definitions,
                                           survey_config, data_config, method_results,
                                           reduction_method, stop_low_only) {
  pages <- list()
  
  # Track which items from each construct have been presented
  construct_item_counts <- list()
  for (cn in names(data_config$constructs)) {
    construct_item_counts[[cn]] <- 0
  }
  
  # Get min_items_per_construct from method_results
  # First try constraints_applied, then constraints from config
  min_items_per_construct <- method_results$reduction_result$constraints_applied$min_items_per_construct %||% 
    data_config$constraints$min_items_per_construct %||% 
    0
  
  # Track how many items from each construct have been included
  construct_items_included <- list()
  for (cn in names(data_config$constructs)) {
    construct_items_included[[cn]] <- 0
  }
  
  for (i in 1:nrow(admin_sequence)) {
    item_id <- admin_sequence$item_id[i]
    original_pos <- admin_sequence$original_position[i]
    item_construct <- admin_sequence$construct[i]
    
    # Update construct item count
    construct_item_counts[[item_construct]] <- construct_item_counts[[item_construct]] + 1
    within_construct_pos <- construct_item_counts[[item_construct]]
    
    # Update items included count
    construct_items_included[[item_construct]] <- construct_items_included[[item_construct]] + 1
    
    # Create page
    page <- list(
      name = as.character(original_pos),
      elements = list()
    )
    
    # Initialize question with proper field ordering
    question <- list()
    
    # 1. type (always first)
    question$type <- "rating"
    
    # 2. name
    question$name <- item_id
    
    # 3. visibleIf (if applicable)
    visibility_condition <- NULL
    
    # Generate visibility condition based on construct
    if (within_construct_pos > 1 && reduction_method != "none") {
      # Get previous items FROM THE SAME CONSTRUCT
      prev_construct_items <- admin_sequence$item_id[
        admin_sequence$construct == item_construct &
          1:nrow(admin_sequence) < i
      ]
      
      # Get boundary table for this construct
      construct_boundaries <- boundary_tables[[item_construct]]
      
      if (length(prev_construct_items) > 0 && !is.null(construct_boundaries)) {
        boundary_info <- construct_boundaries[length(prev_construct_items), ]
        
        # Create visibility condition based on reduction method
        visibility_parts <- c()
        
        if (reduction_method == "dc") {
          if (!is.na(boundary_info$low_risk_boundary) &&
              boundary_info$low_risk_boundary != "N/A") {
            low_bound <- as.numeric(gsub("Xk <= ", "", boundary_info$low_risk_boundary))
            if (length(prev_construct_items) == 1) {
              visibility_parts <- c(visibility_parts, 
                                    paste0("{", prev_construct_items[1], "} > ", low_bound))
            } else {
              visibility_parts <- c(visibility_parts,
                                    paste0("sum(",
                                           paste("{", prev_construct_items, "}", sep = "", collapse = ", "),
                                           ") > ", low_bound))
            }
          }
          
          if (!stop_low_only && !is.na(boundary_info$high_risk_boundary) &&
              boundary_info$high_risk_boundary != "N/A") {
            high_bound <- as.numeric(gsub("Xk >= ", "", boundary_info$high_risk_boundary))
            if (length(prev_construct_items) == 1) {
              visibility_parts <- c(visibility_parts,
                                    paste0("{", prev_construct_items[1], "} < ", high_bound))
            } else {
              visibility_parts <- c(visibility_parts,
                                    paste0("sum(",
                                           paste("{", prev_construct_items, "}", sep = "", collapse = ", "),
                                           ") < ", high_bound))
            }
          }
          
          if (length(visibility_parts) > 0) {
            visibility_condition <- paste(visibility_parts, collapse = " and ")
          }
          
        } else if (reduction_method %in% c("sc_ep", "sc_sor")) {
          if (!is.na(boundary_info$low_risk_boundary) &&
              boundary_info$low_risk_boundary != "N/A" &&
              !grepl("Pattern-dependent", boundary_info$low_risk_boundary)) {
            low_bound <- as.numeric(gsub("Xk <= ", "", boundary_info$low_risk_boundary))
            if (length(prev_construct_items) == 1) {
              visibility_condition <- paste0("{", prev_construct_items[1], "} > ", low_bound)
            } else {
              visibility_condition <- paste0("sum(",
                                             paste("{", prev_construct_items, "}", sep = "", collapse = ", "),
                                             ") > ", low_bound)
            }
          }
          
          # Add high-risk boundary check if applicable
          if (!stop_low_only && !is.na(boundary_info$high_risk_boundary) &&
              boundary_info$high_risk_boundary != "N/A" &&
              !grepl("Pattern-dependent", boundary_info$high_risk_boundary)) {
            high_bound <- as.numeric(gsub("Xk >= ", "", boundary_info$high_risk_boundary))
            if (!is.null(visibility_condition)) {
              if (length(prev_construct_items) == 1) {
                visibility_condition <- paste0(visibility_condition, " and {", 
                                               prev_construct_items[1], "} < ", high_bound)
              } else {
                visibility_condition <- paste0(visibility_condition, " and sum(",
                                               paste("{", prev_construct_items, "}", sep = "", collapse = ", "),
                                               ") < ", high_bound)
              }
            }
          }
        }
      }
    }
    
    # Add visibleIf if condition exists (but not for unconditionally required items)
    # First item overall is always visible
    # First min_items_per_construct items from each construct are always visible
    if (i == 1 || (min_items_per_construct > 0 && within_construct_pos <= min_items_per_construct)) {
      # No visibleIf for unconditionally required items
    } else if (!is.null(visibility_condition)) {
      question$visibleIf <- visibility_condition
    }
    
    # 4. title
    question$title <- item_definitions[[item_id]]$item_text
    
    # 5. requiredIf (mirrors visibleIf for conditional items)
    if (!is.null(question$visibleIf)) {
      question$requiredIf <- question$visibleIf
    }
    
    # 6. isRequired
    # First item overall is always required
    # First min_items_per_construct items from each construct are required
    if (i == 1) {
      question$isRequired <- TRUE
    } else if (min_items_per_construct > 0 && 
               within_construct_pos <= min_items_per_construct) {
      question$isRequired <- TRUE
    }
    
    # 7. autoGenerate
    question$autoGenerate <- survey_config$autoGenerate %||% FALSE
    
    # 8. rateValues
    question$rateValues <- item_definitions[[item_id]]$rateValues
    
    # 9. displayMode
    question$displayMode <- survey_config$displayMode %||% "buttons"
    
    page$elements[[1]] <- question
    pages[[i]] <- page
  }
  
  return(pages)
}

# [Keep all other helper functions unchanged]
# generate_boundary_tables_html
# generate_surveyjs_html
# generate_deployment_guide

#' Generate HTML Tables for Boundaries
#'
#' @param boundary_tables List of boundary tables
#' @param prepared_data Prepared data
#' @param output_dir Output directory
generate_boundary_tables_html <- function(boundary_tables, prepared_data, output_dir) {
  
  html_content <- paste0(
    "<html><head><title>Stopping Boundary Tables</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1, h2 { color: #333; }",
    "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".boundary-value { font-family: monospace; text-align: center; }",
    ".na-value { color: #999; text-align: center; }",
    ".note { background-color: #f0f0f0; padding: 10px; margin: 10px 0; border-left: 4px solid #333; }",
    "</style></head><body>",
    "<h1>Stopping Boundary Tables</h1>"
  )
  
  # Add method information
  html_content <- paste0(html_content,
                         "<div class='note'>",
                         "<strong>Method:</strong> ",
                         prepared_data$config$questionnaire_type, " questionnaire<br>",
                         "These tables show the cumulative score boundaries (Xk) for stopping item administration.",
                         "</div>"
  )
  
  # Process each boundary table
  for (construct_name in names(boundary_tables)) {
    boundary_table <- boundary_tables[[construct_name]]
    
    if (prepared_data$config$questionnaire_type == "multi-construct") {
      html_content <- paste0(html_content, "<h2>Construct: ", construct_name, "</h2>")
      
      # Add cutoff information
      cutoff <- prepared_data$config$cutoffs[[construct_name]]
      html_content <- paste0(html_content,
                             "<p><strong>Classification cutoff:</strong> ",
                             cutoff, " (scores >= ", cutoff, " are classified as high risk)</p>")
    }
    
    # Create table
    html_content <- paste0(html_content,
                           "<table>",
                           "<tr>",
                           "<th>Items Administered (k)</th>",
                           "<th>Low Risk Stopping Boundary</th>",
                           "<th>High Risk Stopping Boundary</th>",
                           "</tr>"
    )
    
    for (i in 1:nrow(boundary_table)) {
      low_class <- ifelse(boundary_table$low_risk_boundary[i] == "N/A",
                          "na-value", "boundary-value")
      high_class <- ifelse(boundary_table$high_risk_boundary[i] == "N/A",
                           "na-value", "boundary-value")
      
      html_content <- paste0(html_content,
                             "<tr>",
                             "<td>", boundary_table$items_administered[i], "</td>",
                             "<td class='", low_class, "'>",
                             boundary_table$low_risk_boundary[i], "</td>",
                             "<td class='", high_class, "'>",
                             boundary_table$high_risk_boundary[i], "</td>",
                             "</tr>"
      )
    }
    
    html_content <- paste0(html_content, "</table>")
  }
  
  html_content <- paste0(html_content, "</body></html>")
  
  writeLines(html_content, file.path(output_dir, "stopping_boundaries.html"))
}

#' Generate SurveyJS HTML Example
#'
#' @param json_config JSON configuration string
#' @param output_dir Output directory
generate_surveyjs_html <- function(json_config, output_dir) {
  
  html_content <- paste0(
    '<!DOCTYPE html>
<html>
<head>
    <title>Questionnaire</title>
    <script src="https://unpkg.com/survey-jquery/survey.jquery.min.js"></script>
    <link href="https://unpkg.com/survey-core/defaultV2.min.css" type="text/css" rel="stylesheet">
    <script src="https://unpkg.com/jquery/dist/jquery.min.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        #surveyContainer { max-width: 800px; margin: 0 auto; }
    </style>
</head>
<body>
    <div id="surveyContainer"></div>

    <script>
        const surveyJson = ',
    json_config,
    ';

        const survey = new Survey.Model(surveyJson);

        // Add completion handler
        survey.onComplete.add(function(sender) {
            console.log("Survey results:", sender.data);
            // Here you would typically send the data to your server
            alert("Thank you for completing the questionnaire!");
        });

        // Render survey
        $("#surveyContainer").Survey({
            model: survey
        });
    </script>
</body>
</html>'
  )
  
  writeLines(html_content, file.path(output_dir, "surveyjs_example.html"))
}

#' Generate Human-Readable Deployment Guide (ENHANCED WITH PATTERN INFO)
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param boundary_tables Boundary tables
#' @param admin_sequence Administration sequence
#' @param item_definitions List of item definitions
#' @param output_dir Output directory
#' @param is_optimized Whether this is an optimized method
#' @param optimization_results Full optimization results (if optimized)
#' @param pattern_rules Pattern rules (if applicable)
generate_deployment_guide <- function(method_results, prepared_data, boundary_tables,
                                      admin_sequence, item_definitions, output_dir,
                                      is_optimized = FALSE, optimization_results = NULL,
                                      pattern_rules = NULL) {
  
  guide_file <- file.path(output_dir, "deployment_guide.txt")
  
  # Extract method details
  method_id <- paste(method_results$combination$ordering,
                     method_results$combination$reduction, sep = "_")
  
  # Add gamma info to method_id if not optimized
  if (!is_optimized && !is.na(method_results$combination$gamma_0)) {
    method_id <- paste0(method_id, "_g0_", method_results$combination$gamma_0,
                        "_g1_", method_results$combination$gamma_1)
  } else if (is_optimized) {
    method_id <- paste0(method_id, "_optimized")
  }
  
  guide_text <- paste0(
    "DEPLOYMENT GUIDE FOR ADAPTIVE QUESTIONNAIRE\n",
    "==========================================\n\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "Method: ", method_id, "\n"
  )
  
  if (is_optimized) {
    guide_text <- paste0(guide_text,
                         "Status: OPTIMIZED\n")
  }
  
  # Add pattern-specific note if applicable
  if (!is.null(pattern_rules) && method_results$combination$reduction == "sc_ep") {
    guide_text <- paste0(guide_text,
                         "Boundary Type: PATTERN-SPECIFIC (High Fidelity)\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n1. METHOD COMPONENTS\n",
                       "-------------------\n",
                       "Ordering Method: ", method_results$combination$ordering, "\n",
                       "Reduction Method: ", method_results$combination$reduction, "\n")
  
  # Handle gamma parameters based on optimization status
  if (is_optimized && prepared_data$config$questionnaire_type == "multi-construct") {
    guide_text <- paste0(guide_text,
                         "Gamma Parameters: Construct-specific (see section 6)\n")
  } else if (!is.na(method_results$combination$gamma_0)) {
    guide_text <- paste0(guide_text,
                         "Gamma Parameters:\n",
                         "  - γ₀ (low-risk threshold): ", method_results$combination$gamma_0, "\n",
                         "  - γ₁ (high-risk threshold): ", method_results$combination$gamma_1, "\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n2. QUESTIONNAIRE STRUCTURE\n",
                       "-------------------------\n",
                       "Type: ", prepared_data$config$questionnaire_type, "\n",
                       "Total Items: ", nrow(admin_sequence), "\n")
  
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    guide_text <- paste0(guide_text,
                         "Constructs: ", length(prepared_data$config$constructs), "\n")
    
    for (cn in names(prepared_data$config$constructs)) {
      n_items <- length(prepared_data$config$constructs[[cn]])
      guide_text <- paste0(guide_text,
                           "  - ", cn, ": ", n_items, " items\n")
    }
  }
  
  guide_text <- paste0(guide_text,
                       "\n3. ADMINISTRATION SEQUENCE\n",
                       "--------------------------\n",
                       "Items are administered in the following order:\n")
  
  # Show first 10 items
  n_show <- min(10, nrow(admin_sequence))
  for (i in 1:n_show) {
    guide_text <- paste0(guide_text,
                         sprintf("  %2d. %s (original position: %d)",
                                 i,
                                 admin_sequence$item_id[i],
                                 admin_sequence$original_position[i]))
    
    if (prepared_data$config$questionnaire_type == "multi-construct") {
      guide_text <- paste0(guide_text,
                           " [", admin_sequence$construct[i], "]")
    }
    
    # Add item text preview (truncated if too long)
    item_text_preview <- admin_sequence$item_text[i]
    if (nchar(item_text_preview) > 50) {
      item_text_preview <- paste0(substr(item_text_preview, 1, 47), "...")
    }
    guide_text <- paste0(guide_text, "\n      \"", item_text_preview, "\"\n")
  }
  
  if (nrow(admin_sequence) > 10) {
    guide_text <- paste0(guide_text, "  ... (see administration_sequence.csv for full list)\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n4. STOPPING RULES\n",
                       "-----------------\n")
  
  if (method_results$combination$reduction == "none") {
    guide_text <- paste0(guide_text,
                         "No early stopping - all items must be administered.\n")
  } else {
    if (!is.null(pattern_rules) && method_results$combination$reduction == "sc_ep") {
      guide_text <- paste0(guide_text,
                           "Early stopping is based on EXACT RESPONSE PATTERNS.\n",
                           "See pattern_rules_summary.txt for pattern-specific rules.\n",
                           "See stopping_boundaries.html for sum-score approximations.\n\n")
    } else {
      guide_text <- paste0(guide_text,
                           "Early stopping is based on cumulative scores.\n",
                           "See stopping_boundaries.html for detailed rules.\n\n")
    }
    
    guide_text <- paste0(guide_text,
                         "Key points:\n",
                         "- Low risk: Stop when conditions indicate low risk\n")
    
    stop_low_only <- method_results$reduction_result$constraints_applied$stop_low_only %||% FALSE
    if (!stop_low_only && method_results$combination$gamma_1 < 1.0) {
      guide_text <- paste0(guide_text,
                           "- High risk: Stop when conditions indicate high risk\n")
    } else {
      guide_text <- paste0(guide_text,
                           "- High risk: No early stopping (complete assessment required)\n")
    }
  }
  
  guide_text <- paste0(guide_text,
                       "\n5. IMPLEMENTATION FILES\n",
                       "----------------------\n",
                       "- surveyjs_config.json: Configuration for SurveyJS implementation\n",
                       "- surveyjs_example.html: Example HTML page showing the questionnaire\n",
                       "- stopping_boundaries.csv: Detailed stopping boundaries (data format)\n",
                       "- stopping_boundaries.html: Visual presentation of boundaries\n",
                       "- administration_sequence.csv: Complete item ordering\n",
                       "- implementation_params.rds: R object with all parameters\n")
  
  if (!is.null(pattern_rules)) {
    guide_text <- paste0(guide_text,
                         "- pattern_rules.rds: Pattern-specific stopping rules (R format)\n",
                         "- pattern_rules.json: Pattern-specific stopping rules (JSON format)\n",
                         "- pattern_rules_summary.txt: Human-readable pattern rules summary\n")
  }
  
  # Add optimization section if applicable
  if (is_optimized && !is.null(optimization_results)) {
    guide_text <- paste0(guide_text,
                         "\n6. OPTIMIZATION DETAILS\n",
                         "-----------------------\n",
                         "This method has been optimized with construct-specific parameters.\n",
                         "Optimization timestamp: ", 
                         format(optimization_results$timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    if (prepared_data$config$questionnaire_type == "multi-construct") {
      guide_text <- paste0(guide_text,
                           "Construct-Specific Gamma Values:\n")
      
      for (cn in names(method_results$combination$construct_gammas)) {
        gammas <- method_results$combination$construct_gammas[[cn]]
        
        # Get optimization details if available
        if (!is.null(optimization_results$optimization_analysis$construct_results[[cn]])) {
          cr <- optimization_results$optimization_analysis$construct_results[[cn]]
          
          guide_text <- paste0(guide_text,
                               "  ", cn, ":\n",
                               "    - Base rate: ", sprintf("%.1f%%", cr$base_rate * 100), 
                               " (", cr$prevalence_category, " prevalence)\n",
                               "    - γ₀ = ", gammas$gamma_0, "\n",
                               "    - γ₁ = ", gammas$gamma_1, "\n",
                               "    - FNR achieved: ", sprintf("%.1f%%", cr$best_performance$fnr * 100), "\n")
          
          if (isTRUE(cr$optimization_failed)) {
            guide_text <- paste0(guide_text,
                                 "    - ⚠️  WARNING: ", cr$failure_reason, "\n")
          }
        }
      }
      
      # Add performance comparison
      guide_text <- paste0(guide_text,
                           "\nPerformance Improvement:\n",
                           "  Original FNR: ", sprintf("%.3f", optimization_results$original_performance$fnr), "\n",
                           "  Optimized FNR: ", sprintf("%.3f", optimization_results$optimized_performance$fnr), "\n",
                           "  Improvement: ", sprintf("%.1f%%", 
                                                      100 * (optimization_results$original_performance$fnr - 
                                                               optimization_results$optimized_performance$fnr) / 
                                                        optimization_results$original_performance$fnr), " reduction\n")
    }
  }
  
  guide_text <- paste0(guide_text,
                       "\n7. DEPLOYMENT CHECKLIST\n",
                       "----------------------\n",
                       "[ ] Review stopping boundaries for clinical appropriateness\n",
                       "[ ] Verify item texts are correctly mapped\n",
                       "[ ] Test the questionnaire flow with sample responses\n",
                       "[ ] Implement data collection for responses\n",
                       "[ ] Set up monitoring for real-world performance\n",
                       "[ ] Train staff on adaptive administration\n",
                       "[ ] Prepare fallback for technical issues\n")
  
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    guide_text <- paste0(guide_text,
                         "\n8. MULTI-CONSTRUCT CONSIDERATIONS\n",
                         "---------------------------------\n",
                         "- Items are interleaved across constructs\n",
                         "- Stopping decisions are made per construct\n",
                         "- Each construct has its own boundaries\n",
                         "- SurveyJS logic uses construct-specific sums\n")
    
    if (is_optimized) {
      guide_text <- paste0(guide_text,
                           "- Each construct has optimized gamma values\n",
                           "- See section 6 for construct-specific parameters\n")
    }
  }
  
  writeLines(guide_text, guide_file)
}

# ============================================================================
# Helper Functions
# ============================================================================

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}