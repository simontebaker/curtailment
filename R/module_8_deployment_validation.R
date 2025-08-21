# ============================================================================
# Module 8: Deployment Validation Module (ENHANCED WITH PATTERN SUPPORT)
# ============================================================================
# Purpose: Validate deployment artifacts by simulating the deployed questionnaire
#          and comparing actual performance to predicted performance
#          NOW WITH: Full pattern-specific validation for SC-EP method
# ============================================================================

# Required packages
required_packages <- c("ggplot2", "gridExtra")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for visualization functionality"))
  }
}

# Define NULL-coalescing operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

#' Extract Boundary Value from Text
#'
#' @param boundary_text Boundary text like "Xk <= 5" or "Xk >= 10"
#' @param type Type of boundary ("low" or "high")
#' @return Numeric boundary value or NA
extract_boundary_value <- function(boundary_text, type = "low") {
  
  if (is.na(boundary_text) || boundary_text == "N/A" || boundary_text == "") {
    return(NA)
  }
  
  # Handle different boundary formats
  if (type == "low") {
    # Low boundaries typically use <=
    if (grepl("Xk <= ", boundary_text)) {
      value <- as.numeric(gsub("Xk <= ", "", boundary_text))
    } else if (grepl("Xk < ", boundary_text)) {
      # If using strict inequality, adjust by 1
      value <- as.numeric(gsub("Xk < ", "", boundary_text)) - 1
    } else if (grepl("<=", boundary_text)) {
      # Generic format
      value <- as.numeric(gsub(".*<= *", "", boundary_text))
    } else {
      # Try to extract any number
      numbers <- as.numeric(regmatches(boundary_text, regexpr("[0-9]+\\.?[0-9]*", boundary_text)))
      value <- if (length(numbers) > 0) numbers[1] else NA
    }
  } else if (type == "high") {
    # High boundaries typically use >=
    if (grepl("Xk >= ", boundary_text)) {
      value <- as.numeric(gsub("Xk >= ", "", boundary_text))
    } else if (grepl("Xk > ", boundary_text)) {
      # If using strict inequality, adjust by 1
      value <- as.numeric(gsub("Xk > ", "", boundary_text)) + 1
    } else if (grepl(">=", boundary_text)) {
      # Generic format
      value <- as.numeric(gsub(".*>= *", "", boundary_text))
    } else {
      # Try to extract any number
      numbers <- as.numeric(regmatches(boundary_text, regexpr("[0-9]+\\.?[0-9]*", boundary_text)))
      value <- if (length(numbers) > 0) numbers[1] else NA
    }
  } else {
    value <- NA
  }
  
  return(value)
}

#' Validate Deployment Package (ENHANCED)
#'
#' @param deployment_package Deployment package from Module 7
#' @param prepared_data Prepared data from Module 1
#' @param evaluation_results Evaluation results from Module 5 (optional)
#' @param optimization_results Optimization results from Module 6 (optional)
#' @param validation_data Data to use for validation (default: test split)
#' @param output_dir Output directory for validation reports
#' @return List containing validation results and performance comparison
#' @export
validate_deployment <- function(deployment_package,
                                prepared_data,
                                evaluation_results = NULL,
                                optimization_results = NULL,
                                validation_data = NULL,
                                output_dir = "deployment_validation") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("========================================\n")
  cat("Starting Deployment Validation Process\n")
  cat("========================================\n\n")
  
  # Use test data if no validation data provided
  if (is.null(validation_data)) {
    validation_data <- prepared_data$splits$test
    cat("Using test split data for validation\n")
  } else {
    cat("Using provided validation data\n")
  }
  
  # Get implementation parameters
  impl_params <- deployment_package$implementation_params
  
  # Check if pattern-specific boundaries are being used
  use_patterns <- impl_params$use_pattern_boundaries %||% FALSE
  if (use_patterns) {
    cat("Using PATTERN-SPECIFIC boundaries for SC-EP validation\n")
  }
  
  # 1. Simulate deployed questionnaire
  cat("\n1. Simulating deployed questionnaire...\n")
  simulation_results <- simulate_deployed_questionnaire(
    boundary_tables = deployment_package$boundary_tables,
    pattern_rules = deployment_package$pattern_rules,  # Pass pattern rules
    admin_sequence = deployment_package$admin_sequence,
    validation_data = validation_data,
    impl_params = impl_params,
    prepared_data = prepared_data
  )
  
  # 2. Calculate deployment performance metrics
  cat("\n2. Calculating deployment performance...\n")
  deployment_performance <- calculate_deployment_performance(
    simulation_results = simulation_results,
    validation_data = validation_data,
    prepared_data = prepared_data,
    impl_params = impl_params
  )
  
  # 3. Compare with original evaluation results
  cat("\n3. Comparing with original evaluation results...\n")
  performance_comparison <- compare_deployment_performance(
    deployment_performance = deployment_performance,
    evaluation_results = evaluation_results,
    optimization_results = optimization_results,
    impl_params = impl_params
  )
  
  # 4. Validate continuation logic
  cat("\n4. Validating continuation logic...\n")
  continuation_validation <- validate_continuation_logic_simplified(
    simulation_results = simulation_results,
    boundary_tables = deployment_package$boundary_tables,
    pattern_rules = deployment_package$pattern_rules,
    admin_sequence = deployment_package$admin_sequence,
    validation_data = validation_data,
    impl_params = impl_params,
    prepared_data = prepared_data
  )
  
  # 5. Analyze boundary utilization
  cat("\n5. Analyzing boundary utilization...\n")
  boundary_analysis <- analyze_boundary_utilization(
    simulation_results = simulation_results,
    boundary_tables = deployment_package$boundary_tables,
    pattern_rules = deployment_package$pattern_rules,
    prepared_data = prepared_data
  )
  
  # 6. Generate validation reports
  cat("\n6. Generating validation reports...\n")
  generate_validation_reports(
    deployment_performance = deployment_performance,
    performance_comparison = performance_comparison,
    continuation_validation = continuation_validation,
    boundary_analysis = boundary_analysis,
    simulation_results = simulation_results,
    impl_params = impl_params,
    output_dir = output_dir
  )
  
  # Compile validation results
  validation_results <- list(
    deployment_performance = deployment_performance,
    performance_comparison = performance_comparison,
    continuation_validation = continuation_validation,
    boundary_analysis = boundary_analysis,
    simulation_results = simulation_results,
    validation_passed = assess_validation_success(performance_comparison, continuation_validation),
    pattern_based = use_patterns,
    timestamp = Sys.time()
  )
  
  # Save results
  saveRDS(validation_results, file.path(output_dir, "deployment_validation_results.rds"))
  
  cat("\n✅ Deployment validation complete!\n")
  cat("Results saved to:", output_dir, "\n")
  
  return(validation_results)
}

#' Simulate Deployed Questionnaire (ENHANCED)
#'
#' @param boundary_tables Stopping boundary tables
#' @param pattern_rules Pattern-specific rules (if applicable)
#' @param admin_sequence Administration sequence
#' @param validation_data Validation data
#' @param impl_params Implementation parameters
#' @param prepared_data Prepared data
#' @return Simulation results
simulate_deployed_questionnaire <- function(boundary_tables, pattern_rules = NULL,
                                            admin_sequence, validation_data, 
                                            impl_params, prepared_data) {
  
  n_respondents <- nrow(validation_data)
  ordered_items <- admin_sequence$item_id
  n_items <- length(ordered_items)
  
  # Initialize simulation results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  stop_reasons <- character(n_respondents)
  classifications <- numeric(n_respondents)
  stop_patterns <- character(n_respondents)  # Track pattern that triggered stop
  
  # Get method configuration
  reduction_method <- impl_params$method_combination$reduction
  stop_low_only <- prepared_data$config$constraints$stop_low_only %||% FALSE
  min_items_per_construct <- prepared_data$config$constraints$min_items_per_construct %||% 1
  use_patterns <- impl_params$use_pattern_boundaries %||% FALSE
  
  cat("  Processing", n_respondents, "respondents...\n")
  if (use_patterns) {
    cat("  Using pattern-specific stopping rules\n")
  }
  pb <- txtProgressBar(min = 0, max = n_respondents, style = 3)
  
  # Simulate each respondent's questionnaire experience
  for (i in seq_len(n_respondents)) {
    
    if (prepared_data$config$questionnaire_type == "unidimensional") {
      # Unidimensional simulation
      if (use_patterns && !is.null(pattern_rules)) {
        # Use pattern-specific simulation
        result <- simulate_unidimensional_respondent_patterns(
          respondent_idx = i,
          validation_data = validation_data,
          pattern_rules = pattern_rules[["total"]],
          ordered_items = ordered_items,
          reduction_method = reduction_method,
          stop_low_only = stop_low_only,
          cutoff = prepared_data$config$cutoffs[["total"]]
        )
      } else {
        # Use sum-score simulation
        result <- simulate_unidimensional_respondent(
          respondent_idx = i,
          validation_data = validation_data,
          boundary_table = boundary_tables[["total"]],
          ordered_items = ordered_items,
          reduction_method = reduction_method,
          stop_low_only = stop_low_only,
          cutoff = prepared_data$config$cutoffs[["total"]]
        )
      }
    } else {
      # Multi-construct simulation
      if (use_patterns && !is.null(pattern_rules)) {
        # Use pattern-specific simulation
        result <- simulate_multicontruct_respondent_patterns(
          respondent_idx = i,
          validation_data = validation_data,
          pattern_rules = pattern_rules,
          admin_sequence = admin_sequence,
          prepared_data = prepared_data,
          reduction_method = reduction_method,
          stop_low_only = stop_low_only,
          min_items_per_construct = min_items_per_construct
        )
      } else {
        # Use sum-score simulation
        result <- simulate_multicontruct_respondent(
          respondent_idx = i,
          validation_data = validation_data,
          boundary_tables = boundary_tables,
          admin_sequence = admin_sequence,
          prepared_data = prepared_data,
          reduction_method = reduction_method,
          stop_low_only = stop_low_only,
          min_items_per_construct = min_items_per_construct
        )
      }
    }
    
    # Store results
    items_administered[i, 1:result$items_used] <- TRUE
    stopped_at[i] <- result$items_used
    stop_reasons[i] <- result$stop_reason
    classifications[i] <- result$classification
    if (!is.null(result$stop_pattern)) {
      stop_patterns[i] <- result$stop_pattern
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(list(
    items_administered = items_administered,
    stopped_at = stopped_at,
    stop_reasons = stop_reasons,
    classifications = classifications,
    stop_patterns = stop_patterns,
    n_items_used = stopped_at,
    ordered_items = ordered_items,
    pattern_based = use_patterns
  ))
}

#' Simulate Single Respondent - Unidimensional with Patterns (NEW)
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param pattern_rules Pattern rules
#' @param ordered_items Ordered items
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @param cutoff Classification cutoff
#' @return Simulation result for this respondent
simulate_unidimensional_respondent_patterns <- function(respondent_idx, validation_data, 
                                                        pattern_rules, ordered_items,
                                                        reduction_method, stop_low_only,
                                                        cutoff) {
  
  n_items <- length(ordered_items)
  current_sum <- 0
  
  for (k in seq_along(ordered_items)) {
    item <- ordered_items[k]
    
    # Add current item response
    if (item %in% names(validation_data) && !is.na(validation_data[respondent_idx, item])) {
      current_sum <- current_sum + validation_data[respondent_idx, item]
    }
    
    # Get response pattern so far
    current_pattern <- character()
    for (j in 1:k) {
      item_j <- ordered_items[j]
      if (item_j %in% names(validation_data)) {
        current_pattern <- c(current_pattern, 
                             as.character(validation_data[respondent_idx, item_j]))
      }
    }
    pattern_string <- paste(current_pattern, collapse = "_")
    
    # Check pattern-specific stopping conditions
    if (reduction_method == "sc_ep" && k <= length(pattern_rules)) {
      rules <- pattern_rules[[k]]
      
      if (!is.null(rules)) {
        # Check low-risk patterns
        if (!is.null(rules$low_risk_patterns) && 
            pattern_string %in% names(rules$low_risk_patterns)) {
          return(list(
            items_used = k,
            stop_reason = "pattern_matched_low_risk",
            classification = 0,
            final_sum = current_sum,
            stop_pattern = pattern_string
          ))
        }
        
        # Check high-risk patterns (if allowed)
        if (!stop_low_only && !is.null(rules$high_risk_patterns) &&
            pattern_string %in% names(rules$high_risk_patterns)) {
          return(list(
            items_used = k,
            stop_reason = "pattern_matched_high_risk",
            classification = 1,
            final_sum = current_sum,
            stop_pattern = pattern_string
          ))
        }
      }
    }
  }
  
  # Completed all items - no pattern matched
  final_classification <- ifelse(current_sum >= cutoff, 1, 0)
  return(list(
    items_used = n_items,
    stop_reason = "no_pattern_matched",
    classification = final_classification,
    final_sum = current_sum,
    stop_pattern = NULL
  ))
}

#' Simulate Single Respondent - Multi-construct with Patterns (NEW)
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param pattern_rules Pattern rules by construct
#' @param admin_sequence Administration sequence
#' @param prepared_data Prepared data
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @param min_items_per_construct Minimum items per construct
#' @return Simulation result for this respondent
simulate_multicontruct_respondent_patterns <- function(respondent_idx, validation_data, 
                                                       pattern_rules, admin_sequence,
                                                       prepared_data, reduction_method,
                                                       stop_low_only, min_items_per_construct) {
  
  # Track progress for each construct
  construct_sums <- list()
  construct_items_given <- list()
  construct_items_total <- list()
  construct_classifications <- list()
  construct_stopped <- list()
  construct_patterns <- list()
  construct_stop_patterns <- list()
  
  # Initialize tracking for each construct
  for (cn in names(prepared_data$config$constructs)) {
    construct_sums[[cn]] <- 0
    construct_items_given[[cn]] <- 0
    construct_items_total[[cn]] <- length(prepared_data$config$constructs[[cn]])
    construct_classifications[[cn]] <- NA
    construct_stopped[[cn]] <- FALSE
    construct_patterns[[cn]] <- character()
    construct_stop_patterns[[cn]] <- NULL
  }
  
  total_items_given <- 0
  num_constructs_stopped_early <- 0
  total_constructs <- length(prepared_data$config$constructs)
  
  # Go through administration sequence
  for (k in seq_len(nrow(admin_sequence))) {
    item <- admin_sequence$item_id[k]
    item_construct <- admin_sequence$construct[k]
    
    # Skip if this construct has already stopped
    if (construct_stopped[[item_construct]]) {
      next
    }
    
    # Add response for this item
    if (item %in% names(validation_data) && !is.na(validation_data[respondent_idx, item])) {
      response_value <- validation_data[respondent_idx, item]
      construct_sums[[item_construct]] <- construct_sums[[item_construct]] + response_value
      construct_patterns[[item_construct]] <- c(construct_patterns[[item_construct]], 
                                                as.character(response_value))
    }
    
    construct_items_given[[item_construct]] <- construct_items_given[[item_construct]] + 1
    total_items_given <- total_items_given + 1
    
    # Get current pattern for this construct
    pattern_string <- paste(construct_patterns[[item_construct]], collapse = "_")
    
    # Check if this construct can stop (respecting min_items_per_construct)
    if (reduction_method == "sc_ep" && 
        construct_items_given[[item_construct]] >= min_items_per_construct &&
        !is.null(pattern_rules[[item_construct]])) {
      
      # Get pattern rules for this construct at this position
      within_construct_position <- construct_items_given[[item_construct]]
      
      if (within_construct_position <= length(pattern_rules[[item_construct]])) {
        rules <- pattern_rules[[item_construct]][[within_construct_position]]
        
        if (!is.null(rules)) {
          # Check stopping conditions for this construct
          stopped <- FALSE
          
          # Check low-risk patterns
          if (!is.null(rules$low_risk_patterns) &&
              pattern_string %in% names(rules$low_risk_patterns)) {
            construct_classifications[[item_construct]] <- 0
            construct_stopped[[item_construct]] <- TRUE
            construct_stop_patterns[[item_construct]] <- pattern_string
            stopped <- TRUE
          }
          
          # Check high-risk patterns (if allowed)
          if (!stopped && !stop_low_only && !is.null(rules$high_risk_patterns) &&
              pattern_string %in% names(rules$high_risk_patterns)) {
            construct_classifications[[item_construct]] <- 1
            construct_stopped[[item_construct]] <- TRUE
            construct_stop_patterns[[item_construct]] <- pattern_string
            stopped <- TRUE
          }
        }
      }
    }
    
    # Check if all constructs have stopped
    if (all(unlist(construct_stopped))) {
      break
    }
  }
  
  # Count how many constructs stopped early
  for (cn in names(prepared_data$config$constructs)) {
    if (construct_stopped[[cn]] && 
        construct_items_given[[cn]] < construct_items_total[[cn]]) {
      num_constructs_stopped_early <- num_constructs_stopped_early + 1
    }
  }
  
  # Determine the stop reason
  if (num_constructs_stopped_early == total_constructs) {
    final_stop_reason <- "all_constructs_pattern_matched"
  } else if (num_constructs_stopped_early > 0) {
    final_stop_reason <- "some_constructs_pattern_matched"
  } else {
    final_stop_reason <- "no_patterns_matched"
  }
  
  # Classify any constructs that didn't stop early
  for (cn in names(prepared_data$config$constructs)) {
    if (is.na(construct_classifications[[cn]])) {
      cutoff <- prepared_data$config$cutoffs[[cn]]
      construct_classifications[[cn]] <- ifelse(construct_sums[[cn]] >= cutoff, 1, 0)
    }
  }
  
  # Overall classification
  overall_classification <- ifelse(sum(unlist(construct_classifications)) > 0, 1, 0)
  
  # Combine stop patterns if any
  all_stop_patterns <- paste(unlist(construct_stop_patterns[!sapply(construct_stop_patterns, is.null)]), 
                             collapse = ";")
  
  return(list(
    items_used = total_items_given,
    stop_reason = final_stop_reason,
    classification = overall_classification,
    construct_classifications = construct_classifications,
    construct_sums = construct_sums,
    num_constructs_stopped_early = num_constructs_stopped_early,
    stop_pattern = if(nchar(all_stop_patterns) > 0) all_stop_patterns else NULL
  ))
}

# [Keep original simulation functions for sum-score based stopping]
#' Simulate Single Respondent - Unidimensional (ORIGINAL)
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param boundary_table Boundary table
#' @param ordered_items Ordered items
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @param cutoff Classification cutoff
#' @return Simulation result for this respondent
simulate_unidimensional_respondent <- function(respondent_idx, validation_data, boundary_table,
                                               ordered_items, reduction_method, stop_low_only,
                                               cutoff) {
  
  current_sum <- 0
  n_items <- length(ordered_items)
  
  for (k in seq_along(ordered_items)) {
    item <- ordered_items[k]
    
    # Add current item response
    if (item %in% names(validation_data) && !is.na(validation_data[respondent_idx, item])) {
      current_sum <- current_sum + validation_data[respondent_idx, item]
    }
    
    # Check stopping conditions
    if (reduction_method != "none" && k <= nrow(boundary_table)) {
      boundary_row <- boundary_table[k, ]
      
      # Check low risk boundary
      if (boundary_row$low_risk_boundary != "N/A") {
        boundary_val <- extract_boundary_value(boundary_row$low_risk_boundary, type = "low")
        if (!is.na(boundary_val) && current_sum <= boundary_val) {
          return(list(
            items_used = k,
            stop_reason = "stopped_early_low_risk",
            classification = 0,
            final_sum = current_sum
          ))
        }
      }
      
      # Check high risk boundary (if allowed)
      if (!stop_low_only && boundary_row$high_risk_boundary != "N/A") {
        boundary_val <- extract_boundary_value(boundary_row$high_risk_boundary, type = "high")
        if (!is.na(boundary_val) && current_sum >= boundary_val) {
          return(list(
            items_used = k,
            stop_reason = "stopped_early_high_risk",
            classification = 1,
            final_sum = current_sum
          ))
        }
      }
    }
  }
  
  # Completed all items - no early stopping occurred
  final_classification <- ifelse(current_sum >= cutoff, 1, 0)
  return(list(
    items_used = n_items,
    stop_reason = "no_early_stopping",
    classification = final_classification,
    final_sum = current_sum
  ))
}

#' Simulate Single Respondent - Multi-construct (ORIGINAL)
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param boundary_tables Boundary tables by construct
#' @param admin_sequence Administration sequence
#' @param prepared_data Prepared data
#' @param reduction_method Reduction method
#' @param stop_low_only Stop low only flag
#' @param min_items_per_construct Minimum items per construct
#' @return Simulation result for this respondent
simulate_multicontruct_respondent <- function(respondent_idx, validation_data, boundary_tables,
                                              admin_sequence, prepared_data, reduction_method,
                                              stop_low_only, min_items_per_construct) {
  
  # Track progress for each construct
  construct_sums <- list()
  construct_items_given <- list()
  construct_items_total <- list()
  construct_classifications <- list()
  construct_stopped <- list()
  
  # Initialize tracking for each construct
  for (cn in names(prepared_data$config$constructs)) {
    construct_sums[[cn]] <- 0
    construct_items_given[[cn]] <- 0
    construct_items_total[[cn]] <- length(prepared_data$config$constructs[[cn]])
    construct_classifications[[cn]] <- NA
    construct_stopped[[cn]] <- FALSE
  }
  
  total_items_given <- 0
  num_constructs_stopped_early <- 0
  total_constructs <- length(prepared_data$config$constructs)
  
  # Go through administration sequence
  for (k in seq_len(nrow(admin_sequence))) {
    item <- admin_sequence$item_id[k]
    item_construct <- admin_sequence$construct[k]
    
    # Skip if this construct has already stopped
    if (construct_stopped[[item_construct]]) {
      next
    }
    
    # Add response for this item
    if (item %in% names(validation_data) && !is.na(validation_data[respondent_idx, item])) {
      construct_sums[[item_construct]] <- construct_sums[[item_construct]] + 
        validation_data[respondent_idx, item]
    }
    
    construct_items_given[[item_construct]] <- construct_items_given[[item_construct]] + 1
    total_items_given <- total_items_given + 1
    
    # Check if this construct can stop (respecting min_items_per_construct)
    if (reduction_method != "none" && 
        construct_items_given[[item_construct]] >= min_items_per_construct) {
      
      # Get boundary table for this construct
      construct_boundary <- boundary_tables[[item_construct]]
      within_construct_position <- construct_items_given[[item_construct]]
      
      if (within_construct_position <= nrow(construct_boundary)) {
        boundary_row <- construct_boundary[within_construct_position, ]
        current_sum <- construct_sums[[item_construct]]
        
        # Check stopping conditions for this construct
        stopped <- FALSE
        
        # Check low risk boundary
        if (boundary_row$low_risk_boundary != "N/A") {
          boundary_val <- extract_boundary_value(boundary_row$low_risk_boundary, type = "low")
          if (!is.na(boundary_val) && current_sum <= boundary_val) {
            construct_classifications[[item_construct]] <- 0
            construct_stopped[[item_construct]] <- TRUE
            stopped <- TRUE
          }
        }
        
        # Check high risk boundary (if allowed)
        if (!stopped && !stop_low_only && boundary_row$high_risk_boundary != "N/A") {
          boundary_val <- extract_boundary_value(boundary_row$high_risk_boundary, type = "high")
          if (!is.na(boundary_val) && current_sum >= boundary_val) {
            construct_classifications[[item_construct]] <- 1
            construct_stopped[[item_construct]] <- TRUE
            stopped <- TRUE
          }
        }
      }
    }
    
    # Check if all constructs have stopped
    if (all(unlist(construct_stopped))) {
      break
    }
  }
  
  # Count how many constructs stopped early (before all their items were administered)
  for (cn in names(prepared_data$config$constructs)) {
    if (construct_stopped[[cn]] && 
        construct_items_given[[cn]] < construct_items_total[[cn]]) {
      num_constructs_stopped_early <- num_constructs_stopped_early + 1
    }
  }
  
  # Determine the stop reason with CLEAR, UNAMBIGUOUS terminology
  if (num_constructs_stopped_early == total_constructs) {
    final_stop_reason <- "all_constructs_stopped_early"
  } else if (num_constructs_stopped_early > 0) {
    final_stop_reason <- "some_constructs_stopped_early"
  } else {
    final_stop_reason <- "no_constructs_stopped_early"
  }
  
  # Classify any constructs that didn't stop early
  for (cn in names(prepared_data$config$constructs)) {
    if (is.na(construct_classifications[[cn]])) {
      cutoff <- prepared_data$config$cutoffs[[cn]]
      construct_classifications[[cn]] <- ifelse(construct_sums[[cn]] >= cutoff, 1, 0)
    }
  }
  
  # Overall classification (could be customized based on questionnaire logic)
  # For now, use majority vote or any positive
  overall_classification <- ifelse(sum(unlist(construct_classifications)) > 0, 1, 0)
  
  return(list(
    items_used = total_items_given,
    stop_reason = final_stop_reason,
    classification = overall_classification,
    construct_classifications = construct_classifications,
    construct_sums = construct_sums,
    num_constructs_stopped_early = num_constructs_stopped_early
  ))
}

#' Calculate Deployment Performance
#'
#' @param simulation_results Simulation results
#' @param validation_data Validation data
#' @param prepared_data Prepared data
#' @param impl_params Implementation parameters
#' @return Performance metrics
calculate_deployment_performance <- function(simulation_results, validation_data,
                                             prepared_data, impl_params) {
  
  # Get true outcomes
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    outcome_col <- prepared_data$config$outcome_column
    true_outcomes <- validation_data[[outcome_col]]
  } else {
    # For multi-construct, calculate overall outcome
    # This logic should match your specific questionnaire requirements
    outcome_cols <- paste0(names(prepared_data$config$constructs), "_outcome")
    true_outcomes <- apply(validation_data[, outcome_cols], 1, function(x) ifelse(sum(x) > 0, 1, 0))
  }
  
  # Calculate confusion matrix
  predictions <- simulation_results$classifications
  tp <- sum(predictions == 1 & true_outcomes == 1)
  tn <- sum(predictions == 0 & true_outcomes == 0)
  fp <- sum(predictions == 1 & true_outcomes == 0)
  fn <- sum(predictions == 0 & true_outcomes == 1)
  
  # Calculate metrics
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  accuracy <- (tp + tn) / length(true_outcomes)
  balanced_accuracy <- (sensitivity + specificity) / 2
  fnr <- fn / (fn + tp)
  fpr <- fp / (fp + tn)
  
  # Calculate efficiency metrics
  n_items_total <- length(simulation_results$ordered_items)
  mean_items_used <- mean(simulation_results$n_items_used)
  reduction_pct <- (1 - mean_items_used / n_items_total) * 100
  
  # Add pattern-specific metrics if applicable
  pattern_metrics <- NULL
  if (simulation_results$pattern_based) {
    pattern_stop_count <- sum(grepl("pattern_matched", simulation_results$stop_reasons))
    pattern_stop_rate <- pattern_stop_count / length(simulation_results$stop_reasons)
    
    pattern_metrics <- list(
      pattern_stop_count = pattern_stop_count,
      pattern_stop_rate = pattern_stop_rate,
      unique_patterns = length(unique(simulation_results$stop_patterns[simulation_results$stop_patterns != ""]))
    )
  }
  
  return(list(
    tp = tp, tn = tn, fp = fp, fn = fn,
    sensitivity = sensitivity,
    specificity = specificity,
    accuracy = accuracy,
    balanced_accuracy = balanced_accuracy,
    fnr = fnr,
    fpr = fpr,
    mean_items_used = mean_items_used,
    median_items_used = median(simulation_results$n_items_used),
    sd_items_used = sd(simulation_results$n_items_used),
    reduction_pct = reduction_pct,
    n_respondents = length(predictions),
    pattern_metrics = pattern_metrics
  ))
}

#' Compare Deployment Performance
#'
#' @param deployment_performance Deployment performance
#' @param evaluation_results Evaluation results from Module 5
#' @param optimization_results Optimization results from Module 6
#' @param impl_params Implementation parameters
#' @return Performance comparison
compare_deployment_performance <- function(deployment_performance, evaluation_results = NULL,
                                           optimization_results = NULL, impl_params) {
  
  # Determine what we're comparing to and use appropriate terminology
  baseline_performance <- NULL
  comparison_type <- NULL
  
  if (!is.null(optimization_results) && !is.null(optimization_results$optimized_performance)) {
    baseline_performance <- optimization_results$optimized_performance
    comparison_type <- "optimized"
  } else if (!is.null(evaluation_results) && !is.null(evaluation_results$full_performance)) {
    baseline_performance <- evaluation_results$full_performance
    comparison_type <- "original"
  }
  
  if (is.null(baseline_performance)) {
    return(list(
      comparison_available = FALSE,
      message = "No baseline performance metrics available for comparison"
    ))
  }
  
  # Calculate differences
  metrics_to_compare <- c("sensitivity", "specificity", "accuracy", "balanced_accuracy", 
                          "fnr", "mean_items_used", "reduction_pct")
  
  absolute_diffs <- list()
  percentage_diffs <- list()
  
  for (metric in metrics_to_compare) {
    if (!is.null(baseline_performance[[metric]]) && !is.null(deployment_performance[[metric]])) {
      absolute_diffs[[metric]] <- deployment_performance[[metric]] - baseline_performance[[metric]]
      if (baseline_performance[[metric]] != 0) {
        percentage_diffs[[metric]] <- (absolute_diffs[[metric]] / abs(baseline_performance[[metric]])) * 100
      } else {
        percentage_diffs[[metric]] <- NA
      }
    }
  }
  
  # Assess fidelity (now direction-aware)
  fidelity_assessment <- assess_deployment_fidelity(absolute_diffs, percentage_diffs)
  
  # Add pattern-specific comparison if applicable
  pattern_comparison <- NULL
  if (!is.null(deployment_performance$pattern_metrics)) {
    pattern_comparison <- list(
      using_patterns = TRUE,
      pattern_stop_rate = deployment_performance$pattern_metrics$pattern_stop_rate,
      unique_patterns_used = deployment_performance$pattern_metrics$unique_patterns
    )
  }
  
  return(list(
    comparison_available = TRUE,
    comparison_type = comparison_type,  # "original" or "optimized"
    baseline_performance = baseline_performance,  # More neutral term
    deployment_performance = deployment_performance,
    absolute_differences = absolute_diffs,
    percentage_differences = percentage_diffs,
    fidelity_assessment = fidelity_assessment,
    pattern_comparison = pattern_comparison,
    # Keep these for backward compatibility
    original_performance = baseline_performance
  ))
}

#' Assess Deployment Fidelity (Direction-Aware)
#'
#' @param abs_diffs Absolute differences
#' @param pct_diffs Percentage differences
#' @return Fidelity assessment
assess_deployment_fidelity <- function(abs_diffs, pct_diffs) {
  
  # Define metrics where LOWER is better (we want these to decrease or stay low)
  lower_is_better <- c("fnr", "fpr", "mean_items_used", "median_items_used")
  
  # Define metrics where HIGHER is better (we want these to increase or stay high)
  higher_is_better <- c("sensitivity", "specificity", "accuracy", "balanced_accuracy", "reduction_pct")
  
  # Define thresholds for acceptable DEGRADATION (not just any change)
  # These are for when metrics get WORSE, not better
  degradation_thresholds <- list(
    sensitivity = 0.03,      # Allow up to 3% decrease
    specificity = 0.03,      # Allow up to 3% decrease
    fnr = 0.02,             # Allow up to 2% increase
    accuracy = 0.02,        # Allow up to 2% decrease
    balanced_accuracy = 0.02,# Allow up to 2% decrease
    mean_items_used = 5.0,  # Allow up to 5 items increase
    reduction_pct = 5.0     # Allow up to 5% decrease in reduction
  )
  
  # Percentage thresholds for degradation warnings
  degradation_pct_thresholds <- list(
    sensitivity = 5,         # Warn if >5% relative decrease
    specificity = 5,         # Warn if >5% relative decrease
    fnr = 20,               # Warn if >20% relative increase (more lenient for FNR)
    accuracy = 5,           # Warn if >5% relative decrease
    mean_items_used = 10    # Warn if >10% relative increase
  )
  
  # Check each metric
  issues <- character()
  warnings <- character()
  improvements <- character()
  
  for (metric in names(abs_diffs)) {
    if (!is.na(abs_diffs[[metric]])) {
      diff <- abs_diffs[[metric]]
      abs_diff <- abs(diff)
      
      # Determine if this change is an improvement or degradation
      is_improvement <- FALSE
      if (metric %in% lower_is_better && diff < 0) {
        # Metric decreased and lower is better = improvement
        is_improvement <- TRUE
        improvements <- c(improvements, sprintf("%s improved by %.3f", metric, abs_diff))
      } else if (metric %in% higher_is_better && diff > 0) {
        # Metric increased and higher is better = improvement
        is_improvement <- TRUE
        improvements <- c(improvements, sprintf("%s improved by %.3f", metric, abs_diff))
      }
      
      # Only flag as issue if it's a DEGRADATION that exceeds threshold
      if (!is_improvement && metric %in% names(degradation_thresholds)) {
        if (abs_diff > degradation_thresholds[[metric]]) {
          issues <- c(issues, sprintf("%s degraded beyond acceptable threshold: %.3f > %.3f",
                                      metric, abs_diff, degradation_thresholds[[metric]]))
        }
      }
      
      # Check percentage changes for warnings (only for degradations)
      if (!is_improvement && metric %in% names(pct_diffs) && metric %in% names(degradation_pct_thresholds)) {
        pct_diff <- abs(pct_diffs[[metric]])
        if (!is.na(pct_diff) && pct_diff > degradation_pct_thresholds[[metric]]) {
          warnings <- c(warnings, sprintf("%s degraded by %.1f%% (threshold: %.1f%%)",
                                          metric, pct_diff, degradation_pct_thresholds[[metric]]))
        }
      }
    }
  }
  
  # Overall assessment - PASS if no degradation issues
  overall_fidelity <- ifelse(length(issues) == 0, "PASS", "FAIL")
  
  return(list(
    overall_fidelity = overall_fidelity,
    critical_issues = issues,
    warnings = warnings,
    improvements = improvements,
    thresholds_used = list(
      degradation = degradation_thresholds,
      percentage = degradation_pct_thresholds
    )
  ))
}

#' Validate Continuation Logic (ENHANCED)
#'
#' @param simulation_results Simulation results
#' @param boundary_tables Boundary tables
#' @param pattern_rules Pattern rules (if applicable)
#' @param admin_sequence Administration sequence
#' @param validation_data Validation data
#' @param impl_params Implementation parameters
#' @param prepared_data Prepared data
#' @return Logic validation results
validate_continuation_logic_simplified <- function(simulation_results, boundary_tables, 
                                                   pattern_rules = NULL,
                                                   admin_sequence, validation_data,
                                                   impl_params, prepared_data) {
  
  # Validate that stopping reasons make sense
  stop_reasons <- table(simulation_results$stop_reasons)
  
  # Check for logical consistency
  logic_checks <- list()
  
  # 1. Check that reduction methods produce early stopping
  reduction_method <- impl_params$method_combination$reduction
  if (reduction_method != "none") {
    # Count how many had some form of early stopping
    if (simulation_results$pattern_based) {
      # Pattern-based stopping
      early_stop_count <- sum(
        stop_reasons[grepl("pattern_matched", names(stop_reasons))]
      )
    } else {
      # Sum-score based stopping
      early_stop_count <- sum(
        stop_reasons[grepl("all_constructs_stopped_early|some_constructs_stopped_early|stopped_early", 
                           names(stop_reasons))]
      )
    }
    early_stop_rate <- early_stop_count / sum(stop_reasons)
    logic_checks$early_stopping_present <- early_stop_rate > 0
  } else {
    # For no reduction, all should have no early stopping
    logic_checks$all_no_early_stop <- all(grepl("no_early_stopping|no_constructs_stopped_early", 
                                                simulation_results$stop_reasons))
  }
  
  # 2. Check that items used makes sense
  logic_checks$items_used_valid <- all(simulation_results$n_items_used >= 1) && 
    all(simulation_results$n_items_used <= length(admin_sequence$item_id))
  
  # 3. Check that classifications are binary
  logic_checks$classifications_binary <- all(simulation_results$classifications %in% c(0, 1))
  
  # 4. Sample check a few respondents in detail
  sample_size <- min(10, nrow(validation_data))
  sample_indices <- sample(seq_len(nrow(validation_data)), sample_size)
  
  detailed_checks <- list()
  for (idx in sample_indices) {
    # Rerun simulation for this respondent to verify
    if (prepared_data$config$questionnaire_type == "unidimensional") {
      if (simulation_results$pattern_based && !is.null(pattern_rules)) {
        rerun_result <- simulate_unidimensional_respondent_patterns(
          respondent_idx = idx,
          validation_data = validation_data,
          pattern_rules = pattern_rules[["total"]],
          ordered_items = admin_sequence$item_id,
          reduction_method = reduction_method,
          stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
          cutoff = prepared_data$config$cutoffs[["total"]]
        )
      } else {
        rerun_result <- simulate_unidimensional_respondent(
          respondent_idx = idx,
          validation_data = validation_data,
          boundary_table = boundary_tables[["total"]],
          ordered_items = admin_sequence$item_id,
          reduction_method = reduction_method,
          stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
          cutoff = prepared_data$config$cutoffs[["total"]]
        )
      }
    } else {
      if (simulation_results$pattern_based && !is.null(pattern_rules)) {
        rerun_result <- simulate_multicontruct_respondent_patterns(
          respondent_idx = idx,
          validation_data = validation_data,
          pattern_rules = pattern_rules,
          admin_sequence = admin_sequence,
          prepared_data = prepared_data,
          reduction_method = reduction_method,
          stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
          min_items_per_construct = prepared_data$config$constraints$min_items_per_construct %||% 1
        )
      } else {
        rerun_result <- simulate_multicontruct_respondent(
          respondent_idx = idx,
          validation_data = validation_data,
          boundary_tables = boundary_tables,
          admin_sequence = admin_sequence,
          prepared_data = prepared_data,
          reduction_method = reduction_method,
          stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
          min_items_per_construct = prepared_data$config$constraints$min_items_per_construct %||% 1
        )
      }
    }
    
    # Check consistency
    detailed_checks[[paste0("respondent_", idx)]] <- list(
      items_match = (rerun_result$items_used == simulation_results$n_items_used[idx]),
      classification_match = (rerun_result$classification == simulation_results$classifications[idx]),
      stop_reason_match = (rerun_result$stop_reason == simulation_results$stop_reasons[idx])
    )
  }
  
  # Check if all detailed checks passed
  all_detailed_passed <- all(sapply(detailed_checks, function(x) all(unlist(x))))
  
  # Overall validation
  logic_validation_passed <- all(unlist(logic_checks)) && all_detailed_passed
  
  return(list(
    logic_validation_passed = logic_validation_passed,
    logic_checks = logic_checks,
    detailed_checks = detailed_checks,
    stop_reason_distribution = stop_reasons,
    pattern_based = simulation_results$pattern_based,
    message = ifelse(logic_validation_passed, 
                     "Continuation logic validation passed",
                     "Issues detected in continuation logic")
  ))
}

#' Analyze Boundary Utilization (ENHANCED)
#'
#' @param simulation_results Simulation results
#' @param boundary_tables Boundary tables
#' @param pattern_rules Pattern rules (if applicable)
#' @param prepared_data Prepared data
#' @return Boundary utilization analysis
analyze_boundary_utilization <- function(simulation_results, boundary_tables, 
                                         pattern_rules = NULL, prepared_data) {
  
  # Analyze how often each boundary type is used
  stop_reasons <- simulation_results$stop_reasons
  
  utilization_summary <- list(
    total_respondents = length(stop_reasons),
    stop_reason_counts = table(stop_reasons),
    stop_reason_percentages = prop.table(table(stop_reasons)) * 100,
    pattern_based = simulation_results$pattern_based
  )
  
  # Calculate boundary coverage - how many positions have usable boundaries
  boundary_coverage <- list()
  
  for (construct_name in names(boundary_tables)) {
    boundary_table <- boundary_tables[[construct_name]]
    
    n_positions <- nrow(boundary_table)
    n_low_boundaries <- sum(boundary_table$low_risk_boundary != "N/A")
    n_high_boundaries <- sum(boundary_table$high_risk_boundary != "N/A")
    
    boundary_coverage[[construct_name]] <- list(
      total_positions = n_positions,
      low_boundary_coverage = n_low_boundaries / n_positions,
      high_boundary_coverage = n_high_boundaries / n_positions,
      effective_positions = max(n_low_boundaries, n_high_boundaries) / n_positions
    )
  }
  
  # Add pattern coverage if applicable
  pattern_coverage <- NULL
  if (!is.null(pattern_rules)) {
    pattern_coverage <- list()
    
    for (construct_name in names(pattern_rules)) {
      if (!is.null(pattern_rules[[construct_name]])) {
        rules <- pattern_rules[[construct_name]]
        n_positions <- length(rules)
        n_with_low_patterns <- sum(sapply(rules, function(r) 
          !is.null(r) && length(r$low_risk_patterns) > 0))
        n_with_high_patterns <- sum(sapply(rules, function(r) 
          !is.null(r) && length(r$high_risk_patterns) > 0))
        
        pattern_coverage[[construct_name]] <- list(
          total_positions = n_positions,
          positions_with_low_patterns = n_with_low_patterns,
          positions_with_high_patterns = n_with_high_patterns,
          total_low_patterns = sum(sapply(rules, function(r) 
            if(!is.null(r)) length(r$low_risk_patterns) else 0)),
          total_high_patterns = sum(sapply(rules, function(r) 
            if(!is.null(r)) length(r$high_risk_patterns) else 0))
        )
      }
    }
  }
  
  # Calculate efficiency metrics by stopping reason
  efficiency_by_reason <- list()
  for (reason in names(utilization_summary$stop_reason_counts)) {
    reason_indices <- which(stop_reasons == reason)
    if (length(reason_indices) > 0) {
      efficiency_by_reason[[reason]] <- list(
        count = length(reason_indices),
        mean_items = mean(simulation_results$n_items_used[reason_indices]),
        median_items = median(simulation_results$n_items_used[reason_indices]),
        sd_items = sd(simulation_results$n_items_used[reason_indices])
      )
    }
  }
  
  # Add pattern-specific utilization if applicable
  if (simulation_results$pattern_based && length(simulation_results$stop_patterns) > 0) {
    pattern_utilization <- table(simulation_results$stop_patterns[simulation_results$stop_patterns != ""])
    utilization_summary$pattern_utilization <- pattern_utilization
    utilization_summary$most_common_patterns <- head(sort(pattern_utilization, decreasing = TRUE), 10)
  }
  
  return(list(
    utilization_summary = utilization_summary,
    boundary_coverage = boundary_coverage,
    pattern_coverage = pattern_coverage,
    efficiency_by_reason = efficiency_by_reason
  ))
}

# [Keep all report generation functions unchanged - they will work with enhanced data]
# generate_validation_reports
# generate_validation_executive_summary
# generate_detailed_comparison_report
# generate_boundary_utilization_report
# generate_continuation_logic_report
# generate_deployment_recommendations
# generate_validation_visualizations
# All plotting functions
# assess_validation_success
# quick_deployment_check
# run_deployment_validation

#' Generate Validation Reports
#'
#' @param deployment_performance Deployment performance
#' @param performance_comparison Performance comparison
#' @param continuation_validation Continuation validation
#' @param boundary_analysis Boundary analysis
#' @param simulation_results Simulation results
#' @param impl_params Implementation parameters
#' @param output_dir Output directory
generate_validation_reports <- function(deployment_performance, performance_comparison,
                                        continuation_validation, boundary_analysis,
                                        simulation_results, impl_params, output_dir) {
  
  # 1. Executive validation summary
  generate_validation_executive_summary(
    performance_comparison = performance_comparison,
    continuation_validation = continuation_validation,
    output_dir = output_dir
  )
  
  # 2. Detailed performance comparison report
  generate_detailed_comparison_report(
    performance_comparison = performance_comparison,
    output_dir = output_dir
  )
  
  # 3. Boundary utilization report
  generate_boundary_utilization_report(
    boundary_analysis = boundary_analysis,
    output_dir = output_dir
  )
  
  # 4. Continuation logic validation report
  generate_continuation_logic_report(
    continuation_validation = continuation_validation,
    output_dir = output_dir
  )
  
  # 5. Deployment recommendations
  generate_deployment_recommendations(
    performance_comparison = performance_comparison,
    continuation_validation = continuation_validation,
    boundary_analysis = boundary_analysis,
    output_dir = output_dir
  )
  
  # 6. Generate visualizations if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    generate_validation_visualizations(
      deployment_performance = deployment_performance,
      performance_comparison = performance_comparison,
      boundary_analysis = boundary_analysis,
      simulation_results = simulation_results,
      output_dir = output_dir
    )
  }
}

# [Include all remaining unchanged functions here]
# The rest of Module 8 functions remain the same as they already handle
# the enhanced data structures appropriately

# ============================================================================
# Helper Functions
# ============================================================================

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}