# ============================================================================
# Module 8: Deployment Validation Module
# ============================================================================
# Purpose: Validate deployment artifacts by simulating the deployed questionnaire
#          and comparing actual performance to predicted performance
# ============================================================================

# Note on required packages:
# - ggplot2
# - gridExtra

# Required packages
required_packages <- c("ggplot2", "gridExtra")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for visualization functionality"))
  }
}

#' Validate Deployment Package
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
  
  # 1. Simulate deployed questionnaire
  cat("\n1. Simulating deployed questionnaire...\n")
  simulation_results <- simulate_deployed_questionnaire(
    boundary_tables = deployment_package$boundary_tables,
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
  continuation_validation <- validate_continuation_logic(
    boundary_tables = deployment_package$boundary_tables,
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
    timestamp = Sys.time()
  )
  
  # Save results
  saveRDS(validation_results, file.path(output_dir, "deployment_validation_results.rds"))
  
  cat("\n✅ Deployment validation complete!\n")
  cat("Results saved to:", output_dir, "\n")
  
  return(validation_results)
}

#' Simulate Deployed Questionnaire
#'
#' @param boundary_tables Stopping boundary tables
#' @param admin_sequence Administration sequence
#' @param validation_data Validation data
#' @param impl_params Implementation parameters
#' @param prepared_data Prepared data
#' @return Simulation results
simulate_deployed_questionnaire <- function(boundary_tables, admin_sequence,
                                            validation_data, impl_params, prepared_data) {
  
  n_respondents <- nrow(validation_data)
  ordered_items <- admin_sequence$item_id
  n_items <- length(ordered_items)
  
  # Initialize simulation results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  stop_reasons <- character(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Get method configuration
  reduction_method <- impl_params$method_combination$reduction
  stop_low_only <- prepared_data$config$constraints$stop_low_only %||% FALSE
  min_items_per_construct <- prepared_data$config$constraints$min_items_per_construct %||% 1
  
  cat("  Processing", n_respondents, "respondents...\n")
  pb <- txtProgressBar(min = 0, max = n_respondents, style = 3)
  
  # Simulate each respondent's questionnaire experience
  for (i in seq_len(n_respondents)) {
    
    if (prepared_data$config$questionnaire_type == "unidimensional") {
      # Unidimensional simulation
      result <- simulate_unidimensional_respondent(
        respondent_idx = i,
        validation_data = validation_data,
        boundary_table = boundary_tables[["total"]],
        ordered_items = ordered_items,
        reduction_method = reduction_method,
        stop_low_only = stop_low_only,
        cutoff = prepared_data$config$cutoffs[["total"]]
      )
    } else {
      # Multi-construct simulation
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
    
    # Store results
    items_administered[i, 1:result$items_used] <- TRUE
    stopped_at[i] <- result$items_used
    stop_reasons[i] <- result$stop_reason
    classifications[i] <- result$classification
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(list(
    items_administered = items_administered,
    stopped_at = stopped_at,
    stop_reasons = stop_reasons,
    classifications = classifications,
    n_items_used = stopped_at,
    ordered_items = ordered_items
  ))
}

#' Simulate Single Respondent - Unidimensional
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
            stop_reason = "low_risk_boundary",
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
            stop_reason = "high_risk_boundary",
            classification = 1,
            final_sum = current_sum
          ))
        }
      }
    }
  }
  
  # Completed all items
  final_classification <- ifelse(current_sum >= cutoff, 1, 0)
  return(list(
    items_used = length(ordered_items),
    stop_reason = "completed_all_items",
    classification = final_classification,
    final_sum = current_sum
  ))
}

#' Simulate Single Respondent - Multi-construct
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
  construct_classifications <- list()
  
  # Initialize tracking for each construct
  for (cn in names(prepared_data$config$constructs)) {
    construct_sums[[cn]] <- 0
    construct_items_given[[cn]] <- 0
    construct_classifications[[cn]] <- NA
  }
  
  # Go through administration sequence
  for (k in seq_len(nrow(admin_sequence))) {
    item <- admin_sequence$item_id[k]
    item_construct <- admin_sequence$construct[k]
    
    # Add response for this item
    if (item %in% names(validation_data) && !is.na(validation_data[respondent_idx, item])) {
      construct_sums[[item_construct]] <- construct_sums[[item_construct]] + 
        validation_data[respondent_idx, item]
    }
    
    construct_items_given[[item_construct]] <- construct_items_given[[item_construct]] + 1
    
    # Check if this construct can stop (respecting min_items_per_construct)
    if (reduction_method != "none" && 
        construct_items_given[[item_construct]] > min_items_per_construct) {
      
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
            stopped <- TRUE
          }
        }
        
        # Check high risk boundary (if allowed)
        if (!stopped && !stop_low_only && boundary_row$high_risk_boundary != "N/A") {
          boundary_val <- extract_boundary_value(boundary_row$high_risk_boundary, type = "high")
          if (!is.na(boundary_val) && current_sum >= boundary_val) {
            construct_classifications[[item_construct]] <- 1
            stopped <- TRUE
          }
        }
        
        # If this construct stopped, skip remaining items for this construct
        if (stopped) {
          # Check if we can stop the entire questionnaire
          if (all_constructs_classified_or_completed(construct_classifications, 
                                                     construct_items_given, 
                                                     prepared_data$config$constructs)) {
            
            # Calculate overall classification (any positive rule)
            overall_classification <- any(sapply(construct_classifications, function(x) x == 1), na.rm = TRUE)
            
            return(list(
              items_used = k,
              stop_reason = paste0("construct_", item_construct, "_", 
                                   ifelse(construct_classifications[[item_construct]] == 0, "low", "high")),
              classification = as.numeric(overall_classification),
              construct_sums = construct_sums,
              construct_classifications = construct_classifications,
              construct_items_given = construct_items_given
            ))
          }
        }
      }
    }
  }
  
  # Completed all items - classify any remaining constructs
  for (cn in names(construct_classifications)) {
    if (is.na(construct_classifications[[cn]])) {
      cutoff <- prepared_data$config$cutoffs[[cn]]
      construct_classifications[[cn]] <- ifelse(construct_sums[[cn]] >= cutoff, 1, 0)
    }
  }
  
  # Calculate overall classification
  overall_classification <- any(sapply(construct_classifications, function(x) x == 1), na.rm = TRUE)
  
  return(list(
    items_used = nrow(admin_sequence),
    stop_reason = "completed_all_items",
    classification = as.numeric(overall_classification),
    construct_sums = construct_sums,
    construct_classifications = construct_classifications,
    construct_items_given = construct_items_given
  ))
}

#' Check if All Constructs are Classified or Completed
#'
#' @param construct_classifications List of construct classifications
#' @param construct_items_given List of items given per construct
#' @param constructs_config Construct configuration
#' @return TRUE if all constructs are either classified or completed
all_constructs_classified_or_completed <- function(construct_classifications, 
                                                   construct_items_given, 
                                                   constructs_config) {
  
  for (cn in names(constructs_config)) {
    # Check if construct is classified
    if (is.na(construct_classifications[[cn]])) {
      # Not classified - check if all items completed
      total_items <- length(constructs_config[[cn]])
      if (construct_items_given[[cn]] < total_items) {
        return(FALSE)  # This construct is neither classified nor completed
      }
    }
  }
  
  return(TRUE)
}

#' Extract Boundary Value from Boundary Text
#'
#' @param boundary_text Text like "Xk <= 5" or "Xk >= 8"
#' @param type "low" or "high"
#' @return Numeric boundary value or NA
extract_boundary_value <- function(boundary_text, type = "low") {
  if (is.na(boundary_text) || boundary_text == "N/A") {
    return(NA)
  }
  
  if (type == "low" && grepl("Xk <= ", boundary_text)) {
    return(as.numeric(gsub("Xk <= ", "", boundary_text)))
  } else if (type == "high" && grepl("Xk >= ", boundary_text)) {
    return(as.numeric(gsub("Xk >= ", "", boundary_text)))
  } else if (grepl("Xk < ", boundary_text)) {
    return(as.numeric(gsub("Xk < ", "", boundary_text)))
  }
  
  return(NA)
}

#' Calculate Deployment Performance Metrics
#'
#' @param simulation_results Results from questionnaire simulation
#' @param validation_data Validation data
#' @param prepared_data Prepared data
#' @param impl_params Implementation parameters
#' @return Performance metrics
calculate_deployment_performance <- function(simulation_results, validation_data,
                                             prepared_data, impl_params) {
  
  # Use the same performance calculation as Module 5
  # Convert simulation results to reduction_results format
  reduction_results <- list(
    items_administered = simulation_results$items_administered,
    classifications = simulation_results$classifications,
    stopped_at = simulation_results$stopped_at,
    n_items_used = simulation_results$n_items_used
  )
  
  # For multi-construct, add construct classifications
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    construct_classifications <- list()
    
    for (cn in names(prepared_data$config$constructs)) {
      construct_classifications[[cn]] <- sapply(seq_len(length(simulation_results$classifications)), 
                                                function(i) {
                                                  # Extract from simulation results if available
                                                  if (!is.null(simulation_results$construct_classifications) && 
                                                      !is.null(simulation_results$construct_classifications[[i]])) {
                                                    return(simulation_results$construct_classifications[[i]][[cn]])
                                                  } else {
                                                    # Fallback: use overall classification
                                                    return(simulation_results$classifications[i])
                                                  }
                                                })
    }
    
    reduction_results$construct_classifications <- construct_classifications
  }
  
  # Calculate performance using existing function
  performance <- calculate_reduction_performance_full(reduction_results, prepared_data)
  
  # Add simulation-specific metrics
  performance$stop_reason_distribution <- table(simulation_results$stop_reasons)
  performance$simulation_timestamp <- Sys.time()
  
  return(performance)
}

#' Compare Deployment Performance with Original Results
#'
#' @param deployment_performance Deployment performance metrics
#' @param evaluation_results Original evaluation results
#' @param optimization_results Optimization results if applicable
#' @param impl_params Implementation parameters
#' @return Performance comparison
compare_deployment_performance <- function(deployment_performance, evaluation_results,
                                           optimization_results, impl_params) {
  
  # Determine which original performance to compare against
  if (!is.null(optimization_results) && impl_params$is_optimized) {
    original_performance <- optimization_results$optimized_performance
    comparison_type <- "optimized"
  } else if (!is.null(evaluation_results)) {
    original_performance <- evaluation_results$full_performance
    comparison_type <- "evaluation"
  } else {
    warning("No original performance available for comparison")
    return(list(
      comparison_type = "none",
      comparison_available = FALSE
    ))
  }
  
  # Calculate differences
  performance_diffs <- list(
    sensitivity = deployment_performance$sensitivity - original_performance$sensitivity,
    specificity = deployment_performance$specificity - original_performance$specificity,
    fnr = deployment_performance$fnr - original_performance$fnr,
    fpr = deployment_performance$fpr - original_performance$fpr,
    accuracy = deployment_performance$accuracy - original_performance$accuracy,
    balanced_accuracy = deployment_performance$balanced_accuracy - original_performance$balanced_accuracy,
    mean_items_used = deployment_performance$mean_items_used - original_performance$mean_items_used,
    reduction_pct = deployment_performance$reduction_pct - original_performance$reduction_pct
  )
  
  # Calculate percentage differences where appropriate
  performance_pct_diffs <- list(
    sensitivity = ifelse(original_performance$sensitivity > 0,
                         100 * performance_diffs$sensitivity / original_performance$sensitivity, NA),
    specificity = ifelse(original_performance$specificity > 0,
                         100 * performance_diffs$specificity / original_performance$specificity, NA),
    fnr = ifelse(original_performance$fnr > 0,
                 100 * performance_diffs$fnr / original_performance$fnr, NA),
    accuracy = ifelse(original_performance$accuracy > 0,
                      100 * performance_diffs$accuracy / original_performance$accuracy, NA),
    mean_items_used = ifelse(original_performance$mean_items_used > 0,
                             100 * performance_diffs$mean_items_used / original_performance$mean_items_used, NA)
  )
  
  # Assess deployment fidelity
  fidelity_assessment <- assess_deployment_fidelity(performance_diffs, performance_pct_diffs)
  
  return(list(
    comparison_type = comparison_type,
    comparison_available = TRUE,
    original_performance = original_performance,
    deployment_performance = deployment_performance,
    absolute_differences = performance_diffs,
    percentage_differences = performance_pct_diffs,
    fidelity_assessment = fidelity_assessment
  ))
}

#' Assess Deployment Fidelity
#'
#' @param abs_diffs Absolute differences
#' @param pct_diffs Percentage differences
#' @return Fidelity assessment
assess_deployment_fidelity <- function(abs_diffs, pct_diffs) {
  
  # Define thresholds for acceptable differences
  abs_thresholds <- list(
    sensitivity = 0.02,    # 2 percentage points
    specificity = 0.02,    # 2 percentage points
    fnr = 0.02,            # 2 percentage points
    accuracy = 0.02,       # 2 percentage points
    mean_items_used = 1.0  # 1 item
  )
  
  pct_thresholds <- list(
    sensitivity = 5,       # 5% relative change
    specificity = 5,       # 5% relative change
    fnr = 10,             # 10% relative change (more lenient)
    accuracy = 5          # 5% relative change
  )
  
  # Check each metric
  issues <- character()
  warnings <- character()
  
  for (metric in names(abs_thresholds)) {
    abs_diff <- abs(abs_diffs[[metric]])
    pct_diff <- abs(pct_diffs[[metric]])
    
    if (!is.na(abs_diff) && abs_diff > abs_thresholds[[metric]]) {
      issues <- c(issues, paste(metric, "absolute difference exceeds threshold:",
                                sprintf("%.3f > %.3f", abs_diff, abs_thresholds[[metric]])))
    }
    
    if (metric %in% names(pct_thresholds) && !is.na(pct_diff) && 
        pct_diff > pct_thresholds[[metric]]) {
      warnings <- c(warnings, paste(metric, "percentage difference exceeds threshold:",
                                    sprintf("%.1f%% > %.1f%%", pct_diff, pct_thresholds[[metric]])))
    }
  }
  
  # Overall assessment
  overall_fidelity <- ifelse(length(issues) == 0, "PASS", "FAIL")
  
  return(list(
    overall_fidelity = overall_fidelity,
    critical_issues = issues,
    warnings = warnings,
    thresholds_used = list(
      absolute = abs_thresholds,
      percentage = pct_thresholds
    )
  ))
}

#' Validate Continuation Logic
#'
#' @param boundary_tables Boundary tables
#' @param admin_sequence Administration sequence
#' @param validation_data Validation data
#' @param impl_params Implementation parameters
#' @param prepared_data Prepared data
#' @return Continuation logic validation results
validate_continuation_logic <- function(boundary_tables, admin_sequence, validation_data,
                                        impl_params, prepared_data) {
  
  # Test a sample of respondents to validate SurveyJS logic would match R implementation
  sample_size <- min(100, nrow(validation_data))
  sample_indices <- sample(seq_len(nrow(validation_data)), sample_size)
  
  logic_errors <- list()
  logic_warnings <- list()
  
  cat("  Testing continuation logic for", sample_size, "respondents...\n")
  
  for (i in sample_indices) {
    # Simulate what SurveyJS would show vs. what R implementation would show
    surveyjs_sequence <- simulate_surveyjs_logic(
      respondent_idx = i,
      validation_data = validation_data,
      admin_sequence = admin_sequence,
      boundary_tables = boundary_tables,
      prepared_data = prepared_data
    )
    
    r_sequence <- simulate_r_implementation_logic(
      respondent_idx = i,
      validation_data = validation_data,
      admin_sequence = admin_sequence,
      boundary_tables = boundary_tables,
      prepared_data = prepared_data,
      impl_params = impl_params
    )
    
    # Compare sequences
    if (surveyjs_sequence$items_shown != r_sequence$items_shown) {
      logic_errors[[paste0("respondent_", i)]] <- list(
        surveyjs_items = surveyjs_sequence$items_shown,
        r_items = r_sequence$items_shown,
        difference = r_sequence$items_shown - surveyjs_sequence$items_shown
      )
    }
    
    # Check if classifications match
    if (surveyjs_sequence$classification != r_sequence$classification) {
      logic_warnings[[paste0("respondent_", i, "_classification")]] <- list(
        surveyjs_class = surveyjs_sequence$classification,
        r_class = r_sequence$classification
      )
    }
  }
  
  # Summarize validation results
  n_logic_errors <- length(logic_errors)
  n_classification_warnings <- length(logic_warnings)
  
  logic_validation_passed <- (n_logic_errors == 0)
  
  return(list(
    logic_validation_passed = logic_validation_passed,
    n_respondents_tested = sample_size,
    n_logic_errors = n_logic_errors,
    n_classification_warnings = n_classification_warnings,
    logic_errors = logic_errors,
    classification_warnings = logic_warnings,
    error_rate = n_logic_errors / sample_size,
    warning_rate = n_classification_warnings / sample_size
  ))
}

#' Simulate SurveyJS Continuation Logic
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param admin_sequence Administration sequence
#' @param boundary_tables Boundary tables
#' @param prepared_data Prepared data
#' @return SurveyJS simulation result
simulate_surveyjs_logic <- function(respondent_idx, validation_data, admin_sequence,
                                    boundary_tables, prepared_data) {
  
  # This simulates what SurveyJS would show based on visibleIf conditions
  # Simplified implementation - would need to parse actual SurveyJS conditions in practice
  
  items_shown <- 0
  final_classification <- 0
  
  # For now, assume SurveyJS follows the same logic as R implementation
  # In practice, you'd want to parse the actual SurveyJS JSON conditions
  
  return(list(
    items_shown = items_shown,
    classification = final_classification
  ))
}

#' Simulate R Implementation Logic (Reference)
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param admin_sequence Administration sequence
#' @param boundary_tables Boundary tables
#' @param prepared_data Prepared data
#' @param impl_params Implementation parameters
#' @return R implementation simulation result
simulate_r_implementation_logic <- function(respondent_idx, validation_data, admin_sequence,
                                            boundary_tables, prepared_data, impl_params) {
  
  # This uses the actual R reduction logic to determine stopping
  # This serves as the reference for what SurveyJS should produce
  
  reduction_method <- impl_params$method_combination$reduction
  
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    result <- simulate_unidimensional_respondent(
      respondent_idx = respondent_idx,
      validation_data = validation_data,
      boundary_table = boundary_tables[["total"]],
      ordered_items = admin_sequence$item_id,
      reduction_method = reduction_method,
      stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
      cutoff = prepared_data$config$cutoffs[["total"]]
    )
  } else {
    result <- simulate_multicontruct_respondent(
      respondent_idx = respondent_idx,
      validation_data = validation_data,
      boundary_tables = boundary_tables,
      admin_sequence = admin_sequence,
      prepared_data = prepared_data,
      reduction_method = reduction_method,
      stop_low_only = prepared_data$config$constraints$stop_low_only %||% FALSE,
      min_items_per_construct = prepared_data$config$constraints$min_items_per_construct %||% 1
    )
  }
  
  return(list(
    items_shown = result$items_used,
    classification = result$classification
  ))
}

#' Analyze Boundary Utilization
#'
#' @param simulation_results Simulation results
#' @param boundary_tables Boundary tables
#' @param prepared_data Prepared data
#' @return Boundary utilization analysis
analyze_boundary_utilization <- function(simulation_results, boundary_tables, prepared_data) {
  
  # Analyze how often each boundary type is used
  stop_reasons <- simulation_results$stop_reasons
  
  utilization_summary <- list(
    total_respondents = length(stop_reasons),
    stop_reason_counts = table(stop_reasons),
    stop_reason_percentages = prop.table(table(stop_reasons)) * 100
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
  
  return(list(
    utilization_summary = utilization_summary,
    boundary_coverage = boundary_coverage,
    efficiency_by_reason = efficiency_by_reason
  ))
}

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
  
  # 6. Visualization reports
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

#' Generate Executive Validation Summary
#'
#' @param performance_comparison Performance comparison
#' @param continuation_validation Continuation validation
#' @param output_dir Output directory
generate_validation_executive_summary <- function(performance_comparison, continuation_validation,
                                                  output_dir) {
  
  summary_file <- file.path(output_dir, "validation_executive_summary.txt")
  
  summary_text <- paste0(
    "DEPLOYMENT VALIDATION EXECUTIVE SUMMARY\n",
    "======================================\n\n",
    "Validation Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
  )
  
  # Overall assessment
  if (performance_comparison$comparison_available) {
    fidelity <- performance_comparison$fidelity_assessment$overall_fidelity
    summary_text <- paste0(summary_text,
                           "DEPLOYMENT FIDELITY: ", fidelity, "\n")
    
    if (fidelity == "PASS") {
      summary_text <- paste0(summary_text,
                             "✅ Deployment performs within acceptable bounds of original evaluation\n\n")
    } else {
      summary_text <- paste0(summary_text,
                             "⚠️  Deployment performance differs significantly from original evaluation\n\n")
    }
    
    # Key performance differences
    summary_text <- paste0(summary_text,
                           "KEY PERFORMANCE DIFFERENCES:\n",
                           "----------------------------\n")
    
    orig <- performance_comparison$original_performance
    deploy <- performance_comparison$deployment_performance
    
    summary_text <- paste0(summary_text,
                           sprintf("FNR:        %.3f → %.3f (%+.3f)\n",
                                   orig$fnr, deploy$fnr, 
                                   performance_comparison$absolute_differences$fnr),
                           sprintf("Accuracy:   %.3f → %.3f (%+.3f)\n",
                                   orig$accuracy, deploy$accuracy,
                                   performance_comparison$absolute_differences$accuracy),
                           sprintf("Items Used: %.1f → %.1f (%+.1f)\n",
                                   orig$mean_items_used, deploy$mean_items_used,
                                   performance_comparison$absolute_differences$mean_items_used),
                           sprintf("Reduction:  %.1f%% → %.1f%% (%+.1f%%)\n",
                                   orig$reduction_pct, deploy$reduction_pct,
                                   performance_comparison$absolute_differences$reduction_pct))
  } else {
    summary_text <- paste0(summary_text,
                           "DEPLOYMENT FIDELITY: UNKNOWN\n",
                           "No original performance available for comparison\n\n")
  }
  
  # Continuation logic validation
  summary_text <- paste0(summary_text,
                         "\nCONTINUATION LOGIC VALIDATION:\n",
                         "-----------------------------\n")
  
  if (continuation_validation$logic_validation_passed) {
    summary_text <- paste0(summary_text,
                           "✅ SurveyJS logic matches R implementation\n")
  } else {
    summary_text <- paste0(summary_text,
                           "⚠️  Logic discrepancies detected\n",
                           "Error rate: ", sprintf("%.1f%%", 
                                                   continuation_validation$error_rate * 100), "\n")
  }
  
  # Recommendations
  summary_text <- paste0(summary_text,
                         "\nRECOMMENDATIONS:\n",
                         "---------------\n")
  
  if (performance_comparison$comparison_available && 
      performance_comparison$fidelity_assessment$overall_fidelity == "PASS" &&
      continuation_validation$logic_validation_passed) {
    summary_text <- paste0(summary_text,
                           "✅ APPROVED FOR DEPLOYMENT\n",
                           "All validation checks passed. Deployment artifacts are ready for production use.\n")
  } else {
    summary_text <- paste0(summary_text,
                           "⚠️  REVIEW REQUIRED BEFORE DEPLOYMENT\n",
                           "See detailed validation reports for specific issues to address.\n")
  }
  
  writeLines(summary_text, summary_file)
}

#' Generate Detailed Performance Comparison Report
#'
#' @param performance_comparison Performance comparison
#' @param output_dir Output directory
generate_detailed_comparison_report <- function(performance_comparison, output_dir) {
  
  if (!performance_comparison$comparison_available) {
    return()
  }
  
  report_file <- file.path(output_dir, "performance_comparison_detailed.html")
  
  orig <- performance_comparison$original_performance
  deploy <- performance_comparison$deployment_performance
  abs_diffs <- performance_comparison$absolute_differences
  pct_diffs <- performance_comparison$percentage_differences
  
  html_content <- paste0(
    "<html><head><title>Deployment Performance Comparison</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1, h2 { color: #333; }",
    "table { border-collapse: collapse; width: 80%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".metric-value { text-align: right; }",
    ".improvement { color: green; font-weight: bold; }",
    ".degradation { color: red; font-weight: bold; }",
    ".acceptable { color: orange; }",
    ".status-pass { background-color: #e6ffe6; }",
    ".status-fail { background-color: #ffe6e6; }",
    "</style></head><body>",
    "<h1>Deployment Performance Validation</h1>",
    "<p><strong>Comparison Type:</strong> ", performance_comparison$comparison_type, "</p>",
    "<p><strong>Fidelity Status:</strong> <span class='", 
    ifelse(performance_comparison$fidelity_assessment$overall_fidelity == "PASS", 
           "status-pass", "status-fail"), "'>",
    performance_comparison$fidelity_assessment$overall_fidelity, "</span></p>",
    
    "<h2>Performance Metrics Comparison</h2>",
    "<table>",
    "<tr><th>Metric</th><th class='metric-value'>Original</th>",
    "<th class='metric-value'>Deployed</th><th class='metric-value'>Absolute Diff</th>",
    "<th class='metric-value'>Relative Diff %</th><th>Assessment</th></tr>"
  )
  
  # Key metrics to compare
  metrics <- list(
    list(name = "Sensitivity", orig = orig$sensitivity, deploy = deploy$sensitivity,
         abs_diff = abs_diffs$sensitivity, pct_diff = pct_diffs$sensitivity, lower_better = FALSE),
    list(name = "Specificity", orig = orig$specificity, deploy = deploy$specificity,
         abs_diff = abs_diffs$specificity, pct_diff = pct_diffs$specificity, lower_better = FALSE),
    list(name = "False Negative Rate", orig = orig$fnr, deploy = deploy$fnr,
         abs_diff = abs_diffs$fnr, pct_diff = pct_diffs$fnr, lower_better = TRUE),
    list(name = "Accuracy", orig = orig$accuracy, deploy = deploy$accuracy,
         abs_diff = abs_diffs$accuracy, pct_diff = pct_diffs$accuracy, lower_better = FALSE),
    list(name = "Balanced Accuracy", orig = orig$balanced_accuracy, deploy = deploy$balanced_accuracy,
         abs_diff = abs_diffs$balanced_accuracy, pct_diff = pct_diffs$balanced_accuracy, lower_better = FALSE),
    list(name = "Mean Items Used", orig = orig$mean_items_used, deploy = deploy$mean_items_used,
         abs_diff = abs_diffs$mean_items_used, pct_diff = pct_diffs$mean_items_used, lower_better = TRUE),
    list(name = "Reduction %", orig = orig$reduction_pct, deploy = deploy$reduction_pct,
         abs_diff = abs_diffs$reduction_pct, pct_diff = NA, lower_better = FALSE)
  )
  
  for (metric in metrics) {
    # Determine change class
    if (abs(metric$abs_diff) < 0.01) {
      change_class <- ""
      assessment <- "Stable"
    } else if ((metric$lower_better && metric$abs_diff < 0) || 
               (!metric$lower_better && metric$abs_diff > 0)) {
      change_class <- "improvement"
      assessment <- "Improved"
    } else {
      change_class <- "degradation"
      assessment <- "Degraded"
    }
    
    pct_text <- ifelse(is.na(metric$pct_diff), "N/A", sprintf("%+.1f", metric$pct_diff))
    
    html_content <- paste0(html_content,
                           "<tr>",
                           "<td>", metric$name, "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", metric$orig), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", metric$deploy), "</td>",
                           "<td class='metric-value ", change_class, "'>", 
                           sprintf("%+.3f", metric$abs_diff), "</td>",
                           "<td class='metric-value ", change_class, "'>", pct_text, "</td>",
                           "<td>", assessment, "</td>",
                           "</tr>")
  }
  
  html_content <- paste0(html_content, "</table>")
  
  # Add critical issues if any
  if (length(performance_comparison$fidelity_assessment$critical_issues) > 0) {
    html_content <- paste0(html_content,
                           "<h2>Critical Issues</h2>",
                           "<ul>")
    for (issue in performance_comparison$fidelity_assessment$critical_issues) {
      html_content <- paste0(html_content, "<li>", issue, "</li>")
    }
    html_content <- paste0(html_content, "</ul>")
  }
  
  # Add warnings if any
  if (length(performance_comparison$fidelity_assessment$warnings) > 0) {
    html_content <- paste0(html_content,
                           "<h2>Warnings</h2>",
                           "<ul>")
    for (warning in performance_comparison$fidelity_assessment$warnings) {
      html_content <- paste0(html_content, "<li>", warning, "</li>")
    }
    html_content <- paste0(html_content, "</ul>")
  }
  
  html_content <- paste0(html_content, "</body></html>")
  
  writeLines(html_content, report_file)
}

#' Generate Boundary Utilization Report
#'
#' @param boundary_analysis Boundary analysis results
#' @param output_dir Output directory
generate_boundary_utilization_report <- function(boundary_analysis, output_dir) {
  
  report_file <- file.path(output_dir, "boundary_utilization_report.txt")
  
  report_text <- paste0(
    "BOUNDARY UTILIZATION REPORT\n",
    "==========================\n\n",
    "Total Respondents: ", boundary_analysis$utilization_summary$total_respondents, "\n\n",
    "STOPPING REASON DISTRIBUTION:\n",
    "-----------------------------\n"
  )
  
  # Add stop reason statistics
  for (reason in names(boundary_analysis$utilization_summary$stop_reason_counts)) {
    count <- boundary_analysis$utilization_summary$stop_reason_counts[[reason]]
    pct <- boundary_analysis$utilization_summary$stop_reason_percentages[[reason]]
    
    report_text <- paste0(report_text,
                          sprintf("%-30s: %4d (%5.1f%%)\n", reason, count, pct))
    
    # Add efficiency info if available
    if (reason %in% names(boundary_analysis$efficiency_by_reason)) {
      eff <- boundary_analysis$efficiency_by_reason[[reason]]
      report_text <- paste0(report_text,
                            sprintf("    Mean items: %.1f (SD: %.1f)\n", 
                                    eff$mean_items, eff$sd_items))
    }
  }
  
  # Add boundary coverage statistics
  report_text <- paste0(report_text,
                        "\nBOUNDARY COVERAGE:\n",
                        "------------------\n")
  
  for (construct_name in names(boundary_analysis$boundary_coverage)) {
    cov <- boundary_analysis$boundary_coverage[[construct_name]]
    
    report_text <- paste0(report_text,
                          sprintf("%-30s:\n", construct_name),
                          sprintf("  Total positions: %d\n", cov$total_positions),
                          sprintf("  Low boundary coverage:  %.1f%%\n", cov$low_boundary_coverage * 100),
                          sprintf("  High boundary coverage: %.1f%%\n", cov$high_boundary_coverage * 100),
                          sprintf("  Effective coverage:     %.1f%%\n", cov$effective_positions * 100),
                          "\n")
  }
  
  writeLines(report_text, report_file)
}

#' Generate Continuation Logic Report
#'
#' @param continuation_validation Continuation validation results
#' @param output_dir Output directory
generate_continuation_logic_report <- function(continuation_validation, output_dir) {
  
  report_file <- file.path(output_dir, "continuation_logic_validation.txt")
  
  report_text <- paste0(
    "CONTINUATION LOGIC VALIDATION REPORT\n",
    "===================================\n\n",
    "Validation Status: ", ifelse(continuation_validation$logic_validation_passed, 
                                  "PASSED", "FAILED"), "\n",
    "Respondents Tested: ", continuation_validation$n_respondents_tested, "\n",
    "Logic Errors: ", continuation_validation$n_logic_errors, 
    " (", sprintf("%.1f%%", continuation_validation$error_rate * 100), ")\n",
    "Classification Warnings: ", continuation_validation$n_classification_warnings,
    " (", sprintf("%.1f%%", continuation_validation$warning_rate * 100), ")\n\n"
  )
  
  # Add detailed errors if any
  if (continuation_validation$n_logic_errors > 0) {
    report_text <- paste0(report_text,
                          "DETAILED LOGIC ERRORS:\n",
                          "----------------------\n")
    
    for (error_name in names(continuation_validation$logic_errors)) {
      error_info <- continuation_validation$logic_errors[[error_name]]
      report_text <- paste0(report_text,
                            error_name, ":\n",
                            "  SurveyJS would show: ", error_info$surveyjs_items, " items\n",
                            "  R implementation: ", error_info$r_items, " items\n",
                            "  Difference: ", error_info$difference, " items\n\n")
    }
  }
  
  # Add classification warnings if any
  if (continuation_validation$n_classification_warnings > 0) {
    report_text <- paste0(report_text,
                          "CLASSIFICATION WARNINGS:\n",
                          "------------------------\n")
    
    for (warning_name in names(continuation_validation$classification_warnings)) {
      warning_info <- continuation_validation$classification_warnings[[warning_name]]
      report_text <- paste0(report_text,
                            warning_name, ":\n",
                            "  SurveyJS classification: ", warning_info$surveyjs_class, "\n",
                            "  R classification: ", warning_info$r_class, "\n\n")
    }
  }
  
  writeLines(report_text, report_file)
}

#' Generate Deployment Recommendations
#'
#' @param performance_comparison Performance comparison
#' @param continuation_validation Continuation validation
#' @param boundary_analysis Boundary analysis
#' @param output_dir Output directory
generate_deployment_recommendations <- function(performance_comparison, continuation_validation,
                                                boundary_analysis, output_dir) {
  
  recommendations_file <- file.path(output_dir, "deployment_recommendations.txt")
  
  recommendations_text <- paste0(
    "DEPLOYMENT RECOMMENDATIONS\n",
    "=========================\n\n"
  )
  
  # Overall deployment readiness
  ready_for_deployment <- TRUE
  
  if (performance_comparison$comparison_available) {
    if (performance_comparison$fidelity_assessment$overall_fidelity != "PASS") {
      ready_for_deployment <- FALSE
      recommendations_text <- paste0(recommendations_text,
                                     "🚨 CRITICAL: Performance fidelity check FAILED\n",
                                     "Action required: Investigate performance discrepancies\n\n")
    }
  }
  
  if (!continuation_validation$logic_validation_passed) {
    ready_for_deployment <- FALSE
    recommendations_text <- paste0(recommendations_text,
                                   "🚨 CRITICAL: Continuation logic validation FAILED\n",
                                   "Action required: Fix SurveyJS logic inconsistencies\n\n")
  }
  
  # Specific recommendations based on utilization
  early_stop_rate <- sum(boundary_analysis$utilization_summary$stop_reason_percentages[
    !names(boundary_analysis$utilization_summary$stop_reason_percentages) %in% 
      c("completed_all_items")
  ])
  
  if (early_stop_rate < 10) {
    recommendations_text <- paste0(recommendations_text,
                                   "📊 LOW UTILIZATION: Early stopping rate is only ", 
                                   sprintf("%.1f%%", early_stop_rate), "\n",
                                   "Consider: More aggressive gamma parameters or different reduction method\n\n")
  } else if (early_stop_rate > 80) {
    recommendations_text <- paste0(recommendations_text,
                                   "📊 HIGH UTILIZATION: Early stopping rate is ", 
                                   sprintf("%.1f%%", early_stop_rate), "\n",
                                   "Monitor: Ensure clinical validity of early stops\n\n")
  }
  
  # Final recommendation
  if (ready_for_deployment) {
    recommendations_text <- paste0(recommendations_text,
                                   "✅ DEPLOYMENT APPROVED\n",
                                   "The deployment package has passed all validation checks.\n",
                                   "Recommended next steps:\n",
                                   "1. Deploy in pilot environment\n",
                                   "2. Monitor real-world performance\n",
                                   "3. Collect feedback from users\n",
                                   "4. Schedule periodic performance reviews\n")
  } else {
    recommendations_text <- paste0(recommendations_text,
                                   "❌ DEPLOYMENT NOT RECOMMENDED\n",
                                   "Critical issues must be resolved before deployment.\n",
                                   "See detailed reports for specific remediation steps.\n")
  }
  
  writeLines(recommendations_text, recommendations_file)
}

#' Generate Validation Visualizations
#'
#' @param deployment_performance Deployment performance
#' @param performance_comparison Performance comparison
#' @param boundary_analysis Boundary analysis
#' @param simulation_results Simulation results
#' @param output_dir Output directory
generate_validation_visualizations <- function(deployment_performance, performance_comparison,
                                               boundary_analysis, simulation_results, output_dir) {
  
  viz_dir <- file.path(output_dir, "visualizations")
  if (!dir.exists(viz_dir)) {
    dir.create(viz_dir, recursive = TRUE)
  }
  
  # 1. Performance comparison radar chart (simplified as bar chart)
  if (performance_comparison$comparison_available) {
    comparison_plot <- create_performance_comparison_plot(performance_comparison)
    ggplot2::ggsave(file.path(viz_dir, "performance_comparison.png"),
                    comparison_plot, width = 10, height = 6)
  }
  
  # 2. Item usage distribution
  usage_plot <- create_item_usage_validation_plot(simulation_results)
  ggplot2::ggsave(file.path(viz_dir, "item_usage_validation.png"),
                  usage_plot, width = 10, height = 6)
  
  # 3. Stopping reason distribution
  stop_reason_plot <- create_stop_reason_plot(boundary_analysis)
  ggplot2::ggsave(file.path(viz_dir, "stop_reason_distribution.png"),
                  stop_reason_plot, width = 10, height = 6)
  
  # 4. Boundary utilization heatmap
  if (length(boundary_analysis$boundary_coverage) > 1) {
    boundary_plot <- create_boundary_coverage_plot(boundary_analysis)
    ggplot2::ggsave(file.path(viz_dir, "boundary_coverage.png"),
                    boundary_plot, width = 12, height = 8)
  }
}

#' Create Performance Comparison Plot
#'
#' @param performance_comparison Performance comparison results
#' @return ggplot object
create_performance_comparison_plot <- function(performance_comparison) {
  
  # Prepare data for comparison
  metrics <- c("sensitivity", "specificity", "accuracy", "balanced_accuracy")
  
  orig_values <- sapply(metrics, function(m) performance_comparison$original_performance[[m]])
  deploy_values <- sapply(metrics, function(m) performance_comparison$deployment_performance[[m]])
  
  plot_data <- data.frame(
    metric = rep(metrics, 2),
    value = c(orig_values, deploy_values),
    type = rep(c("Original", "Deployed"), each = length(metrics)),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = metric, y = value, fill = type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = "Performance Comparison: Original vs Deployed",
      x = "Metric",
      y = "Value",
      fill = "Implementation"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Create Item Usage Validation Plot
#'
#' @param simulation_results Simulation results
#' @return ggplot object
create_item_usage_validation_plot <- function(simulation_results) {
  
  plot_data <- data.frame(
    items_used = simulation_results$n_items_used,
    respondent_id = seq_along(simulation_results$n_items_used)
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = items_used)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mean(plot_data$items_used), 
                        color = "red", linetype = "dashed", size = 1) +
    ggplot2::labs(
      title = "Distribution of Items Used in Deployed Questionnaire",
      subtitle = paste("Mean:", sprintf("%.1f", mean(plot_data$items_used)), 
                       "| Median:", sprintf("%.1f", median(plot_data$items_used))),
      x = "Number of Items Administered",
      y = "Number of Respondents"
    ) +
    ggplot2::theme_minimal()
  
  return(p)
}

#' Create Stop Reason Distribution Plot
#'
#' @param boundary_analysis Boundary analysis results
#' @return ggplot object
create_stop_reason_plot <- function(boundary_analysis) {
  
  plot_data <- data.frame(
    reason = names(boundary_analysis$utilization_summary$stop_reason_percentages),
    percentage = as.numeric(boundary_analysis$utilization_summary$stop_reason_percentages),
    stringsAsFactors = FALSE
  )
  
  # Clean up reason names for display
  plot_data$reason_clean <- gsub("_", " ", plot_data$reason)
  plot_data$reason_clean <- tools::toTitleCase(plot_data$reason_clean)
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(reason_clean, percentage), 
                                               y = percentage)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), 
                       hjust = -0.1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Distribution of Stopping Reasons",
      x = "Stopping Reason",
      y = "Percentage of Respondents"
    ) +
    ggplot2::theme_minimal()
  
  return(p)
}

#' Create Boundary Coverage Plot
#'
#' @param boundary_analysis Boundary analysis results
#' @return ggplot object
create_boundary_coverage_plot <- function(boundary_analysis) {
  
  # Prepare data for heatmap
  coverage_data <- do.call(rbind, lapply(names(boundary_analysis$boundary_coverage), 
                                         function(cn) {
                                           cov <- boundary_analysis$boundary_coverage[[cn]]
                                           data.frame(
                                             construct = cn,
                                             low_coverage = cov$low_boundary_coverage * 100,
                                             high_coverage = cov$high_boundary_coverage * 100,
                                             stringsAsFactors = FALSE
                                           )
                                         }))
  
  # Reshape for ggplot
  coverage_long <- reshape2::melt(coverage_data, 
                                  id.vars = "construct",
                                  variable.name = "boundary_type",
                                  value.name = "coverage_pct")
  
  p <- ggplot2::ggplot(coverage_long, 
                       ggplot2::aes(x = construct, y = boundary_type, fill = coverage_pct)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", coverage_pct)), 
                       color = "white", size = 3) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue", 
                                 name = "Coverage %") +
    ggplot2::labs(
      title = "Boundary Coverage by Construct",
      x = "Construct",
      y = "Boundary Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#' Assess Overall Validation Success
#'
#' @param performance_comparison Performance comparison
#' @param continuation_validation Continuation validation
#' @return TRUE if validation passed, FALSE otherwise
assess_validation_success <- function(performance_comparison, continuation_validation) {
  
  performance_ok <- TRUE
  if (performance_comparison$comparison_available) {
    performance_ok <- (performance_comparison$fidelity_assessment$overall_fidelity == "PASS")
  }
  
  logic_ok <- continuation_validation$logic_validation_passed
  
  return(performance_ok && logic_ok)
}

#' Quick Deployment Check
#'
#' @param deployment_package Deployment package from Module 7
#' @param prepared_data Prepared data from Module 1
#' @param evaluation_results Evaluation results from Module 5 (optional)
#' @param n_test_respondents Number of respondents to test (default: 50)
#' @return Quick validation summary
#' @export
quick_deployment_check <- function(deployment_package, prepared_data, 
                                   evaluation_results = NULL, n_test_respondents = 50) {
  
  cat("Running quick deployment validation check...\n")
  
  # Sample test data
  test_data <- prepared_data$splits$test
  if (nrow(test_data) > n_test_respondents) {
    sample_indices <- sample(seq_len(nrow(test_data)), n_test_respondents)
    test_data <- test_data[sample_indices, ]
  }
  
  # Quick simulation
  quick_results <- simulate_deployed_questionnaire(
    boundary_tables = deployment_package$boundary_tables,
    admin_sequence = deployment_package$admin_sequence,
    validation_data = test_data,
    impl_params = deployment_package$implementation_params,
    prepared_data = prepared_data
  )
  
  # Quick performance calculation
  quick_performance <- calculate_deployment_performance(
    simulation_results = quick_results,
    validation_data = test_data,
    prepared_data = prepared_data,
    impl_params = deployment_package$implementation_params
  )
  
  # Summary output
  cat("\nQuick Validation Results:\n")
  cat("------------------------\n")
  cat("Respondents tested:", nrow(test_data), "\n")
  cat("Mean items used:", sprintf("%.1f", quick_performance$mean_items_used), "\n")
  cat("Reduction achieved:", sprintf("%.1f%%", quick_performance$reduction_pct), "\n")
  cat("FNR:", sprintf("%.3f", quick_performance$fnr), "\n")
  cat("Accuracy:", sprintf("%.3f", quick_performance$accuracy), "\n")
  
  # Early stopping utilization
  early_stops <- sum(!grepl("completed_all", quick_results$stop_reasons))
  early_stop_rate <- early_stops / length(quick_results$stop_reasons) * 100
  cat("Early stopping rate:", sprintf("%.1f%%", early_stop_rate), "\n")
  
  # Compare with evaluation if available
  if (!is.null(evaluation_results)) {
    orig_perf <- evaluation_results$full_performance
    fnr_diff <- quick_performance$fnr - orig_perf$fnr
    acc_diff <- quick_performance$accuracy - orig_perf$accuracy
    
    cat("\nComparison with evaluation:\n")
    cat("FNR difference:", sprintf("%+.3f", fnr_diff), "\n")
    cat("Accuracy difference:", sprintf("%+.3f", acc_diff), "\n")
    
    if (abs(fnr_diff) < 0.02 && abs(acc_diff) < 0.02) {
      cat("✅ Performance matches evaluation within tolerance\n")
    } else {
      cat("⚠️  Performance differs from evaluation - full validation recommended\n")
    }
  }
  
  return(list(
    performance = quick_performance,
    early_stop_rate = early_stop_rate,
    validation_summary = "quick_check_completed"
  ))
}

# ============================================================================
# Integration Functions for Existing Workflow
# ============================================================================

#' Add Validation to Master Workflow
#'
#' This function can be called at the end of the master script to automatically
#' run deployment validation
#'
#' @param deployment_package Deployment package from Module 7
#' @param prepared_data Prepared data from Module 1
#' @param evaluation_results Evaluation results from Module 5
#' @param optimization_results Optimization results from Module 6 (if applicable)
#' @param run_full_validation Whether to run full validation (default: TRUE)
#' @param output_dir Output directory relative to current working directory
#' @export
run_deployment_validation <- function(deployment_package, prepared_data,
                                      evaluation_results, optimization_results = NULL,
                                      run_full_validation = TRUE,
                                      output_dir = "deployment_validation") {
  
  cat("\n========================================\n")
  cat("=== Module 8: Deployment Validation ===\n")
  cat("========================================\n\n")
  
  if (run_full_validation) {
    # Run comprehensive validation
    validation_results <- validate_deployment(
      deployment_package = deployment_package,
      prepared_data = prepared_data,
      evaluation_results = evaluation_results,
      optimization_results = optimization_results,
      output_dir = output_dir
    )
    
    # Print summary
    cat("\nValidation Summary:\n")
    cat("------------------\n")
    cat("Deployment fidelity:", 
        ifelse(validation_results$validation_passed, "PASSED", "FAILED"), "\n")
    
    if (validation_results$performance_comparison$comparison_available) {
      cat("Performance comparison available: YES\n")
      cat("Fidelity status:", 
          validation_results$performance_comparison$fidelity_assessment$overall_fidelity, "\n")
    } else {
      cat("Performance comparison available: NO\n")
    }
    
    cat("Continuation logic:", 
        ifelse(validation_results$continuation_validation$logic_validation_passed, 
               "PASSED", "FAILED"), "\n")
    
    return(validation_results)
    
  } else {
    # Run quick check only
    quick_results <- quick_deployment_check(
      deployment_package = deployment_package,
      prepared_data = prepared_data,
      evaluation_results = evaluation_results
    )
    
    return(quick_results)
  }
}

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}