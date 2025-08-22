# ============================================================================
# Module 8: Deployment Validation Module (JSON-BASED VALIDATION)
# ============================================================================
# Purpose: Validate deployment artifacts by simulating the actual JSON logic
#          and comparing actual performance to predicted performance
# ============================================================================

# Required packages
required_packages <- c("ggplot2", "gridExtra", "jsonlite")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is required for this module"))
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

#' Validate Deployment Package (MAIN ENTRY POINT)
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
  
  # 1. Simulate deployed questionnaire using JSON
  cat("\n1. Simulating deployed questionnaire from JSON...\n")
  simulation_results <- simulate_deployed_questionnaire(
    boundary_tables = deployment_package$boundary_tables,
    pattern_rules = deployment_package$pattern_rules,
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
  continuation_validation <- validate_continuation_logic_json_based(
    simulation_results = simulation_results,
    admin_sequence = deployment_package$admin_sequence,
    validation_data = validation_data,
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
    json_based = TRUE,
    timestamp = Sys.time()
  )
  
  # Save results
  saveRDS(validation_results, file.path(output_dir, "deployment_validation_results.rds"))
  
  cat("\n✅ Deployment validation complete!\n")
  cat("Results saved to:", output_dir, "\n")
  
  return(validation_results)
}

#' Simulate Deployed Questionnaire (JSON-BASED)
#'
#' This simulates what the actual SurveyJS implementation would do
#' based on the JSON visibility conditions
#'
#' @param boundary_tables Stopping boundary tables (not used in JSON simulation)
#' @param pattern_rules Pattern-specific rules (not used in JSON simulation)
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
  
  # Load the actual JSON to simulate
  surveyjs_json_file <- file.path("deployment", "surveyjs_config.json")
  if (!file.exists(surveyjs_json_file)) {
    stop("surveyjs_config.json not found. Please ensure Module 7 has been run.")
  }
  
  # Load JSON configuration
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required to load JSON configuration")
  }
  
  surveyjs_config <- jsonlite::fromJSON(surveyjs_json_file)
  
  cat("  Processing", n_respondents, "respondents...\n")
  cat("  Simulating actual SurveyJS visibility logic from JSON\n")
  
  pb <- txtProgressBar(min = 0, max = n_respondents, style = 3)
  
  # Simulate each respondent
  for (i in seq_len(n_respondents)) {
    
    # Simulate based on JSON visibility conditions
    result <- simulate_json_logic_for_respondent(
      respondent_idx = i,
      validation_data = validation_data,
      surveyjs_pages = surveyjs_config$pages,
      admin_sequence = admin_sequence,
      prepared_data = prepared_data
    )
    
    # Store results
    for (item in result$items_administered) {
      item_idx <- which(ordered_items == item)
      if (length(item_idx) > 0) {
        items_administered[i, item_idx[1]] <- TRUE
      }
    }
    
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
    ordered_items = ordered_items,
    json_based = TRUE  # Flag that this used JSON simulation
  ))
}

#' Simulate JSON Logic for a Single Respondent
#'
#' @param respondent_idx Respondent index
#' @param validation_data Validation data
#' @param surveyjs_pages Pages from JSON config
#' @param admin_sequence Administration sequence
#' @param prepared_data Prepared data
#' @return Simulation result
simulate_json_logic_for_respondent <- function(respondent_idx, validation_data,
                                               surveyjs_pages, admin_sequence,
                                               prepared_data) {
  
  responses_collected <- list()
  items_administered <- character()
  
  # Process each page
  for (page in surveyjs_pages) {
    # Skip completion page
    if (!is.null(page$name) && page$name == "completion") next
    
    # Get question from page
    if (is.null(page$elements) || length(page$elements) == 0) next
    question <- page$elements[[1]]
    if (is.null(question)) next
    
    item_name <- question$name
    
    # Check visibility
    is_visible <- TRUE
    
    # Check isRequired first (always visible)
    if (!is.null(question$isRequired) && question$isRequired) {
      is_visible <- TRUE
    }
    # Then check visibleIf condition
    else if (!is.null(question$visibleIf)) {
      # Evaluate visibility based on COLLECTED responses only
      is_visible <- evaluate_visibility_with_collected_responses(
        condition = question$visibleIf,
        collected_responses = responses_collected
      )
    }
    
    if (is_visible) {
      # Item is shown - collect response
      items_administered <- c(items_administered, item_name)
      
      if (item_name %in% names(validation_data)) {
        response_value <- validation_data[respondent_idx, item_name]
        if (!is.na(response_value)) {
          responses_collected[[item_name]] <- response_value
        }
      }
    }
    # If not visible, item is skipped (no response collected)
  }
  
  # Determine stop reason and classification
  n_items_total <- nrow(admin_sequence)
  n_items_used <- length(items_administered)
  
  if (n_items_used < n_items_total) {
    stop_reason <- "stopped_early_per_json"
  } else {
    stop_reason <- "completed_all_items"
  }
  
  # Calculate classification based on collected responses
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    total_score <- sum(unlist(responses_collected), na.rm = TRUE)
    cutoff <- prepared_data$config$cutoffs[["total"]]
    classification <- ifelse(total_score >= cutoff, 1, 0)
  } else {
    # Multi-construct classification
    construct_classifications <- list()
    for (cn in names(prepared_data$config$constructs)) {
      construct_items <- prepared_data$config$constructs[[cn]]
      construct_responses <- responses_collected[names(responses_collected) %in% construct_items]
      construct_sum <- sum(unlist(construct_responses), na.rm = TRUE)
      cutoff <- prepared_data$config$cutoffs[[cn]]
      construct_classifications[[cn]] <- ifelse(construct_sum >= cutoff, 1, 0)
    }
    classification <- ifelse(sum(unlist(construct_classifications)) > 0, 1, 0)
  }
  
  return(list(
    items_used = n_items_used,
    items_administered = items_administered,
    responses_collected = responses_collected,
    stop_reason = stop_reason,
    classification = classification
  ))
}

#' Evaluate Visibility Condition with Collected Responses
#'
#' @param condition Visibility condition string
#' @param collected_responses Responses collected so far
#' @return Boolean for visibility
evaluate_visibility_with_collected_responses <- function(condition, collected_responses) {
  
  # Replace item references with values for collected responses
  eval_condition <- condition
  
  for (item_name in names(collected_responses)) {
    pattern <- paste0("\\{", item_name, "\\}")
    replacement <- as.character(collected_responses[[item_name]])
    eval_condition <- gsub(pattern, replacement, eval_condition)
  }
  
  # Check if there are still unreplaced item references
  # These would be items that weren't shown/collected
  if (grepl("\\{[^}]+\\}", eval_condition)) {
    # Can't evaluate condition with missing items
    # Conservative approach: assume visible
    return(TRUE)
  }
  
  # Convert to R syntax
  eval_condition <- gsub(" and ", " & ", eval_condition)
  eval_condition <- gsub(" or ", " | ", eval_condition)
  
  # Evaluate
  tryCatch({
    result <- eval(parse(text = eval_condition))
    return(as.logical(result))
  }, error = function(e) {
    warning(paste("Failed to evaluate condition:", condition))
    return(TRUE)  # Default to visible if can't evaluate
  })
}

#' Validate Continuation Logic (JSON-BASED VERSION)
#'
#' @param simulation_results Simulation results
#' @param admin_sequence Administration sequence
#' @param validation_data Validation data
#' @param prepared_data Prepared data
#' @return Logic validation results
validate_continuation_logic_json_based <- function(simulation_results, admin_sequence,
                                                   validation_data, prepared_data) {
  
  # Validate that stopping reasons make sense
  stop_reasons <- table(simulation_results$stop_reasons)
  
  # Check for logical consistency
  logic_checks <- list()
  
  # 1. Check that some respondents stopped early
  early_stop_count <- sum(simulation_results$stop_reasons == "stopped_early_per_json")
  complete_count <- sum(simulation_results$stop_reasons == "completed_all_items")
  
  logic_checks$early_stopping_present <- early_stop_count > 0
  logic_checks$some_completed_all <- complete_count > 0
  
  # 2. Check that items used makes sense
  logic_checks$items_used_valid <- all(simulation_results$n_items_used >= 1) && 
    all(simulation_results$n_items_used <= nrow(admin_sequence))
  
  # 3. Check that classifications are binary
  logic_checks$classifications_binary <- all(simulation_results$classifications %in% c(0, 1))
  
  # Overall validation
  logic_validation_passed <- all(unlist(logic_checks))
  
  return(list(
    logic_validation_passed = logic_validation_passed,
    logic_checks = logic_checks,
    stop_reason_distribution = stop_reasons,
    json_based = TRUE,
    message = ifelse(logic_validation_passed, 
                     "JSON-based continuation logic validation passed",
                     "Issues detected in JSON-based continuation logic")
  ))
}

# ============================================================================
# Keep existing functions that don't need changes
# ============================================================================

# These functions work with the simulation results regardless of how they were generated:
# - calculate_deployment_performance [Y]
# - compare_deployment_performance [Y]
# - assess_deployment_fidelity [Y]
# - analyze_boundary_utilization [Y]
# - generate_validation_reports [Y]
# - generate_validation_executive_summary [Y]
# - generate_detailed_comparison_report [Y]
# - generate_boundary_utilization_report [Y]
# - generate_continuation_logic_report [Y]
# - generate_deployment_recommendations [Y]
# - generate_validation_visualizations [Y]
# - assess_validation_success [Y]

# [INSERT ALL THESE FUNCTIONS FROM YOUR ORIGINAL FILE HERE]

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

#' Generate Validation Executive Summary (UPDATED - All Metrics)
#'
#' @param performance_comparison Performance comparison
#' @param continuation_validation Continuation validation
#' @param output_dir Output directory
generate_validation_executive_summary <- function(performance_comparison, continuation_validation, 
                                                  output_dir) {
  
  summary_file <- file.path(output_dir, "validation_executive_summary.txt")
  
  summary_text <- paste0(
    "DEPLOYMENT VALIDATION EXECUTIVE SUMMARY\n",
    "========================================\n\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
    
    "OVERALL STATUS\n",
    "--------------\n"
  )
  
  # Performance comparison status
  if (performance_comparison$comparison_available) {
    baseline_label <- ifelse(performance_comparison$comparison_type == "optimized", 
                             "Optimized", "Original")
    
    summary_text <- paste0(summary_text,
                           "Performance Fidelity: ", 
                           performance_comparison$fidelity_assessment$overall_fidelity, "\n",
                           "Comparison Type: ", baseline_label, " vs Deployed\n")
  } else {
    summary_text <- paste0(summary_text,
                           "Performance Comparison: Not available (no baseline)\n")
  }
  
  # Logic validation status
  summary_text <- paste0(summary_text,
                         "Continuation Logic: ",
                         ifelse(continuation_validation$logic_validation_passed, "PASSED", "FAILED"), "\n\n")
  
  # Key metrics comparison - UPDATED to include all metrics
  if (performance_comparison$comparison_available) {
    baseline_label <- ifelse(performance_comparison$comparison_type == "optimized", 
                             "Optimized", "Original")
    
    summary_text <- paste0(summary_text,
                           "KEY METRICS COMPARISON\n",
                           "---------------------\n",
                           sprintf("%-20s  %10s  %10s  %10s\n", "Metric", baseline_label, "Deployed", "Difference"))
    
    # Include ALL key metrics
    metrics_to_show <- c("sensitivity", "specificity", "fnr", "accuracy", 
                         "balanced_accuracy", "mean_items_used", "reduction_pct")
    
    for (metric in metrics_to_show) {
      if (!is.null(performance_comparison$absolute_differences[[metric]])) {
        baseline_val <- performance_comparison$baseline_performance[[metric]]
        deploy_val <- performance_comparison$deployment_performance[[metric]]
        abs_diff <- performance_comparison$absolute_differences[[metric]]
        
        # Format based on metric type
        if (metric == "mean_items_used") {
          summary_text <- paste0(summary_text,
                                 sprintf("%-20s  %10.1f  %10.1f  %+10.1f\n",
                                         metric, baseline_val, deploy_val, abs_diff))
        } else if (metric == "reduction_pct") {
          summary_text <- paste0(summary_text,
                                 sprintf("%-20s  %9.1f%%  %9.1f%%  %+9.1f%%\n",
                                         metric, baseline_val, deploy_val, abs_diff))
        } else {
          summary_text <- paste0(summary_text,
                                 sprintf("%-20s  %10.3f  %10.3f  %+10.3f\n",
                                         metric, baseline_val, deploy_val, abs_diff))
        }
      }
    }
  }
  
  # Improvements section
  if (length(performance_comparison$fidelity_assessment$improvements) > 0) {
    summary_text <- paste0(summary_text,
                           "\nPERFORMANCE IMPROVEMENTS\n",
                           "------------------------\n")
    for (improvement in performance_comparison$fidelity_assessment$improvements) {
      summary_text <- paste0(summary_text, "✓ ", improvement, "\n")
    }
  }
  
  # Critical issues (only degradations)
  if (length(performance_comparison$fidelity_assessment$critical_issues) > 0) {
    summary_text <- paste0(summary_text,
                           "\nCRITICAL ISSUES (Degradations)\n",
                           "-------------------------------\n")
    for (issue in performance_comparison$fidelity_assessment$critical_issues) {
      summary_text <- paste0(summary_text, "✗ ", issue, "\n")
    }
  }
  
  # Warnings
  if (length(performance_comparison$fidelity_assessment$warnings) > 0) {
    summary_text <- paste0(summary_text,
                           "\nWARNINGS\n",
                           "--------\n")
    for (warning in performance_comparison$fidelity_assessment$warnings) {
      summary_text <- paste0(summary_text, "⚠ ", warning, "\n")
    }
  }
  
  writeLines(summary_text, summary_file)
}

#' Generate Detailed Comparison Report (FIXED - Uses Correct Terminology)
#'
#' @param performance_comparison Performance comparison
#' @param output_dir Output directory
generate_detailed_comparison_report <- function(performance_comparison, output_dir) {
  
  if (!performance_comparison$comparison_available) {
    return(invisible(NULL))
  }
  
  report_file <- file.path(output_dir, "detailed_performance_comparison.html")
  
  # Use appropriate terminology
  baseline_label <- ifelse(performance_comparison$comparison_type == "optimized", 
                           "Optimized", "Original")
  
  html_content <- paste0(
    "<html><head><title>Performance Comparison Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1, h2 { color: #333; }",
    "table { border-collapse: collapse; width: 80%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".metric-value { text-align: right; }",
    ".improvement { color: green; }",
    ".degradation { color: red; }",
    ".neutral { color: black; }",
    "</style></head><body>",
    "<h1>Detailed Performance Comparison</h1>",
    "<p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
    "<p><strong>Comparison Type:</strong> ", baseline_label, " vs Deployed</p>"
  )
  
  # Fidelity status
  fidelity_status <- performance_comparison$fidelity_assessment$overall_fidelity
  status_color <- ifelse(fidelity_status == "PASS", "green", "red")
  html_content <- paste0(html_content,
                         "<h2>Overall Fidelity: <span style='color:", status_color, "'>",
                         fidelity_status, "</span></h2>")
  
  # Add improvements section if any
  if (length(performance_comparison$fidelity_assessment$improvements) > 0) {
    html_content <- paste0(html_content,
                           "<h2>Performance Improvements</h2>",
                           "<ul style='color: green;'>")
    for (improvement in performance_comparison$fidelity_assessment$improvements) {
      html_content <- paste0(html_content, "<li>", improvement, "</li>")
    }
    html_content <- paste0(html_content, "</ul>")
  }
  
  # Performance metrics table
  html_content <- paste0(html_content,
                         "<h2>Performance Metrics</h2>",
                         "<table>",
                         "<tr><th>Metric</th><th class='metric-value'>", baseline_label, "</th>",
                         "<th class='metric-value'>Deployed</th>",
                         "<th class='metric-value'>Abs. Diff</th>",
                         "<th class='metric-value'>% Change</th>",
                         "<th>Assessment</th></tr>")
  
  metrics_info <- list(
    sensitivity = list(name = "Sensitivity", higher_better = TRUE),
    specificity = list(name = "Specificity", higher_better = TRUE),
    fnr = list(name = "False Negative Rate", higher_better = FALSE),
    accuracy = list(name = "Accuracy", higher_better = TRUE),
    balanced_accuracy = list(name = "Balanced Accuracy", higher_better = TRUE),
    mean_items_used = list(name = "Mean Items Used", higher_better = FALSE),
    reduction_pct = list(name = "Reduction %", higher_better = TRUE)
  )
  
  for (metric in names(metrics_info)) {
    info <- metrics_info[[metric]]
    baseline <- performance_comparison$baseline_performance[[metric]]
    deploy <- performance_comparison$deployment_performance[[metric]]
    abs_diff <- performance_comparison$absolute_differences[[metric]]
    pct_diff <- performance_comparison$percentage_differences[[metric]]
    
    if (!is.null(baseline) && !is.null(deploy)) {
      # Determine if change is good, bad, or neutral
      if (abs(abs_diff) < 0.001) {
        change_class <- "neutral"
        assessment <- "No change"
      } else if ((info$higher_better && abs_diff > 0) || (!info$higher_better && abs_diff < 0)) {
        change_class <- "improvement"
        assessment <- "Improved"
      } else {
        change_class <- "degradation"
        assessment <- "Degraded"
      }
      
      pct_text <- ifelse(is.na(pct_diff), "N/A", sprintf("%+.1f%%", pct_diff))
      
      html_content <- paste0(html_content,
                             "<tr>",
                             "<td>", info$name, "</td>",
                             "<td class='metric-value'>", sprintf("%.3f", baseline), "</td>",
                             "<td class='metric-value'>", sprintf("%.3f", deploy), "</td>",
                             "<td class='metric-value ", change_class, "'>", 
                             sprintf("%+.3f", abs_diff), "</td>",
                             "<td class='metric-value ", change_class, "'>", pct_text, "</td>",
                             "<td>", assessment, "</td>",
                             "</tr>")
    }
  }
  
  html_content <- paste0(html_content, "</table>")
  
  # Add critical issues if any
  if (length(performance_comparison$fidelity_assessment$critical_issues) > 0) {
    html_content <- paste0(html_content,
                           "<h2>Critical Issues (Degradations)</h2>",
                           "<ul style='color: red;'>")
    for (issue in performance_comparison$fidelity_assessment$critical_issues) {
      html_content <- paste0(html_content, "<li>", issue, "</li>")
    }
    html_content <- paste0(html_content, "</ul>")
  }
  
  # Add warnings if any
  if (length(performance_comparison$fidelity_assessment$warnings) > 0) {
    html_content <- paste0(html_content,
                           "<h2>Warnings</h2>",
                           "<ul style='color: orange;'>")
    for (warning in performance_comparison$fidelity_assessment$warnings) {
      html_content <- paste0(html_content, "<li>", warning, "</li>")
    }
    html_content <- paste0(html_content, "</ul>")
  }
  
  html_content <- paste0(html_content, "</body></html>")
  
  writeLines(html_content, report_file)
}

#' Generate Boundary Utilization Report (FIXED - Clear Terminology)
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
    "-----------------------------\n",
    "Clear categorization of early stopping patterns:\n",
    "- all_constructs_stopped_early: ALL constructs reached stopping boundaries\n",
    "- some_constructs_stopped_early: SOME (but not all) constructs reached stopping boundaries\n",
    "- no_constructs_stopped_early: NO constructs stopped early (all items administered)\n\n"
  )
  
  # Add stop reason statistics with clear formatting
  for (reason in names(boundary_analysis$utilization_summary$stop_reason_counts)) {
    count <- boundary_analysis$utilization_summary$stop_reason_counts[[reason]]
    pct <- boundary_analysis$utilization_summary$stop_reason_percentages[[reason]]
    
    report_text <- paste0(report_text,
                          sprintf("%-35s: %4d (%5.1f%%)\n", reason, count, pct))
  }
  
  # Boundary coverage by construct
  report_text <- paste0(report_text,
                        "\nBOUNDARY COVERAGE BY CONSTRUCT:\n",
                        "--------------------------------\n")
  
  for (cn in names(boundary_analysis$boundary_coverage)) {
    cov <- boundary_analysis$boundary_coverage[[cn]]
    report_text <- paste0(report_text,
                          cn, ":\n",
                          "  Low boundaries:  ", sprintf("%.1f%%", cov$low_boundary_coverage * 100), "\n",
                          "  High boundaries: ", sprintf("%.1f%%", cov$high_boundary_coverage * 100), "\n")
  }
  
  # Efficiency by stopping reason
  report_text <- paste0(report_text,
                        "\nEFFICIENCY BY STOPPING REASON:\n",
                        "-------------------------------\n")
  
  for (reason in names(boundary_analysis$efficiency_by_reason)) {
    eff <- boundary_analysis$efficiency_by_reason[[reason]]
    
    report_text <- paste0(report_text,
                          reason, ":\n",
                          "  Mean items:   ", sprintf("%.1f", eff$mean_items), "\n",
                          "  Median items: ", sprintf("%.1f", eff$median_items), "\n",
                          "  SD items:     ", sprintf("%.1f", eff$sd_items), "\n")
  }
  
  # Add interpretation note
  report_text <- paste0(report_text,
                        "\nINTERPRETATION:\n",
                        "--------------\n",
                        "The mean items used shows the average assessment length for each category.\n",
                        "Lower mean items indicates more efficient curtailment.\n",
                        "- 'all_constructs_stopped_early' should have the lowest mean items\n",
                        "- 'some_constructs_stopped_early' should have intermediate mean items\n",
                        "- 'no_constructs_stopped_early' should have the highest mean items (all items)\n")
  
  writeLines(report_text, report_file)
}

#' Generate Continuation Logic Report
#'
#' @param continuation_validation Logic validation results
#' @param output_dir Output directory
generate_continuation_logic_report <- function(continuation_validation, output_dir) {
  
  report_file <- file.path(output_dir, "continuation_logic_report.txt")
  
  report_text <- paste0(
    "CONTINUATION LOGIC VALIDATION REPORT\n",
    "===================================\n\n",
    "Overall Status: ", 
    ifelse(continuation_validation$logic_validation_passed, "PASSED", "FAILED"), "\n\n"
  )
  
  # Logic checks
  report_text <- paste0(report_text,
                        "LOGIC CHECKS:\n",
                        "-------------\n")
  
  for (check_name in names(continuation_validation$logic_checks)) {
    check_result <- continuation_validation$logic_checks[[check_name]]
    status <- ifelse(check_result, "✓", "✗")
    report_text <- paste0(report_text,
                          sprintf("%-30s: %s\n", check_name, status))
  }
  
  # Stop reason distribution
  report_text <- paste0(report_text,
                        "\nSTOP REASON DISTRIBUTION:\n",
                        "------------------------\n")
  
  for (reason in names(continuation_validation$stop_reason_distribution)) {
    count <- continuation_validation$stop_reason_distribution[[reason]]
    report_text <- paste0(report_text,
                          sprintf("%-25s: %d\n", reason, count))
  }
  
  # Detailed check results
  if (length(continuation_validation$detailed_checks) > 0) {
    report_text <- paste0(report_text,
                          "\nDETAILED SAMPLE CHECKS:\n",
                          "-----------------------\n")
    
    all_passed <- TRUE
    for (resp_id in names(continuation_validation$detailed_checks)) {
      checks <- continuation_validation$detailed_checks[[resp_id]]
      if (!all(unlist(checks))) {
        all_passed <- FALSE
        report_text <- paste0(report_text,
                              resp_id, ": FAILED\n")
        for (check in names(checks)) {
          if (!checks[[check]]) {
            report_text <- paste0(report_text,
                                  "  - ", check, ": mismatch\n")
          }
        }
      }
    }
    
    if (all_passed) {
      report_text <- paste0(report_text,
                            "All sampled respondents passed consistency checks.\n")
    }
  }
  
  writeLines(report_text, report_file)
}

#' Generate Deployment Recommendations (FIXED - Recognizes Improvements)
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
    "==========================\n\n",
    "Based on validation results:\n\n"
  )
  
  # Determine overall recommendation
  performance_ok <- !performance_comparison$comparison_available || 
    performance_comparison$fidelity_assessment$overall_fidelity == "PASS"
  logic_ok <- continuation_validation$logic_validation_passed
  
  # Check if we have improvements
  has_improvements <- length(performance_comparison$fidelity_assessment$improvements) > 0
  has_critical_issues <- length(performance_comparison$fidelity_assessment$critical_issues) > 0
  
  if (performance_ok && logic_ok) {
    recommendations_text <- paste0(recommendations_text,
                                   "✅ DEPLOYMENT RECOMMENDED\n\n")
    
    if (has_improvements) {
      recommendations_text <- paste0(recommendations_text,
                                     "Performance improvements detected:\n")
      for (improvement in performance_comparison$fidelity_assessment$improvements) {
        recommendations_text <- paste0(recommendations_text, "  ✓ ", improvement, "\n")
      }
      recommendations_text <- paste0(recommendations_text, "\n")
    }
    
    recommendations_text <- paste0(recommendations_text,
                                   "The deployment package has passed all validation checks.\n",
                                   "Performance metrics are within acceptable tolerances.\n\n",
                                   "Next steps:\n",
                                   "1. Deploy in pilot environment\n",
                                   "2. Monitor real-world performance\n",
                                   "3. Collect feedback from users\n",
                                   "4. Schedule periodic performance reviews\n")
  } else {
    # Check why it failed
    failure_reasons <- character()
    
    if (has_critical_issues) {
      failure_reasons <- c(failure_reasons, "Performance degradations exceed acceptable thresholds")
    }
    
    if (!logic_ok) {
      failure_reasons <- c(failure_reasons, "Continuation logic validation failed")
    }
    
    recommendations_text <- paste0(recommendations_text,
                                   "❌ DEPLOYMENT NOT RECOMMENDED\n\n",
                                   "Reasons:\n")
    
    for (reason in failure_reasons) {
      recommendations_text <- paste0(recommendations_text, "  • ", reason, "\n")
    }
    
    recommendations_text <- paste0(recommendations_text,
                                   "\nCritical issues must be resolved before deployment.\n",
                                   "See detailed reports for specific remediation steps.\n")
    
    # If there were also improvements, mention them
    if (has_improvements) {
      recommendations_text <- paste0(recommendations_text,
                                     "\nNote: Some performance improvements were also detected:\n")
      for (improvement in performance_comparison$fidelity_assessment$improvements) {
        recommendations_text <- paste0(recommendations_text, "  ✓ ", improvement, "\n")
      }
    }
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
  
  # 4. Boundary utilization heatmap (if multi-construct)
  if (length(boundary_analysis$boundary_coverage) > 1) {
    boundary_plot <- create_boundary_coverage_plot(boundary_analysis)
    ggplot2::ggsave(file.path(viz_dir, "boundary_coverage.png"),
                    boundary_plot, width = 12, height = 8)
  }
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
# DEPRECATED: Old Pattern-Based Simulation Functions
# ============================================================================
# These are no longer used but kept for reference

# simulate_unidimensional_respondent_patterns <- function(...) { }
# simulate_multicontruct_respondent_patterns <- function(...) { }
# simulate_unidimensional_respondent <- function(...) { }
# simulate_multicontruct_respondent <- function(...) { }

# ============================================================================
# Helper Functions
# ============================================================================

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
