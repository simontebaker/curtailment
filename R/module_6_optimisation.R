# ============================================================================
# Module 6: Optimization Module - Construct-Specific Gamma Optimization
# ============================================================================
# Purpose: Optimize confidence parameters (gamma values) for each construct
#          individually to maximize efficiency while respecting FNR constraints
# ============================================================================

#' Optimize Method Parameters Based on Base Prevalence
#'
#' @param method_id Method ID to optimize (NULL = use recommended from evaluation)
#' @param evaluation_results Results from Module 5 evaluation
#' @param all_combination_results All method results from Module 4
#' @param prepared_data Prepared data from Module 1
#' @param config Analysis configuration from Module 4
#' @param code_dir Directory containing module scripts
#' @param optimization_config List with optimization parameters
#' @param output_dir Directory for optimization outputs
#' @return List containing optimization results in deployment-ready format
#' @export
optimize_method <- function(
    method_id = NULL,
    evaluation_results,
    all_combination_results,
    prepared_data,
    config,
    code_dir = NULL,
    optimization_config = list(),
    output_dir = "optimization_results"
) {
  
  if (is.null(code_dir)) {
    if (exists("code_dir", envir = .GlobalEnv)) {
      code_dir <- get("code_dir", envir = .GlobalEnv)
    } else {
      code_dir <- getwd()
    }
  }
  
  # Load required modules
  cat("\nLoading required module functions...\n")
  source(file.path(code_dir, "module_4_item_reduction.R"))
  source(file.path(code_dir, "module_5_evaluation.R"))
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("========================================\n")
  cat("Starting Construct-Specific Optimization\n")
  cat("========================================\n\n")
  
  # Set default optimization config with new parameters
  opt_config <- list(
    base_rate_threshold = 0.20,
    fnr_threshold_high_prevalence = 0.10,
    fnr_threshold_low_prevalence = 0.10,
    gamma_0_high_prevalence = 0.80,
    gamma_1_high_prevalence = 0.80,
    gamma_0_low_prevalence = 0.80,
    gamma_1_low_prevalence = 0.80,
    gamma_search_step = 0.01,
    use_parallel = TRUE,
    n_cores = parallel::detectCores() - 1
  )
  
  # Override with user config
  for (param in names(optimization_config)) {
    opt_config[[param]] <- optimization_config[[param]]
  }
  
  # Select method to optimize
  if (is.null(method_id)) {
    method_id <- evaluation_results$recommended_method$method_id
    cat("Using recommended method:", method_id, "\n")
  } else {
    cat("Optimizing specified method:", method_id, "\n")
  }
  
  # Get original method
  original_method <- all_combination_results[[method_id]]
  if (is.null(original_method) || "error" %in% names(original_method)) {
    stop(paste("Method", method_id, "not found or had errors"))
  }
  
  # Check if method supports gamma optimization
  reduction_method <- original_method$combination$reduction
  if (!reduction_method %in% c("sc_ep", "sc_sor", "sc_mor")) {
    stop(paste("Method", reduction_method, "does not support gamma optimization"))
  }
  
  # Get original performance
  cat("\nCalculating original method performance...\n")
  original_performance <- calculate_reduction_performance_full(
    original_method$reduction_result,
    prepared_data
  )
  
  # Analyze base rates and categorize constructs
  cat("\nAnalyzing base rates and categorizing constructs...\n")
  construct_categories <- categorize_constructs_by_base_rate(
    prepared_data, 
    opt_config$base_rate_threshold
  )
  
  print_construct_categories(construct_categories)
  
  # Optimize gamma values for each construct
  cat("\nOptimizing gamma values for each construct...\n")
  optimization_analysis <- optimize_construct_specific_gammas(
    original_method,
    prepared_data,
    config,
    construct_categories,
    opt_config,
    output_dir
  )
  
  # Generate comprehensive reports
  cat("\nGenerating optimization reports...\n")
  generate_optimization_reports(
    original_performance,
    optimization_analysis,
    construct_categories,
    opt_config,
    output_dir
  )
  
  # Create deployment-ready optimized method result
  # This mimics the structure from all_combination_results but with optimized parameters
  optimized_method_result <- list(
    combination = as.list(original_method$combination),  # Convert to list to avoid data frame issues
    ordering_result = original_method$ordering_result,
    reduction_result = optimization_analysis$optimized_reduction_result
  )
  
  # Update combination info to reflect optimization
  optimized_method_result$combination$optimized <- TRUE
  optimized_method_result$combination$optimization_timestamp <- optimization_analysis$timestamp
  
  # For multi-construct, store construct-specific gammas
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    optimized_method_result$combination$construct_gammas <- optimization_analysis$optimized_gammas
    # Mark global gammas as construct-specific
    optimized_method_result$combination$gamma_0 <- "construct_specific"
    optimized_method_result$combination$gamma_1 <- "construct_specific"
  } else {
    # For unidimensional, update the global gammas
    optimized_method_result$combination$gamma_0 <- optimization_analysis$optimized_gammas[["total"]]$gamma_0
    optimized_method_result$combination$gamma_1 <- optimization_analysis$optimized_gammas[["total"]]$gamma_1
  }
  
  # Package results in deployment-ready format
  optimization_results <- list(
    optimized_method = optimized_method_result,
    original_method_id = method_id,
    optimization_analysis = optimization_analysis,
    original_performance = original_performance,
    optimized_performance = optimization_analysis$optimized_performance,
    construct_categories = construct_categories,
    optimization_config = opt_config,
    timestamp = Sys.time()
  )
  
  # Save complete results
  saveRDS(optimization_results, file.path(output_dir, "optimization_results.rds"))
  
  cat("\n✅ Optimization complete! Results saved to:", output_dir, "\n")
  
  return(optimization_results)
}

#' Categorize Constructs by Base Rate
#'
#' @param prepared_data Prepared data
#' @param threshold Base rate threshold
#' @return List with construct categories
categorize_constructs_by_base_rate <- function(prepared_data, threshold) {
  
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    base_rate <- prepared_data$config$reference_base_rates$total
    return(list(
      type = "unidimensional",
      threshold = threshold,
      constructs = list(
        total = list(
          base_rate = base_rate,
          category = ifelse(base_rate > threshold, "high", "low")
        )
      )
    ))
  }
  
  # Multi-construct
  base_rates <- prepared_data$config$reference_base_rates
  
  # Categorize each construct
  constructs <- list()
  for (cn in names(base_rates)) {
    constructs[[cn]] <- list(
      base_rate = base_rates[[cn]],
      category = ifelse(base_rates[[cn]] > threshold, "high", "low")
    )
  }
  
  # Summary lists
  high_prevalence <- names(constructs)[sapply(constructs, function(x) x$category == "high")]
  low_prevalence <- names(constructs)[sapply(constructs, function(x) x$category == "low")]
  
  return(list(
    type = "multi-construct",
    threshold = threshold,
    constructs = constructs,
    high_prevalence = high_prevalence,
    low_prevalence = low_prevalence,
    n_high = length(high_prevalence),
    n_low = length(low_prevalence)
  ))
}

#' Print Construct Categories
#'
#' @param categories Construct categories
print_construct_categories <- function(categories) {
  cat("\nBase Rate Analysis:\n")
  cat("Threshold:", sprintf("%.1f%%", categories$threshold * 100), "\n")
  
  if (categories$type == "unidimensional") {
    construct_info <- categories$constructs$total
    cat(sprintf("\nSingle construct: %.1f%% (%s prevalence)\n", 
                construct_info$base_rate * 100, 
                construct_info$category))
  } else {
    cat("\nHigh prevalence constructs (>", sprintf("%.1f%%", categories$threshold * 100), "):\n")
    if (categories$n_high > 0) {
      for (cn in categories$high_prevalence) {
        cat("  -", cn, sprintf("(%.1f%%)", categories$constructs[[cn]]$base_rate * 100), "\n")
      }
    } else {
      cat("  (none)\n")
    }
    
    cat("\nLow prevalence constructs (≤", sprintf("%.1f%%", categories$threshold * 100), "):\n")
    if (categories$n_low > 0) {
      for (cn in categories$low_prevalence) {
        cat("  -", cn, sprintf("(%.1f%%)", categories$constructs[[cn]]$base_rate * 100), "\n")
      }
    } else {
      cat("  (none)\n")
    }
  }
}

#' Optimize Construct-Specific Gammas
#'
#' @param original_method Original method
#' @param prepared_data Prepared data
#' @param config Configuration
#' @param construct_categories Construct categories
#' @param opt_config Optimization config
#' @param output_dir Output directory
#' @return Optimization results
optimize_construct_specific_gammas <- function(original_method, prepared_data, config,
                                               construct_categories, opt_config, 
                                               output_dir) {
  
  # Initialize results storage
  construct_results <- list()
  optimized_gammas <- list()
  
  # Get construct names
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    construct_names <- "total"
  } else {
    construct_names <- names(prepared_data$config$constructs)
  }
  
  # Set up parallel processing if requested
  if (opt_config$use_parallel && length(construct_names) > 1) {
    cl <- parallel::makeCluster(opt_config$n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export required objects and functions to workers
    parallel::clusterExport(cl, c("optimize_single_construct_gammas",
                                  "test_construct_gamma_combination",
                                  "find_lowest_gamma_values",
                                  "reduce_items", 
                                  "calculate_reduction_performance_full",
                                  "apply_construct_specific_reduction",
                                  "reduce_sc_ep", "reduce_sc_sor", "reduce_sc_mor",
                                  "reduce_sc_ep_integrated", "reduce_sc_sor_integrated", 
                                  "reduce_sc_mor_integrated",
                                  "calculate_reduction_performance",
                                  "check_construct_constraints",
                                  "build_empirical_tables", "build_ordinal_models",
                                  "predict_ordinal_probs", "predict_ordinal_probs_multiple",
                                  "calculate_classifications",
                                  "%||%"),
                            envir = .GlobalEnv)
    
    # Also export the data objects
    parallel::clusterExport(cl, c("original_method", "prepared_data", "config",
                                  "construct_categories", "opt_config"),
                            envir = environment())
    
    # Optimize each construct in parallel
    construct_results <- parallel::parLapply(cl, construct_names, function(cn) {
      optimize_single_construct_gammas(
        construct_name = cn,
        original_method = original_method,
        prepared_data = prepared_data,
        config = config,
        construct_categories = construct_categories,
        opt_config = opt_config
      )
    })
    names(construct_results) <- construct_names
    
  } else {
    # Sequential processing
    pb <- txtProgressBar(min = 0, max = length(construct_names), style = 3)
    
    for (i in seq_along(construct_names)) {
      cn <- construct_names[i]
      
      construct_results[[cn]] <- optimize_single_construct_gammas(
        construct_name = cn,
        original_method = original_method,
        prepared_data = prepared_data,
        config = config,
        construct_categories = construct_categories,
        opt_config = opt_config
      )
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  
  # Extract optimized gamma values
  for (cn in names(construct_results)) {
    if (construct_results[[cn]]$success) {
      optimized_gammas[[cn]] <- construct_results[[cn]]$best_gammas
    }
  }
  
  # Test the complete optimized configuration
  cat("\nTesting complete optimized configuration...\n")
  optimized_result <- test_complete_optimized_method(
    original_method,
    prepared_data,
    config,
    optimized_gammas
  )
  
  return(list(
    original_method = original_method,
    construct_categories = construct_categories,
    construct_results = construct_results,
    optimized_gammas = optimized_gammas,
    optimized_performance = optimized_result$performance,
    optimized_reduction_result = optimized_result$reduction_result,
    optimization_config = opt_config,
    timestamp = Sys.time()
  ))
}

#' Optimize Gamma Values for a Single Construct
#'
#' @param construct_name Name of construct to optimize
#' @param original_method Original method
#' @param prepared_data Prepared data
#' @param config Configuration
#' @param construct_categories Construct categories
#' @param opt_config Optimization config
#' @return Optimization results for this construct
optimize_single_construct_gammas <- function(construct_name, original_method, 
                                             prepared_data, config,
                                             construct_categories, opt_config) {
  
  # Determine prevalence category and constraints
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    category <- construct_categories$constructs$total$category
    base_rate <- construct_categories$constructs$total$base_rate
  } else {
    category <- construct_categories$constructs[[construct_name]]$category
    base_rate <- construct_categories$constructs[[construct_name]]$base_rate
  }
  
  # Set constraints based on prevalence category
  if (category == "high") {
    fnr_threshold <- opt_config$fnr_threshold_high_prevalence
    gamma_0_min <- opt_config$gamma_0_high_prevalence
    gamma_1_min <- opt_config$gamma_1_high_prevalence
  } else {
    fnr_threshold <- opt_config$fnr_threshold_low_prevalence
    gamma_0_min <- opt_config$gamma_0_low_prevalence
    gamma_1_min <- opt_config$gamma_1_low_prevalence
  }
  
  # Find lowest gamma values that meet FNR constraint
  best_gammas <- find_lowest_gamma_values(
    construct_name = construct_name,
    original_method = original_method,
    prepared_data = prepared_data,
    config = config,
    fnr_threshold = fnr_threshold,
    gamma_0_min = gamma_0_min,
    gamma_1_min = gamma_1_min,
    gamma_search_step = opt_config$gamma_search_step
  )
  
  # Check if optimization succeeded
  if (!best_gammas$success) {
    # Computational failure - still include in results
    return(list(
      construct_name = construct_name,
      base_rate = base_rate,
      prevalence_category = category,
      fnr_threshold = fnr_threshold,
      gamma_constraints = list(
        gamma_0_min = gamma_0_min,
        gamma_1_min = gamma_1_min
      ),
      best_gammas = list(gamma_0 = 1.0, gamma_1 = 1.0),
      best_performance = best_gammas$performance,
      search_iterations = best_gammas$iterations,
      success = TRUE,  # Include in results
      optimization_failed = TRUE,
      failure_reason = "Could not compute performance metrics"
    ))
  }
  
  # Check if the result meets the threshold
  if (!best_gammas$meets_threshold) {
    # FNR exceeds threshold even at gamma = 1.0
    return(list(
      construct_name = construct_name,
      base_rate = base_rate,
      prevalence_category = category,
      fnr_threshold = fnr_threshold,
      gamma_constraints = list(
        gamma_0_min = gamma_0_min,
        gamma_1_min = gamma_1_min
      ),
      best_gammas = best_gammas$gammas,
      best_performance = best_gammas$performance,
      search_iterations = best_gammas$iterations,
      success = TRUE,  # Include in results
      optimization_failed = TRUE,
      failure_reason = paste0("FNR (", sprintf("%.1f%%", best_gammas$performance$fnr * 100), 
                              ") exceeds threshold (", sprintf("%.1f%%", fnr_threshold * 100), "%)")
    ))
  }
  
  # Normal successful case
  return(list(
    construct_name = construct_name,
    base_rate = base_rate,
    prevalence_category = category,
    fnr_threshold = fnr_threshold,
    gamma_constraints = list(
      gamma_0_min = gamma_0_min,
      gamma_1_min = gamma_1_min
    ),
    best_gammas = best_gammas$gammas,
    best_performance = best_gammas$performance,
    search_iterations = best_gammas$iterations,
    success = best_gammas$success,
    optimization_failed = FALSE
  ))
}

#' Find Lowest Gamma Values Meeting Constraints
#'
#' @param construct_name Name of construct
#' @param original_method Original method
#' @param prepared_data Prepared data
#' @param config Configuration
#' @param fnr_threshold Maximum allowable FNR
#' @param gamma_0_min Minimum gamma_0 value
#' @param gamma_1_min Minimum gamma_1 value
#' @param gamma_search_step Step size for search
#' @return List with best gamma values and performance
find_lowest_gamma_values <- function(construct_name, original_method, prepared_data,
                                     config, fnr_threshold, gamma_0_min, gamma_1_min,
                                     gamma_search_step = 0.01) {
  
  # Initialize search
  best_gammas <- list(gamma_0 = 1.0, gamma_1 = 1.0)
  best_performance <- NULL
  iterations <- 0
  
  # FIRST: Test gamma_0 = 1.0, gamma_1 = 1.0 to establish baseline
  max_test <- test_construct_gamma_combination(
    construct_name = construct_name,
    gamma_0 = 1.0,
    gamma_1 = 1.0,
    original_method = original_method,
    prepared_data = prepared_data,
    config = config
  )
  iterations <- iterations + 1
  
  # If we can't even compute performance at gamma = 1.0, that's a real failure
  if (!max_test$success) {
    return(list(
      gammas = list(gamma_0 = 1.0, gamma_1 = 1.0),
      performance = max_test,
      iterations = iterations,
      success = FALSE,  # Computational failure
      meets_threshold = FALSE
    ))
  }
  
  # Set initial best values
  best_gammas <- list(gamma_0 = 1.0, gamma_1 = 1.0)
  best_performance <- max_test
  meets_threshold <- (max_test$fnr <= fnr_threshold)
  
  # Only search for lower gammas if the max values meet the threshold
  if (meets_threshold) {
    # Binary search for gamma_0 (keeping gamma_1 = 1.0 initially)
    gamma_0_low <- gamma_0_min
    gamma_0_high <- 1.0
    
    while ((gamma_0_high - gamma_0_low) > gamma_search_step) {
      gamma_0_mid <- (gamma_0_low + gamma_0_high) / 2
      iterations <- iterations + 1
      
      test_result <- test_construct_gamma_combination(
        construct_name = construct_name,
        gamma_0 = gamma_0_mid,
        gamma_1 = 1.0,
        original_method = original_method,
        prepared_data = prepared_data,
        config = config
      )
      
      if (test_result$success && test_result$fnr <= fnr_threshold) {
        # This gamma_0 works, try lower
        gamma_0_high <- gamma_0_mid
        best_gammas$gamma_0 <- gamma_0_mid
        best_performance <- test_result
      } else {
        # This gamma_0 is too low, need higher
        gamma_0_low <- gamma_0_mid
      }
    }
    
    # Now optimize gamma_1 with the found gamma_0
    if (!config$constraints$stop_low_only && !is.null(best_performance)) {
      gamma_1_low <- gamma_1_min
      gamma_1_high <- 1.0
      
      while ((gamma_1_high - gamma_1_low) > gamma_search_step) {
        gamma_1_mid <- (gamma_1_low + gamma_1_high) / 2
        iterations <- iterations + 1
        
        test_result <- test_construct_gamma_combination(
          construct_name = construct_name,
          gamma_0 = best_gammas$gamma_0,
          gamma_1 = gamma_1_mid,
          original_method = original_method,
          prepared_data = prepared_data,
          config = config
        )
        
        if (test_result$success && test_result$fnr <= fnr_threshold) {
          # This gamma_1 works, try lower
          gamma_1_high <- gamma_1_mid
          best_gammas$gamma_1 <- gamma_1_mid
          best_performance <- test_result
        } else {
          # This gamma_1 is too low, need higher
          gamma_1_low <- gamma_1_mid
        }
      }
    }
  }
  
  return(list(
    gammas = best_gammas,
    performance = best_performance,
    iterations = iterations,
    success = TRUE,  # We successfully found the best we could
    meets_threshold = meets_threshold  # Whether it meets the FNR threshold
  ))
}

#' Test Gamma Combination for a Single Construct
#'
#' @param construct_name Name of construct
#' @param gamma_0 Gamma_0 value to test
#' @param gamma_1 Gamma_1 value to test
#' @param original_method Original method
#' @param prepared_data Prepared data
#' @param config Configuration
#' @return Test results including FNR
test_construct_gamma_combination <- function(construct_name, gamma_0, gamma_1,
                                             original_method, prepared_data, config) {
  
  # Extract required information
  ordered_items <- original_method$ordering_result$ordered_items
  reduction_method <- original_method$combination$reduction
  training_params <- original_method$reduction_result$training_params
  
  # Create method params for this construct
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    method_params <- list(gamma_0 = gamma_0, gamma_1 = gamma_1)
  } else {
    # For multi-construct, use original gammas for other constructs
    method_params <- list()
    for (cn in names(prepared_data$config$constructs)) {
      if (cn == construct_name) {
        method_params[[cn]] <- list(gamma_0 = gamma_0, gamma_1 = gamma_1)
      } else {
        # Use original gamma values - handle the as.list conversion
        original_combination <- as.list(original_method$combination)
        method_params[[cn]] <- list(
          gamma_0 = original_combination$gamma_0,
          gamma_1 = original_combination$gamma_1
        )
      }
    }
  }
  
  # Apply reduction
  tryCatch({
    if (prepared_data$config$questionnaire_type == "unidimensional") {
      # Standard reduction for unidimensional
      test_result <- reduce_items(
        ordered_items = ordered_items,
        data = prepared_data$splits$test,
        config = config,
        method = reduction_method,
        method_params = method_params,
        training_params = training_params
      )
    } else {
      # Use construct-specific reduction
      test_result <- apply_construct_specific_reduction(
        ordered_items,
        prepared_data$splits$test,
        config,
        reduction_method,
        method_params,
        training_params,
        original_method
      )
    }
    
    # Calculate performance
    performance <- calculate_reduction_performance_full(test_result, prepared_data)
    
    # Extract construct-specific FNR
    if (prepared_data$config$questionnaire_type == "unidimensional") {
      construct_fnr <- performance$fnr
    } else {
      construct_fnr <- performance$construct_metrics[[construct_name]]$fnr
    }
    
    return(list(
      success = TRUE,
      fnr = construct_fnr,
      performance = performance,
      reduction_result = test_result
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = e$message,
      fnr = NA
    ))
  })
}

#' Test Complete Optimized Method
#'
#' @param original_method Original method
#' @param prepared_data Prepared data
#' @param config Configuration
#' @param optimized_gammas Optimized gamma values for all constructs
#' @return Test results
test_complete_optimized_method <- function(original_method, prepared_data, config,
                                           optimized_gammas) {
  
  ordered_items <- original_method$ordering_result$ordered_items
  reduction_method <- original_method$combination$reduction
  training_params <- original_method$reduction_result$training_params
  
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    # For unidimensional, use the single set of gammas
    method_params <- optimized_gammas[["total"]]
    
    reduction_result <- reduce_items(
      ordered_items = ordered_items,
      data = prepared_data$splits$test,
      config = config,
      method = reduction_method,
      method_params = method_params,
      training_params = training_params
    )
  } else {
    # For multi-construct, use construct-specific gammas
    reduction_result <- apply_construct_specific_reduction(
      ordered_items,
      prepared_data$splits$test,
      config,
      reduction_method,
      optimized_gammas,
      training_params,
      original_method
    )
  }
  
  # Calculate performance
  performance <- calculate_reduction_performance_full(reduction_result, prepared_data)
  
  return(list(
    reduction_result = reduction_result,
    performance = performance
  ))
}

#' Generate Optimization Reports
#'
#' @param original_performance Original performance
#' @param optimization_results Optimization results
#' @param construct_categories Construct categories
#' @param opt_config Optimization config
#' @param output_dir Output directory
generate_optimization_reports <- function(original_performance, optimization_results,
                                          construct_categories, opt_config, output_dir) {
  
  # 1. Generate optimization summary
  generate_optimization_summary(optimization_results, opt_config, output_dir)
  
  # 2. Generate detailed results table (CSV and HTML)
  generate_optimization_results_table(optimization_results, output_dir)
  
  # 3. Generate comparison report (TXT and HTML)
  generate_optimization_comparison_report(
    original_performance,
    optimization_results,
    output_dir
  )
}

#' Generate Optimization Summary
#'
#' @param optimization_results Optimization results
#' @param opt_config Optimization config
#' @param output_dir Output directory
generate_optimization_summary <- function(optimization_results, opt_config, output_dir) {
  
  summary_file <- file.path(output_dir, "optimization_summary.txt")
  
  # Get method combination as list to handle both data.frame and list cases
  combination_info <- as.list(optimization_results$original_method$combination)
  
  summary_text <- paste0(
    "CONSTRUCT-SPECIFIC GAMMA OPTIMIZATION SUMMARY\n",
    "============================================\n\n",
    "Timestamp: ", format(optimization_results$timestamp, "%Y-%m-%d %H:%M:%S"), "\n",
    "Method: ", combination_info$ordering, "_",
    combination_info$reduction, "\n\n",
    
    "CONFIGURATION\n",
    "-------------\n",
    "Base rate threshold: ", sprintf("%.1f%%", opt_config$base_rate_threshold * 100), "\n",
    "FNR thresholds:\n",
    "  High prevalence: ", sprintf("%.1f%%", opt_config$fnr_threshold_high_prevalence * 100), "\n",
    "  Low prevalence: ", sprintf("%.1f%%", opt_config$fnr_threshold_low_prevalence * 100), "\n",
    "Minimum gamma values:\n",
    "  High prevalence: γ₀ ≥ ", opt_config$gamma_0_high_prevalence, 
    ", γ₁ ≥ ", opt_config$gamma_1_high_prevalence, "\n",
    "  Low prevalence: γ₀ ≥ ", opt_config$gamma_0_low_prevalence,
    ", γ₁ ≥ ", opt_config$gamma_1_low_prevalence, "\n\n",
    
    "RESULTS\n",
    "-------\n"
  )
  
  # Add construct-specific results
  n_constructs <- length(optimization_results$construct_results)
  n_successful <- sum(sapply(optimization_results$construct_results, 
                             function(x) x$success && !isTRUE(x$optimization_failed)))
  n_failed <- sum(sapply(optimization_results$construct_results, 
                         function(x) isTRUE(x$optimization_failed)))
  
  summary_text <- paste0(summary_text,
                         "Constructs optimized: ", n_successful, " of ", n_constructs, "\n")
  if (n_failed > 0) {
    summary_text <- paste0(summary_text,
                           "Constructs failed optimization: ", n_failed, " (using default γ₀=1.0, γ₁=1.0)\n")
  }
  summary_text <- paste0(summary_text, "\n")
  
  # Summary by prevalence category
  high_prev_constructs <- names(optimization_results$construct_results)[
    sapply(optimization_results$construct_results, function(x) x$prevalence_category == "high")
  ]
  low_prev_constructs <- names(optimization_results$construct_results)[
    sapply(optimization_results$construct_results, function(x) x$prevalence_category == "low")
  ]
  
  if (length(high_prev_constructs) > 0) {
    summary_text <- paste0(summary_text, "High prevalence constructs:\n")
    for (cn in high_prev_constructs) {
      cr <- optimization_results$construct_results[[cn]]
      if (cr$success && !isTRUE(cr$optimization_failed)) {
        summary_text <- paste0(summary_text, "  ", cn, ": γ₀=", cr$best_gammas$gamma_0,
                               ", γ₁=", cr$best_gammas$gamma_1, 
                               " (FNR=", sprintf("%.1f%%", cr$best_performance$fnr * 100), ")\n")
      } else if (isTRUE(cr$optimization_failed)) {
        summary_text <- paste0(summary_text, "  ", cn, ": FAILED - ", cr$failure_reason, "\n")
      } else {
        summary_text <- paste0(summary_text, "  ", cn, ": FAILED\n")
      }
    }
  }
  
  if (length(low_prev_constructs) > 0) {
    summary_text <- paste0(summary_text, "\nLow prevalence constructs:\n")
    for (cn in low_prev_constructs) {
      cr <- optimization_results$construct_results[[cn]]
      if (cr$success && !isTRUE(cr$optimization_failed)) {
        summary_text <- paste0(summary_text, "  ", cn, ": γ₀=", cr$best_gammas$gamma_0,
                               ", γ₁=", cr$best_gammas$gamma_1,
                               " (FNR=", sprintf("%.1f%%", cr$best_performance$fnr * 100), ")\n")
      } else if (isTRUE(cr$optimization_failed)) {
        summary_text <- paste0(summary_text, "  ", cn, ": FAILED - ", cr$failure_reason, "\n")
      } else {
        summary_text <- paste0(summary_text, "  ", cn, ": FAILED\n")
      }
    }
  }
  
  writeLines(summary_text, summary_file)
}

#' Generate Optimization Results Table
#'
#' @param optimization_results Optimization results
#' @param output_dir Output directory
generate_optimization_results_table <- function(optimization_results, output_dir) {
  
  # Create results data frame
  results_df <- data.frame(
    construct = character(),
    base_rate = numeric(),
    prevalence_category = character(),
    lowest_gamma_0 = numeric(),
    lowest_gamma_1 = numeric(),
    sensitivity = numeric(),
    specificity = numeric(),
    fnr = numeric(),
    accuracy = numeric(),
    balanced_accuracy = numeric(),
    reduction_percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract data for each construct
  for (cn in names(optimization_results$construct_results)) {
    cr <- optimization_results$construct_results[[cn]]
    
    if (cr$success) {
      # Get construct-specific metrics from optimized performance
      if (!is.null(optimization_results$optimized_performance$construct_metrics) &&
          cn %in% names(optimization_results$optimized_performance$construct_metrics)) {
        cm <- optimization_results$optimized_performance$construct_metrics[[cn]]
      } else {
        # For unidimensional
        cm <- optimization_results$optimized_performance
      }
      
      # Get reduction percentage
      if (!is.null(optimization_results$optimized_performance$construct_reduction_info) &&
          cn %in% names(optimization_results$optimized_performance$construct_reduction_info)) {
        reduction_pct <- optimization_results$optimized_performance$construct_reduction_info[[cn]]$reduction_pct
      } else {
        reduction_pct <- optimization_results$optimized_performance$reduction_pct
      }
      
      results_df <- rbind(results_df, data.frame(
        construct = cn,
        base_rate = cr$base_rate,
        prevalence_category = cr$prevalence_category,
        lowest_gamma_0 = cr$best_gammas$gamma_0,
        lowest_gamma_1 = cr$best_gammas$gamma_1,
        sensitivity = cm$sensitivity,
        specificity = cm$specificity,
        fnr = cm$fnr,
        accuracy = cm$accuracy,
        balanced_accuracy = cm$balanced_accuracy %||% ((cm$sensitivity + cm$specificity) / 2),
        reduction_percentage = reduction_pct,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Sort by prevalence category and base rate
  results_df <- results_df[order(results_df$prevalence_category, -results_df$base_rate), ]
  
  # Save as CSV
  write.csv(results_df, file.path(output_dir, "optimization_results_table.csv"), row.names = FALSE)
  
  # Generate HTML version
  generate_results_table_html(results_df, output_dir)
}

#' Generate HTML Version of Results Table
#'
#' @param results_df Results data frame
#' @param output_dir Output directory
generate_results_table_html <- function(results_df, output_dir) {
  
  html_content <- paste0(
    "<html><head><title>Optimization Results</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1 { color: #333; }",
    "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".high { background-color: #ffe6e6; }",
    ".low { background-color: #e6f3ff; }",
    ".metric-value { text-align: right; }",
    "</style></head><body>",
    "<h1>Construct-Specific Gamma Optimization Results</h1>",
    "<table>",
    "<tr>",
    "<th>Construct</th>",
    "<th class='metric-value'>Base Rate</th>",
    "<th>Category</th>",
    "<th class='metric-value'>γ₀</th>",
    "<th class='metric-value'>γ₁</th>",
    "<th class='metric-value'>Sensitivity</th>",
    "<th class='metric-value'>Specificity</th>",
    "<th class='metric-value'>FNR</th>",
    "<th class='metric-value'>Accuracy</th>",
    "<th class='metric-value'>Balanced Accuracy</th>",
    "<th class='metric-value'>Reduction %</th>",
    "</tr>"
  )
  
  for (i in 1:nrow(results_df)) {
    row_class <- ifelse(results_df$prevalence_category[i] == "high", "high", "low")
    
    html_content <- paste0(html_content,
                           "<tr class='", row_class, "'>",
                           "<td>", results_df$construct[i], "</td>",
                           "<td class='metric-value'>", sprintf("%.1f%%", results_df$base_rate[i] * 100), "</td>",
                           "<td>", results_df$prevalence_category[i], "</td>",
                           "<td class='metric-value'>", sprintf("%.2f", results_df$lowest_gamma_0[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.2f", results_df$lowest_gamma_1[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", results_df$sensitivity[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", results_df$specificity[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", results_df$fnr[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", results_df$accuracy[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", results_df$balanced_accuracy[i]), "</td>",
                           "<td class='metric-value'>", sprintf("%.1f", results_df$reduction_percentage[i]), "</td>",
                           "</tr>")
  }
  
  html_content <- paste0(html_content, "</table></body></html>")
  
  writeLines(html_content, file.path(output_dir, "optimization_results_table.html"))
}

#' Generate Optimization Comparison Report
#'
#' @param original_performance Original performance
#' @param optimization_results Optimization results
#' @param output_dir Output directory
generate_optimization_comparison_report <- function(original_performance,
                                                    optimization_results,
                                                    output_dir) {
  
  # Text version
  report_file <- file.path(output_dir, "optimization_comparison.txt")
  
  opt_perf <- optimization_results$optimized_performance
  
  report_text <- paste0(
    "PERFORMANCE COMPARISON: ORIGINAL vs OPTIMIZED\n",
    "============================================\n\n",
    "Overall Performance Metrics:\n",
    "----------------------------\n",
    "                    Original    Optimized    Change\n",
    sprintf("FNR:              %7.3f     %7.3f    %+7.3f\n",
            original_performance$fnr,
            opt_perf$fnr,
            opt_perf$fnr - original_performance$fnr),
    sprintf("Accuracy:         %7.3f     %7.3f    %+7.3f\n",
            original_performance$accuracy,
            opt_perf$accuracy,
            opt_perf$accuracy - original_performance$accuracy),
    sprintf("Balanced Accuracy:%7.3f     %7.3f    %+7.3f\n",
            original_performance$balanced_accuracy,
            opt_perf$balanced_accuracy,
            opt_perf$balanced_accuracy - original_performance$balanced_accuracy),
    sprintf("Reduction:        %6.1f%%     %6.1f%%    %+6.1f%%\n",
            original_performance$reduction_pct,
            opt_perf$reduction_pct,
            opt_perf$reduction_pct - original_performance$reduction_pct),
    "\n"
  )
  
  # Add construct-specific comparisons if available
  if (!is.null(original_performance$construct_metrics) && 
      !is.null(opt_perf$construct_metrics)) {
    
    report_text <- paste0(report_text,
                          "Construct-Specific FNR Changes:\n",
                          "-------------------------------\n")
    
    for (cn in names(original_performance$construct_metrics)) {
      orig_fnr <- original_performance$construct_metrics[[cn]]$fnr
      opt_fnr <- opt_perf$construct_metrics[[cn]]$fnr
      
      report_text <- paste0(report_text,
                            sprintf("%-30s: %.3f → %.3f (%+.3f)\n",
                                    cn, orig_fnr, opt_fnr, opt_fnr - orig_fnr))
    }
  }
  
  writeLines(report_text, report_file)
  
  # HTML version
  generate_comparison_report_html(original_performance, opt_perf, output_dir)
}

#' Generate HTML Comparison Report
#'
#' @param original_performance Original performance
#' @param optimized_performance Optimized performance
#' @param output_dir Output directory
generate_comparison_report_html <- function(original_performance, optimized_performance,
                                            output_dir) {
  
  html_content <- paste0(
    "<html><head><title>Optimization Comparison</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1, h2 { color: #333; }",
    "table { border-collapse: collapse; width: 60%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".metric-value { text-align: right; }",
    ".improvement { color: green; font-weight: bold; }",
    ".worse { color: red; font-weight: bold; }",
    "</style></head><body>",
    "<h1>Performance Comparison: Original vs Optimized</h1>",
    "<h2>Overall Performance</h2>",
    "<table>",
    "<tr><th>Metric</th><th class='metric-value'>Original</th>",
    "<th class='metric-value'>Optimized</th><th class='metric-value'>Change</th></tr>"
  )
  
  # Add overall metrics
  metrics <- list(
    list(name = "FNR", orig = original_performance$fnr, opt = optimized_performance$fnr, lower_better = TRUE),
    list(name = "Accuracy", orig = original_performance$accuracy, opt = optimized_performance$accuracy, lower_better = FALSE),
    list(name = "Balanced Accuracy", orig = original_performance$balanced_accuracy, 
         opt = optimized_performance$balanced_accuracy, lower_better = FALSE),
    list(name = "Reduction %", orig = original_performance$reduction_pct / 100, 
         opt = optimized_performance$reduction_pct / 100, lower_better = FALSE)
  )
  
  for (metric in metrics) {
    change <- metric$opt - metric$orig
    change_class <- ifelse(
      (metric$lower_better && change < 0) || (!metric$lower_better && change > 0),
      "improvement", 
      ifelse(change == 0, "", "worse")
    )
    
    html_content <- paste0(html_content,
                           "<tr>",
                           "<td>", metric$name, "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", metric$orig), "</td>",
                           "<td class='metric-value'>", sprintf("%.3f", metric$opt), "</td>",
                           "<td class='metric-value ", change_class, "'>", 
                           sprintf("%+.3f", change), "</td>",
                           "</tr>")
  }
  
  html_content <- paste0(html_content, "</table></body></html>")
  
  writeLines(html_content, file.path(output_dir, "optimization_comparison.html"))
}

#' Apply Construct-Specific Reduction
#'
#' This function applies reduction with different gamma values per construct
#' by leveraging the existing multi-construct infrastructure
#'
#' @param ordered_items Ordered items
#' @param data Test data
#' @param config Configuration
#' @param method Reduction method
#' @param construct_params Construct-specific parameters
#' @param training_params Training parameters
#' @param original_method Original method to preserve structure
#' @return Reduction results
apply_construct_specific_reduction <- function(ordered_items, data, config, method,
                                               construct_params, training_params, 
                                               original_method = NULL) {
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize combined results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  construct_classifications <- list()
  construct_stopped_at <- list()
  
  # Extract constraints
  constraints <- config$constraints %||% list()
  stop_low_only <- constraints$stop_low_only %||% FALSE
  min_items_per_construct <- constraints$min_items_per_construct %||% 1
  complete_triggered_constructs <- constraints$complete_triggered_constructs %||% FALSE
  
  # Initialize triggered constructs tracking
  triggered_constructs <- vector("list", n_respondents)
  
  # Process each construct with its specific gamma values
  for (construct_name in names(config$constructs)) {
    # Get items for this construct
    construct_items <- config$constructs[[construct_name]]
    construct_ordered <- intersect(ordered_items, construct_items)
    
    # Get construct-specific parameters
    method_params <- construct_params[[construct_name]]
    
    # Get construct-specific cutoff
    construct_cutoff <- config$cutoffs[[construct_name]]
    
    # Get construct-specific training params
    construct_training_params <- NULL
    if (!is.null(training_params) && is.list(training_params) && construct_name %in% names(training_params)) {
      construct_training_params <- training_params[[construct_name]]
    } else if (!is.null(training_params)) {
      construct_training_params <- training_params
    }
    
    # Apply reduction for this construct
    if (complete_triggered_constructs) {
      construct_result <- switch(method,
                                 sc_ep = reduce_sc_ep_integrated(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct,
                                   complete_triggered_constructs, triggered_constructs, construct_name
                                 ),
                                 sc_sor = reduce_sc_sor_integrated(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct,
                                   complete_triggered_constructs, triggered_constructs, construct_name
                                 ),
                                 sc_mor = reduce_sc_mor_integrated(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct,
                                   complete_triggered_constructs, triggered_constructs, construct_name
                                 )
      )
    } else {
      construct_result <- switch(method,
                                 sc_ep = reduce_sc_ep(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct
                                 ),
                                 sc_sor = reduce_sc_sor(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct
                                 ),
                                 sc_mor = reduce_sc_mor(
                                   construct_ordered, data, config, method_params, construct_cutoff,
                                   construct_training_params, stop_low_only, min_items_per_construct
                                 )
      )
    }
    
    # Update triggered constructs if applicable
    if (!is.null(construct_result$triggered_constructs)) {
      triggered_constructs <- construct_result$triggered_constructs
    }
    
    # Map results back to full matrix
    for (item in construct_ordered) {
      item_idx <- which(ordered_items == item)
      construct_item_idx <- which(construct_ordered == item)
      
      if (length(item_idx) > 0 && length(construct_item_idx) > 0) {
        item_idx <- item_idx[1]
        construct_item_idx <- construct_item_idx[1]
        
        if (item_idx <= ncol(items_administered) && 
            construct_item_idx <= ncol(construct_result$items_administered)) {
          items_administered[, item_idx] <- construct_result$items_administered[, construct_item_idx]
        }
      }
    }
    
    # Store construct-specific results
    construct_classifications[[construct_name]] <- construct_result$classifications
    construct_stopped_at[[construct_name]] <- construct_result$stopped_at
  }
  
  # Calculate overall statistics
  n_items_used <- rowSums(items_administered)
  overall_classifications <- apply(
    do.call(cbind, construct_classifications), 1,
    function(x) as.numeric(any(x == 1, na.rm = TRUE))
  )
  
  # Get the original reduction result to preserve all fields
  original_reduction_result <- if (!is.null(original_method)) {
    original_method$reduction_result
  } else {
    list()  # Empty list if no original method provided
  }
  
  # Return with the same structure as the original reduction result
  return(list(
    items_administered = items_administered,
    classifications = overall_classifications,
    construct_classifications = construct_classifications,
    stopped_at = n_items_used,
    construct_stopped_at = construct_stopped_at,
    n_items_used = n_items_used,
    triggered_constructs = triggered_constructs,
    training_params = training_params,  # IMPORTANT: Preserve training params
    constraints_applied = original_reduction_result$constraints_applied,  # IMPORTANT: Preserve constraints
    method_info = list(
      description = "Multi-construct reduction with construct-specific gamma values",
      construct_gammas = construct_params,
      original_description = original_reduction_result$method_info$description %||% 
        original_reduction_result$method_info
    ),
    # Preserve any other fields from the original reduction result
    min_items = original_reduction_result$min_items,
    max_items = original_reduction_result$max_items,
    mean_items = original_reduction_result$mean_items,
    sd_items = original_reduction_result$sd_items,
    median_items = original_reduction_result$median_items,
    reduction_pct = original_reduction_result$reduction_pct
  ))
}

# NULL-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}