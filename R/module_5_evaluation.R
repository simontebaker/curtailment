# ============================================================================
# Module 5: Evaluation Module
# ============================================================================
# Purpose: Evaluate all method combinations, calculate performance metrics,
#          identify top candidates, and generate comprehensive reports
# ============================================================================

# Note on required packages:
# - ggplot2
# - gridExtra
# - viridis
# - DT
# - reshape2

# Required packages
required_packages <- c("ggplot2", "gridExtra", "viridis", "DT", "reshape2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for visualization functionality"))
  }
}

#' Evaluate All Method Combinations
#'
#' @param results_path Path to saved results file (default: "all_combination_results.rds")
#' @param prepared_data Prepared data object from Module 1
#' @param config Analysis configuration from Module 2
#' @param output_dir Directory for output files (default: "evaluation_results")
#' @return List containing evaluation results and recommendations
#' @export
evaluate_all_methods <- function(results_path = "all_combination_results.rds",
                                 prepared_data, 
                                 config,
                                 output_dir = "evaluation_results") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load results
  cat("Loading method combination results...\n")
  # Check if results exist in current environment first
  if (exists("all_combination_results") && is.list(all_combination_results)) {
    cat("  Using results from current environment\n")
    all_results <- all_combination_results
    # If results do not exist in current environment, check if results file exists
  } else if (file.exists(results_path)) {
    cat("  Loading results from file:", results_path, "\n")
    all_results <- readRDS(results_path)
    # If results do not exist in current environment and results file does not exist
  } else {
    stop("Method combination results not found. Please run Modules 1-4 first.")
  }
  
  # Extract successful results
  successful_results <- all_results[!sapply(all_results, function(x) "error" %in% names(x))]
  failed_results <- all_results[sapply(all_results, function(x) "error" %in% names(x))]
  
  cat(sprintf("Successfully loaded %d method combinations (%d failed)\n", 
              length(successful_results), length(failed_results)))
  
  # Calculate performance for each method (including construct-level metrics)
  cat("\nCalculating performance metrics...\n")
  performance_list <- list()
  
  pb <- txtProgressBar(min = 0, max = length(successful_results), style = 3)
  for (i in seq_along(successful_results)) {
    method_id <- names(successful_results)[i]
    result <- successful_results[[i]]
    
    # Calculate FULL performance metrics (including construct-level)
    perf <- calculate_method_performance_full(result, prepared_data)
    perf$method_id <- method_id
    perf$ordering <- result$combination$ordering
    perf$reduction <- result$combination$reduction
    perf$gamma_0 <- result$combination$gamma_0
    perf$gamma_1 <- result$combination$gamma_1
    
    performance_list[[method_id]] <- perf
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Create performance matrix
  cat("\nCreating performance matrix...\n")
  performance_matrix <- create_performance_matrix(performance_list)
  
  # Calculate composite score and utility score for ALL methods
  cat("\nCalculating composite and utility scores for all methods...\n")
  
  # Composite score weights
  w1 <- config$screening_weights$w1_fnr
  w2 <- config$screening_weights$w2_accuracy
  w3 <- config$screening_weights$w3_efficiency
  
  # Utility score weights
  alpha <- config$utility_weights$alpha_fnr
  beta <- config$utility_weights$beta_accuracy
  gamma <- config$utility_weights$gamma_efficiency
  
  # Calculate scores for all methods (CONSISTENT ORDERING: FNR, ACCURACY, EFFICIENCY)
  performance_matrix$composite_score <- 
    w1 * (1 - performance_matrix$fnr) +
    w2 * performance_matrix$balanced_accuracy +
    w3 * (performance_matrix$reduction_pct / 100)
  
  performance_matrix$utility_score <- 
    alpha * (1 - performance_matrix$fnr) +
    beta * performance_matrix$balanced_accuracy +
    gamma * (performance_matrix$reduction_pct / 100)
  
  # Handle NA values
  performance_matrix$composite_score[is.na(performance_matrix$composite_score)] <- 0
  performance_matrix$utility_score[is.na(performance_matrix$utility_score)] <- 0
  
  # Sort based on evaluation type
  if (config$two_step_mode) {
    # For two-step: sort by composite score (used for screening)
    performance_matrix <- performance_matrix[order(performance_matrix$composite_score, decreasing = TRUE), ]
  } else {
    # For single-step: sort by utility score (used for selection)
    performance_matrix <- performance_matrix[order(performance_matrix$utility_score, decreasing = TRUE), ]
  }
  
  # Add ranking columns to performance matrix
  performance_matrix$composite_rank <- rank(-performance_matrix$composite_score)
  performance_matrix$utility_rank <- rank(-performance_matrix$utility_score)
  performance_matrix$fnr_rank <- rank(performance_matrix$fnr)
  performance_matrix$accuracy_rank <- rank(-performance_matrix$accuracy)
  performance_matrix$balanced_accuracy_rank <- rank(-performance_matrix$balanced_accuracy)
  performance_matrix$efficiency_rank <- rank(-performance_matrix$reduction_pct)
  
  # Apply two-step evaluation if enabled
  if (config$two_step_mode) {
    cat("\nApplying two-step evaluation process...\n")
    evaluation_results <- two_step_evaluation(
      performance_matrix, 
      config,
      performance_list
    )
  } else {
    cat("\nPerforming single-step evaluation...\n")
    evaluation_results <- single_step_evaluation(
      performance_matrix,
      config,
      performance_list
    )
  }
  
  # Generate visualizations
  cat("\nGenerating visualizations...\n")
  generate_evaluation_plots(
    performance_matrix,
    evaluation_results,
    output_dir
  )
  
  # Generate reports
  cat("\nGenerating evaluation reports...\n")
  generate_evaluation_reports(
    performance_matrix,
    evaluation_results,
    config,
    output_dir,
    performance_list  # Pass the full performance list
  )
  
  # Save results with composite and utility scores and rankings included
  saveRDS(evaluation_results, file.path(output_dir, "evaluation_results.rds"))
  write.csv(performance_matrix, file.path(output_dir, "performance_matrix.csv"), row.names = FALSE)
  
  cat("\nEvaluation complete! Results saved to:", output_dir, "\n")
  
  return(evaluation_results)
}

#' Calculate Full Performance Metrics for a Single Method
#'
#' @param result Method result object
#' @param prepared_data Prepared data object
#' @return List of performance metrics including construct-level details
calculate_method_performance_full <- function(result, prepared_data) {
  
  # Get reduction results
  reduction_results <- result$reduction_result
  
  # Use the enhanced helper function
  perf <- calculate_reduction_performance_full(reduction_results, prepared_data)
  
  # Add method-specific information
  perf$combination <- result$combination
  
  return(perf)
}

#' Calculate Performance Metrics for a Single Method (Basic)
#'
#' @param result Method result object
#' @param prepared_data Prepared data object
#' @return List of performance metrics
calculate_method_performance <- function(result, prepared_data) {
  
  # Get reduction results
  reduction_results <- result$reduction_result
  
  # Use the provided helper function
  perf <- calculate_reduction_performance(reduction_results, prepared_data)
  
  # Add method-specific information
  perf$combination <- result$combination
  
  return(perf)
}

#' Create Performance Matrix from Results
#'
#' @param performance_list List of performance results
#' @return Data frame with all metrics
create_performance_matrix <- function(performance_list) {
  
  # Extract metrics into data frame
  metrics_df <- do.call(rbind, lapply(performance_list, function(perf) {
    data.frame(
      method_id = perf$method_id,
      ordering = perf$ordering,
      reduction = perf$reduction,
      gamma_0 = ifelse(is.na(perf$gamma_0), NA, perf$gamma_0),
      gamma_1 = ifelse(is.na(perf$gamma_1), NA, perf$gamma_1),
      sensitivity = perf$sensitivity,
      specificity = perf$specificity,
      fnr = perf$fnr,
      fpr = perf$fpr,
      accuracy = perf$accuracy,
      balanced_accuracy = perf$balanced_accuracy,
      ppv = perf$ppv,
      npv = perf$npv,
      mean_items_used = perf$mean_items_used,
      median_items_used = perf$median_items_used,
      sd_items_used = perf$sd_items_used,
      reduction_pct = perf$reduction_pct,
      stringsAsFactors = FALSE
    )
  }))
  
  # Do NOT sort by method_id - will sort by scores later
  
  return(metrics_df)
}

#' Two-Step Evaluation Process
#'
#' @param performance_matrix Performance matrix data frame (already includes scores and is sorted)
#' @param config Analysis configuration
#' @param performance_list Full performance results (with construct metrics)
#' @return Evaluation results
two_step_evaluation <- function(performance_matrix, config, performance_list) {
  
  # Step 1: Screening Phase
  cat("  Step 1: Screening phase...\n")
  
  # Performance matrix already has composite_score and all rankings
  
  # Select top candidates (already sorted by composite score)
  top_candidates <- performance_matrix[1:min(config$top_candidates, nrow(performance_matrix)), ]
  
  cat(sprintf("    Selected %d top candidates for detailed evaluation\n", nrow(top_candidates)))
  
  # Step 2: Detailed Analysis
  cat("  Step 2: Detailed analysis of top candidates...\n")
  
  valid_candidates <- top_candidates
  
  # Sort valid candidates by utility score for detailed analysis
  valid_candidates <- valid_candidates[order(valid_candidates$utility_score, decreasing = TRUE), ]
  
  # Identify Pareto-optimal solutions
  pareto_optimal <- identify_pareto_optimal(valid_candidates)
  
  # Sort Pareto-optimal by utility score
  pareto_optimal <- pareto_optimal[order(pareto_optimal$utility_score, decreasing = TRUE), ]
  
  # Select best method (highest utility score)
  best_method <- pareto_optimal[1, ]
  
  # Get full performance for all valid candidates
  valid_candidates_full_performance <- list()
  for (method_id in valid_candidates$method_id) {
    valid_candidates_full_performance[[method_id]] <- performance_list[[method_id]]
  }
  
  # Compile results
  results <- list(
    performance_matrix = performance_matrix,  # Already sorted by composite score
    screening_results = list(
      all_candidates = performance_matrix,   # Sorted by composite score
      top_candidates = top_candidates,       # Top N by composite score
      composite_weights = c(w1 = config$screening_weights$w1_fnr, 
                            w2 = config$screening_weights$w2_accuracy, 
                            w3 = config$screening_weights$w3_efficiency)
    ),
    detailed_results = list(
      valid_candidates = valid_candidates,   # Sorted by utility score
      valid_candidates_full_performance = valid_candidates_full_performance,  # Full perf for all top candidates
      pareto_optimal = pareto_optimal,       # Sorted by utility score
      utility_weights = c(alpha = config$utility_weights$alpha_fnr, 
                          beta = config$utility_weights$beta_accuracy, 
                          gamma = config$utility_weights$gamma_efficiency)
    ),
    recommended_method = best_method,
    full_performance = performance_list[[best_method$method_id]],
    evaluation_type = "two_step"
  )
  
  return(results)
}

#' Single-Step Evaluation Process
#'
#' @param performance_matrix Performance matrix data frame (already includes scores and is sorted)
#' @param config Analysis configuration
#' @param performance_list Full performance results (with construct metrics)
#' @return Evaluation results
single_step_evaluation <- function(performance_matrix, config, performance_list) {
  
  cat("  Evaluating all methods...\n")
  
  valid_candidates <- performance_matrix
  
  # Sort by utility score for single-step evaluation
  valid_candidates <- valid_candidates[order(valid_candidates$utility_score, decreasing = TRUE), ]
  
  # For single-step, select top candidates based on utility score
  top_n <- min(config$top_candidates, nrow(valid_candidates))
  top_candidates <- valid_candidates[1:top_n, ]
  
  # Identify Pareto-optimal solutions
  pareto_optimal <- identify_pareto_optimal(valid_candidates)
  
  # Sort Pareto-optimal by utility score
  pareto_optimal <- pareto_optimal[order(pareto_optimal$utility_score, decreasing = TRUE), ]
  
  # Select best method
  best_method <- pareto_optimal[1, ]
  
  # Get full performance for all top candidates
  valid_candidates_full_performance <- list()
  for (method_id in top_candidates$method_id) {
    valid_candidates_full_performance[[method_id]] <- performance_list[[method_id]]
  }
  
  # Compile results
  results <- list(
    performance_matrix = performance_matrix,  # Sorted by composite score
    detailed_results = list(
      valid_candidates = top_candidates,   # Top N by utility score
      valid_candidates_full_performance = valid_candidates_full_performance,  # Full perf for top candidates
      pareto_optimal = pareto_optimal,       # Sorted by utility score
      utility_weights = c(alpha = config$utility_weights$alpha_fnr, 
                          beta = config$utility_weights$beta_accuracy, 
                          gamma = config$utility_weights$gamma_efficiency)
    ),
    recommended_method = best_method,
    full_performance = performance_list[[best_method$method_id]],
    evaluation_type = "single_step"
  )
  
  return(results)
}

#' Identify Pareto-Optimal Solutions
#'
#' @param candidates Candidate methods data frame
#' @return Data frame of Pareto-optimal methods
identify_pareto_optimal <- function(candidates) {
  
  n <- nrow(candidates)
  is_dominated <- rep(FALSE, n)
  
  # Check dominance for efficiency vs accuracy trade-off
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        # Method j dominates i if it's better in accuracy AND efficiency
        # AND at least strictly better in one
        if (candidates$balanced_accuracy[j] >= candidates$balanced_accuracy[i] &&
            candidates$reduction_pct[j] >= candidates$reduction_pct[i] &&
            candidates$fnr[j] <= candidates$fnr[i] &&
            (candidates$balanced_accuracy[j] > candidates$balanced_accuracy[i] ||
             candidates$reduction_pct[j] > candidates$reduction_pct[i] ||
             candidates$fnr[j] < candidates$fnr[i])) {
          is_dominated[i] <- TRUE
          break
        }
      }
    }
  }
  
  pareto_optimal <- candidates[!is_dominated, ]
  pareto_optimal$is_pareto_optimal <- TRUE
  
  return(pareto_optimal)
}

#' Generate Evaluation Plots
#'
#' @param performance_matrix Performance matrix
#' @param evaluation_results Evaluation results
#' @param output_dir Output directory
generate_evaluation_plots <- function(performance_matrix, evaluation_results, output_dir) {
  
  plots_dir <- file.path(output_dir, "visualizations")
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # 1. Pareto Frontier Plot
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    pareto_plot <- create_pareto_plot(performance_matrix, evaluation_results)
    ggplot2::ggsave(file.path(plots_dir, "pareto_frontier.png"), 
                    pareto_plot, width = 10, height = 8)
  }
  
  # 2. FNR Heatmap
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    fnr_heatmap <- create_fnr_heatmap(performance_matrix)
    ggplot2::ggsave(file.path(plots_dir, "fnr_heatmap.png"), 
                    fnr_heatmap, width = 12, height = 8)
  }
  
  # 3. Item Usage Distribution
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    usage_plot <- create_item_usage_plot(performance_matrix)
    ggplot2::ggsave(file.path(plots_dir, "item_usage_distribution.png"), 
                    usage_plot, width = 10, height = 6)
  }
  
  # 4. Performance Comparison Plot
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    comparison_plot <- create_performance_comparison(evaluation_results)
    ggplot2::ggsave(file.path(plots_dir, "performance_comparison.png"), 
                    comparison_plot, width = 12, height = 8)
  }
}

#' Create Pareto Frontier Plot
#'
#' @param performance_matrix Performance matrix
#' @param evaluation_results Evaluation results
#' @return ggplot object
create_pareto_plot <- function(performance_matrix, evaluation_results) {
  
  # Prepare data
  plot_data <- performance_matrix
  plot_data$is_pareto <- FALSE
  
  if ("detailed_results" %in% names(evaluation_results)) {
    pareto_ids <- evaluation_results$detailed_results$pareto_optimal$method_id
    plot_data$is_pareto[plot_data$method_id %in% pareto_ids] <- TRUE
  }
  
  plot_data$is_recommended <- plot_data$method_id == evaluation_results$recommended_method$method_id
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reduction_pct, y = balanced_accuracy)) +
    ggplot2::geom_point(ggplot2::aes(color = is_pareto, shape = reduction, size = is_recommended), 
                        alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "blue"),
                                labels = c("Dominated", "Pareto-optimal")) +
    ggplot2::scale_size_manual(values = c("FALSE" = 3, "TRUE" = 6),
                               labels = c("Other", "Recommended")) +
    ggplot2::geom_text(data = plot_data[plot_data$is_recommended, ],
                       ggplot2::aes(label = "Best"), vjust = -1.5) +
    ggplot2::labs(
      title = "Pareto Frontier: Efficiency vs. Accuracy Trade-off",
      x = "Item Reduction (%)",
      y = "Balanced Accuracy",
      color = "Status",
      shape = "Reduction Method",
      size = "Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")
  
  # Add Pareto frontier line
  if (any(plot_data$is_pareto)) {
    pareto_data <- plot_data[plot_data$is_pareto, ]
    pareto_data <- pareto_data[order(pareto_data$reduction_pct), ]
    p <- p + ggplot2::geom_line(data = pareto_data, color = "blue", linetype = "dashed")
  }
  
  return(p)
}

#' Create FNR Heatmap
#'
#' @param performance_matrix Performance matrix
#' @return ggplot object
create_fnr_heatmap <- function(performance_matrix) {
  
  # Prepare data for heatmap
  heatmap_data <- performance_matrix[, c("ordering", "reduction", "fnr", "gamma_0", "gamma_1")]
  
  # Create labels including gamma values for SC methods
  heatmap_data$reduction_label <- ifelse(
    heatmap_data$reduction %in% c("sc_ep", "sc_sor", "sc_mor"),
    paste0(heatmap_data$reduction, " (", heatmap_data$gamma_0, ",", heatmap_data$gamma_1, ")"),
    heatmap_data$reduction
  )
  
  # Create plot
  p <- ggplot2::ggplot(heatmap_data, 
                       ggplot2::aes(x = ordering, y = reduction_label, fill = fnr)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", fnr)), size = 3) +
    ggplot2::scale_fill_viridis_c(name = "FNR", direction = -1, limits = c(0, 0.1)) +
    ggplot2::labs(
      title = "False Negative Rate Heatmap",
      x = "Ordering Method",
      y = "Reduction Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Create Item Usage Distribution Plot
#'
#' @param performance_matrix Performance matrix
#' @return ggplot object
create_item_usage_plot <- function(performance_matrix) {
  
  # Prepare data
  plot_data <- performance_matrix[, c("method_id", "ordering", "reduction", 
                                      "mean_items_used", "reduction_pct")]
  
  # Create grouped bar plot
  p <- ggplot2::ggplot(plot_data, 
                       ggplot2::aes(x = reorder(method_id, -mean_items_used), 
                                    y = mean_items_used, 
                                    fill = reduction)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~ ordering, scales = "free_x", ncol = 2) +
    ggplot2::labs(
      title = "Mean Items Administered by Method",
      x = "Method",
      y = "Mean Items Used",
      fill = "Reduction Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Create Performance Comparison Plot
#'
#' @param evaluation_results Evaluation results
#' @return ggplot object
create_performance_comparison <- function(evaluation_results) {
  
  # Get top methods
  if (evaluation_results$evaluation_type == "two_step") {
    top_methods <- evaluation_results$screening_results$top_candidates
  } else {
    pareto <- evaluation_results$detailed_results$pareto_optimal
    top_methods <- pareto[order(pareto$utility_score, decreasing = TRUE)[1:min(10, nrow(pareto))], ]
  }
  
  # Prepare data for radar plot alternative (bar plot comparison)
  metrics <- c("sensitivity", "specificity", "balanced_accuracy", "reduction_pct")
  plot_data <- reshape2::melt(top_methods[, c("method_id", metrics)], 
                              id.vars = "method_id",
                              variable.name = "metric",
                              value.name = "value")
  
  # Normalize reduction_pct to 0-1 scale
  plot_data$value[plot_data$metric == "reduction_pct"] <- 
    plot_data$value[plot_data$metric == "reduction_pct"] / 100
  
  # Create grouped bar plot
  p <- ggplot2::ggplot(plot_data, 
                       ggplot2::aes(x = metric, y = value, fill = method_id)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = "Performance Comparison of Top Methods",
      x = "Metric",
      y = "Value",
      fill = "Method"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

#' Generate Evaluation Reports
#'
#' @param performance_matrix Performance matrix
#' @param evaluation_results Evaluation results
#' @param config Configuration
#' @param output_dir Output directory
#' @param performance_list Full performance list with construct metrics
generate_evaluation_reports <- function(performance_matrix, evaluation_results, 
                                        config, output_dir, performance_list = NULL) {
  
  # 1. Executive Summary
  generate_executive_summary(evaluation_results, output_dir)
  
  # 2. Detailed Performance Report
  generate_performance_report(performance_matrix, evaluation_results, config, output_dir)
  
  # 3. Method Comparison Table
  generate_comparison_table(performance_matrix, evaluation_results, output_dir)
  
  # 4. Implementation Guide
  generate_implementation_guide(evaluation_results, config, output_dir)
  
  # 5. Evaluation Metadata
  generate_evaluation_metadata(evaluation_results, config, output_dir)
  
  # 6. NEW: Construct-Level Performance Report for Top Methods
  if (!is.null(performance_list) && !is.null(evaluation_results$detailed_results$valid_candidates_full_performance)) {
    generate_construct_performance_report(evaluation_results, output_dir)
  }
}

#' Generate Construct-Level Performance Report for Top Methods
#'
#' @param evaluation_results Evaluation results with full performance data
#' @param output_dir Output directory
generate_construct_performance_report <- function(evaluation_results, output_dir) {
  
  report_file <- file.path(output_dir, "construct_performance_report.html")
  
  # Get valid candidates and their full performance
  valid_candidates <- evaluation_results$detailed_results$valid_candidates
  full_perf_list <- evaluation_results$detailed_results$valid_candidates_full_performance
  
  # Create HTML report
  html_content <- paste0(
    "<html><head><title>Construct-Level Performance Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "h1, h2, h3 { color: #333; }",
    "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; font-weight: bold; }",
    ".method-section { margin: 30px 0; padding: 20px; background-color: #f9f9f9; border-left: 4px solid #007bff; }",
    ".recommended { background-color: #e6ffe6; }",
    ".pareto { background-color: #e6f3ff; }",
    ".note { background-color: #f0f0f0; padding: 10px; margin: 10px 0; border-left: 4px solid #333; }",
    ".metric-value { text-align: right; }",
    ".construct-name { font-weight: bold; color: #007bff; }",
    "</style></head><body>",
    "<h1>Construct-Level Performance Analysis</h1>",
    "<p>This report shows detailed construct-level performance metrics for the top ",
    nrow(valid_candidates), " performing methods.</p>"
  )
  
  # Add note about scores
  html_content <- paste0(html_content,
                         "<div class='note'><strong>Note:</strong> ",
                         "Overall performance metrics are weighted by sample sizes across constructs, ",
                         "while construct-level averages give equal weight to each construct. ",
                         "Composite and Utility scores are calculated at the method level, not the construct level.</div>"
  )
  
  # Process each top method
  for (i in 1:nrow(valid_candidates)) {
    method <- valid_candidates[i, ]
    method_id <- method$method_id
    full_perf <- full_perf_list[[method_id]]
    
    # Determine if this is the recommended method or Pareto-optimal
    is_recommended <- method_id == evaluation_results$recommended_method$method_id
    is_pareto <- method_id %in% evaluation_results$detailed_results$pareto_optimal$method_id
    
    section_class <- ifelse(is_recommended, "method-section recommended", 
                            ifelse(is_pareto, "method-section pareto", "method-section"))
    
    # Method header
    html_content <- paste0(html_content,
                           "<div class='", section_class, "'>",
                           "<h2>Method ", i, ": ", method_id, "</h2>",
                           "<p><strong>Ordering:</strong> ", method$ordering, 
                           " | <strong>Reduction:</strong> ", method$reduction)
    
    if (!is.na(method$gamma_0)) {
      html_content <- paste0(html_content,
                             " | <strong>Parameters:</strong> γ₀=", method$gamma_0, ", γ₁=", method$gamma_1)
    }
    
    html_content <- paste0(html_content,
                           "</p>",
                           "<p><strong>Overall Performance:</strong> ",
                           "Sensitivity=", sprintf("%.3f", method$sensitivity),
                           ", Specificity=", sprintf("%.3f", method$specificity),
                           ", FNR=", sprintf("%.3f", method$fnr),
                           ", Accuracy=", sprintf("%.3f", method$accuracy),
                           ", Balanced Accuracy=", sprintf("%.3f", method$balanced_accuracy),
                           ", Reduction=", sprintf("%.1f%%", method$reduction_pct),
                           "</p>",
                           "<p><strong>Scores:</strong> ",
                           "Composite Score=", sprintf("%.3f", method$composite_score),
                           ", Utility Score=", sprintf("%.3f", method$utility_score),
                           "</p>"
    )
    
    if (is_recommended) {
      html_content <- paste0(html_content, "<p><strong>⭐ RECOMMENDED METHOD</strong></p>")
    } else if (is_pareto) {
      html_content <- paste0(html_content, "<p><strong>✓ Pareto-optimal</strong></p>")
    }
    
    # Check if construct metrics exist
    if (!is.null(full_perf$construct_metrics) && !is.null(full_perf$construct_reduction_info)) {
      html_content <- paste0(html_content,
                             "<h3>Construct-Level Metrics:</h3>",
                             "<table>",
                             "<tr>",
                             "<th>Construct</th>",
                             "<th class='metric-value'>Sensitivity</th>",
                             "<th class='metric-value'>Specificity</th>",
                             "<th class='metric-value'>FNR</th>",
                             "<th class='metric-value'>Accuracy</th>",
                             "<th class='metric-value'>Balanced Accuracy</th>",
                             "<th class='metric-value'>Reduction %</th>",
                             "</tr>"
      )
      
      # Variables to track averages
      sum_sensitivity <- 0
      sum_specificity <- 0
      sum_fnr <- 0
      sum_accuracy <- 0
      sum_balanced_accuracy <- 0
      sum_reduction_pct <- 0
      n_constructs <- 0
      n_constructs_with_reduction <- 0
      
      # Add row for each construct
      for (construct_name in names(full_perf$construct_metrics)) {
        cm <- full_perf$construct_metrics[[construct_name]]
        
        # Calculate balanced accuracy for the construct if not present
        if (is.null(cm$balanced_accuracy)) {
          cm$balanced_accuracy <- (cm$sensitivity + cm$specificity) / 2
        }
        
        # Get construct-specific reduction percentage
        construct_reduction_pct <- full_perf$construct_reduction_info[[construct_name]]$reduction_pct
        
        html_content <- paste0(html_content,
                               "<tr>",
                               "<td class='construct-name'>", construct_name, "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$sensitivity), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$specificity), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$fnr), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$accuracy), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$balanced_accuracy), "</td>",
                               "<td class='metric-value'>", 
                               ifelse(is.na(construct_reduction_pct), "N/A", sprintf("%.1f", construct_reduction_pct)), 
                               "</td>",
                               "</tr>"
        )
        
        # Update sums for averages
        sum_sensitivity <- sum_sensitivity + cm$sensitivity
        sum_specificity <- sum_specificity + cm$specificity
        sum_fnr <- sum_fnr + cm$fnr
        sum_accuracy <- sum_accuracy + cm$accuracy
        sum_balanced_accuracy <- sum_balanced_accuracy + cm$balanced_accuracy
        n_constructs <- n_constructs + 1
        
        if (!is.na(construct_reduction_pct)) {
          sum_reduction_pct <- sum_reduction_pct + construct_reduction_pct
          n_constructs_with_reduction <- n_constructs_with_reduction + 1
        }
      }
      
      html_content <- paste0(html_content, "</table>")
      
      # Add summary statistics
      if (n_constructs > 0) {
        avg_sensitivity <- sum_sensitivity / n_constructs
        avg_specificity <- sum_specificity / n_constructs
        avg_fnr <- sum_fnr / n_constructs
        avg_accuracy <- sum_accuracy / n_constructs
        avg_balanced_accuracy <- sum_balanced_accuracy / n_constructs
        
        summary_text <- paste0(
          "<p><em>Construct-level averages: ",
          "Sensitivity=", sprintf("%.3f", avg_sensitivity),
          ", Specificity=", sprintf("%.3f", avg_specificity),
          ", FNR=", sprintf("%.3f", avg_fnr),
          ", Accuracy=", sprintf("%.3f", avg_accuracy),
          ", Balanced Accuracy=", sprintf("%.3f", avg_balanced_accuracy)
        )
        
        if (n_constructs_with_reduction > 0) {
          avg_reduction_pct <- sum_reduction_pct / n_constructs_with_reduction
          summary_text <- paste0(summary_text, 
                                 ", Reduction=", sprintf("%.1f%%", avg_reduction_pct))
        }
        
        summary_text <- paste0(summary_text, "</em></p>")
        html_content <- paste0(html_content, summary_text)
      }
      
    } else if (!is.null(full_perf$construct_metrics)) {
      # Fallback if construct_reduction_info is missing
      html_content <- paste0(html_content,
                             "<p><em>Construct-specific reduction percentages not available. Using overall reduction percentage.</em></p>",
                             "<h3>Construct-Level Metrics:</h3>",
                             "<table>",
                             "<tr>",
                             "<th>Construct</th>",
                             "<th class='metric-value'>Sensitivity</th>",
                             "<th class='metric-value'>Specificity</th>",
                             "<th class='metric-value'>FNR</th>",
                             "<th class='metric-value'>Accuracy</th>",
                             "<th class='metric-value'>Balanced Accuracy</th>",
                             "<th class='metric-value'>Reduction %</th>",
                             "</tr>"
      )
      
      # Variables to track averages
      sum_sensitivity <- 0
      sum_specificity <- 0
      sum_fnr <- 0
      sum_accuracy <- 0
      sum_balanced_accuracy <- 0
      n_constructs <- 0
      
      for (construct_name in names(full_perf$construct_metrics)) {
        cm <- full_perf$construct_metrics[[construct_name]]
        
        # Calculate balanced accuracy for the construct if not present
        if (is.null(cm$balanced_accuracy)) {
          cm$balanced_accuracy <- (cm$sensitivity + cm$specificity) / 2
        }
        
        html_content <- paste0(html_content,
                               "<tr>",
                               "<td class='construct-name'>", construct_name, "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$sensitivity), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$specificity), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$fnr), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$accuracy), "</td>",
                               "<td class='metric-value'>", sprintf("%.3f", cm$balanced_accuracy), "</td>",
                               "<td class='metric-value'>", sprintf("%.1f", method$reduction_pct), " (overall)</td>",
                               "</tr>"
        )
        
        # Update sums for averages
        sum_sensitivity <- sum_sensitivity + cm$sensitivity
        sum_specificity <- sum_specificity + cm$specificity
        sum_fnr <- sum_fnr + cm$fnr
        sum_accuracy <- sum_accuracy + cm$accuracy
        sum_balanced_accuracy <- sum_balanced_accuracy + cm$balanced_accuracy
        n_constructs <- n_constructs + 1
      }
      
      html_content <- paste0(html_content, "</table>")
      
      # Add summary statistics (including reduction percentage since it's the same for all)
      if (n_constructs > 0) {
        avg_sensitivity <- sum_sensitivity / n_constructs
        avg_specificity <- sum_specificity / n_constructs
        avg_fnr <- sum_fnr / n_constructs
        avg_accuracy <- sum_accuracy / n_constructs
        avg_balanced_accuracy <- sum_balanced_accuracy / n_constructs
        
        html_content <- paste0(html_content,
                               "<p><em>Construct-level averages: ",
                               "Sensitivity=", sprintf("%.3f", avg_sensitivity),
                               ", Specificity=", sprintf("%.3f", avg_specificity),
                               ", FNR=", sprintf("%.3f", avg_fnr),
                               ", Accuracy=", sprintf("%.3f", avg_accuracy),
                               ", Balanced Accuracy=", sprintf("%.3f", avg_balanced_accuracy),
                               ", Reduction=", sprintf("%.1f%%", method$reduction_pct), " (overall)",
                               "</em></p>"
        )
      }
    } else {
      html_content <- paste0(html_content,
                             "<p><em>No construct-level metrics available (unidimensional questionnaire)</em></p>"
      )
    }
    
    html_content <- paste0(html_content, "</div>")
  }
  
  html_content <- paste0(html_content, "</body></html>")
  
  writeLines(html_content, report_file)
  cat("  Generated construct-level performance report: construct_performance_report.html\n")
}

#' Generate Executive Summary
#'
#' @param evaluation_results Evaluation results
#' @param output_dir Output directory
generate_executive_summary <- function(evaluation_results, output_dir) {
  
  summary_file <- file.path(output_dir, "executive_summary.txt")
  
  # Get recommended method details
  rec_method <- evaluation_results$recommended_method
  full_perf <- evaluation_results$full_performance
  
  summary_text <- paste0(
    "EXECUTIVE SUMMARY\n",
    "=================\n\n",
    "Recommended Method: ", rec_method$method_id, "\n",
    "  - Ordering: ", rec_method$ordering, "\n",
    "  - Reduction: ", rec_method$reduction, "\n"
  )
  
  if (!is.na(rec_method$gamma_0)) {
    summary_text <- paste0(summary_text,
                           "  - Gamma parameters: γ₀=", rec_method$gamma_0, 
                           ", γ₁=", rec_method$gamma_1, "\n")
  }
  
  summary_text <- paste0(summary_text,
                         "\nKey Performance Metrics:\n",
                         "  - Sensitivity: ", sprintf("%.3f", rec_method$sensitivity), "\n",
                         "  - Specificity: ", sprintf("%.3f", rec_method$specificity), "\n",
                         "  - False Negative Rate: ", sprintf("%.3f", rec_method$fnr), "\n",
                         "  - Accuracy: ", sprintf("%.3f", rec_method$accuracy), "\n",
                         "  - Balanced Accuracy: ", sprintf("%.3f", rec_method$balanced_accuracy), "\n",
                         "  - Item Reduction: ", sprintf("%.1f%%", rec_method$reduction_pct), "\n",
                         "  - Mean Items Used: ", sprintf("%.1f", rec_method$mean_items_used), 
                         " (out of ", round(rec_method$mean_items_used / (1 - rec_method$reduction_pct/100)), ")\n",
                         "\nEvaluation Scores:\n",
                         "  - Composite Score: ", sprintf("%.3f", rec_method$composite_score))
  
  if (evaluation_results$evaluation_type == "two_step") {
    summary_text <- paste0(summary_text, " (used for screening)\n")
  } else {
    summary_text <- paste0(summary_text, "\n")
  }
  
  summary_text <- paste0(summary_text,
                         "  - Utility Score: ", sprintf("%.3f", rec_method$utility_score),
                         " (used for final selection)\n",
                         "\nClinical Implications:\n"
  )
  
  if (rec_method$fnr < 0.05) {
    summary_text <- paste0(summary_text,
                           "  - Excellent safety profile with false negative rate <5%\n")
  } else if (rec_method$fnr < 0.10) {
    summary_text <- paste0(summary_text,
                           "  - Good safety profile with false negative rate <10%\n")
  } else if (rec_method$fnr < 0.15) {
    summary_text <- paste0(summary_text,
                           "  - Moderate safety profile with false negative rate ≥10% and <15%\n")
  } else {
    summary_text <- paste0(summary_text,
                           "  - Questionable safety profile with false negative rate ≥15%\n")
  }
  
  if (rec_method$reduction_pct > 50) {
    summary_text <- paste0(summary_text,
                           "  - Substantial reduction in respondent burden (>50% fewer items)\n")
  } else if (rec_method$reduction_pct > 30) {
    summary_text <- paste0(summary_text,
                           "  - Moderate reduction in respondent burden\n")
  } else {
    summary_text <- paste0(summary_text,
                           "  - Modest reduction in respondent burden\n")
  }
  
  writeLines(summary_text, summary_file)
}

#' Generate Detailed Performance Report
#'
#' @param performance_matrix Performance matrix
#' @param evaluation_results Evaluation results
#' @param output_dir Output directory
generate_performance_report <- function(performance_matrix, evaluation_results, config, output_dir) {
  
  report_file <- file.path(output_dir, "performance_report.html")
  
  # Create HTML report
  html_content <- paste0(
    "<html><head><title>Curtailment Performance Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    ".recommended { background-color: #e6ffe6; font-weight: bold; }",
    ".pareto { background-color: #e6f3ff; }",
    ".note { background-color: #f0f0f0; padding: 10px; margin: 10px 0; border-left: 4px solid #333; }",
    "</style></head><body>",
    "<h1>Questionnaire Curtailment Performance Report</h1>",
    "<h2>Summary Statistics</h2>",
    "<p>Total methods evaluated: ", nrow(performance_matrix), "</p>",
    "<p>Pareto-optimal methods: ", 
    nrow(evaluation_results$detailed_results$pareto_optimal), "</p>"
  )
  
  # Add note about evaluation process
  if (evaluation_results$evaluation_type == "two_step") {
    html_content <- paste0(html_content,
                           "<div class='note'><strong>Note:</strong> Two-step evaluation was used. ",
                           "Methods are ranked by Composite Score for initial screening, ",
                           "then Utility Score is used for final selection among top candidates.</div>")
  } else {
    html_content <- paste0(html_content,
                           "<div class='note'><strong>Note:</strong> Single-step evaluation was used. ",
                           "Methods are ranked by Utility Score for direct selection.</div>")
  }
  
  # Add link to construct-level report
  html_content <- paste0(html_content,
                         "<p><strong>See also:</strong> <a href='construct_performance_report.html'>",
                         "Construct-Level Performance Report</a> for detailed analysis of top methods.</p>")
  
  # Add top performers table - heading based on evaluation type
  sorting_criterion <- ifelse(evaluation_results$evaluation_type == "two_step", 
                              "Composite Score", "Utility Score")
  html_content <- paste0(html_content,
                         "<h2>Top Performing Methods (by ", sorting_criterion, ")</h2>",
                         "<table>",
                         "<tr><th>Method</th><th>Ordering</th><th>Reduction</th>",
                         "<th>FNR</th><th>Accuracy</th><th>Balanced Accuracy</th><th>Reduction %</th>",
                         "<th>Composite Score</th><th>Utility Score</th></tr>"
  )
  
  # Get top methods by composite score (already sorted in performance_matrix)
  # top_n <- min(10, nrow(performance_matrix))
  # top_methods <- performance_matrix[1:top_n, ]
  # Fix: (Get number of top methods based on config)
  # Get top methods by composite score (already sorted in performance_matrix)
  top_n <- min(config$top_candidates, nrow(performance_matrix))
  top_methods <- performance_matrix[1:top_n, ]
  
  for (i in 1:nrow(top_methods)) {
    row_class <- ifelse(top_methods$method_id[i] == evaluation_results$recommended_method$method_id,
                        "recommended", 
                        ifelse(top_methods$method_id[i] %in% 
                                 evaluation_results$detailed_results$pareto_optimal$method_id,
                               "pareto", ""))
    html_content <- paste0(html_content,
                           "<tr class='", row_class, "'>",
                           "<td>", top_methods$method_id[i], "</td>",
                           "<td>", top_methods$ordering[i], "</td>",
                           "<td>", top_methods$reduction[i], "</td>",
                           "<td>", sprintf("%.3f", top_methods$fnr[i]), "</td>",
                           "<td>", sprintf("%.3f", top_methods$accuracy[i]), "</td>",
                           "<td>", sprintf("%.3f", top_methods$balanced_accuracy[i]), "</td>",
                           "<td>", sprintf("%.1f", top_methods$reduction_pct[i]), "</td>",
                           "<td>", sprintf("%.3f", top_methods$composite_score[i]), "</td>",
                           "<td>", sprintf("%.3f", top_methods$utility_score[i]), "</td>",
                           "</tr>"
    )
  }
  
  html_content <- paste0(html_content, "</table></body></html>")
  
  writeLines(html_content, report_file)
}

#' Generate Method Comparison Table
#'
#' @param performance_matrix Performance matrix
#' @param evaluation_results Evaluation results
#' @param output_dir Output directory
generate_comparison_table <- function(performance_matrix, evaluation_results, output_dir) {
  
  # Create full comparison table based on performance_matrix
  full_comparison <- performance_matrix
  
  # Add primary rank column based on evaluation type
  if (evaluation_results$evaluation_type == "two_step") {
    full_comparison$primary_rank <- full_comparison$composite_rank
  } else {
    full_comparison$primary_rank <- full_comparison$utility_rank
  }
  
  # Reorder columns to put primary_rank first among ranks
  col_order <- c("method_id", "ordering", "reduction", "gamma_0", "gamma_1",
                 "sensitivity", "specificity", "fnr", "fpr", "accuracy",
                 "balanced_accuracy", "ppv", "npv", "mean_items_used",
                 "median_items_used", "sd_items_used", "reduction_pct",
                 "composite_score", "utility_score", "primary_rank",
                 "composite_rank", "utility_rank", "fnr_rank", "accuracy_rank",
                 "balanced_accuracy_rank", "efficiency_rank")
  full_comparison <- full_comparison[, col_order]
  
  # Save full comparison table (already has all rankings and is sorted by appropriate score)
  write.csv(full_comparison, 
            file.path(output_dir, "method_comparison_full.csv"), 
            row.names = FALSE)
  
  # Create summary comparison focused on key metrics
  summary_comparison <- full_comparison[, c("method_id", "ordering", "reduction", 
                                            "sensitivity", "specificity", "fnr", "accuracy", "balanced_accuracy",
                                            "mean_items_used", "reduction_pct", "composite_score", "utility_score",
                                            "primary_rank", "composite_rank", "utility_rank"
  )]
  
  # Round numeric columns
  numeric_cols <- c("sensitivity", "specificity", "fnr", "accuracy", "balanced_accuracy",
                    "mean_items_used", "reduction_pct", "composite_score", "utility_score")
  summary_comparison[numeric_cols] <- lapply(summary_comparison[numeric_cols], 
                                             function(x) round(x, 3))
  
  write.csv(summary_comparison, 
            file.path(output_dir, "method_comparison_summary.csv"), 
            row.names = FALSE)
}

#' Generate Implementation Guide
#'
#' @param evaluation_results Evaluation results
#' @param config Configuration
#' @param output_dir Output directory
generate_implementation_guide <- function(evaluation_results, config, output_dir) {
  
  guide_file <- file.path(output_dir, "implementation_guide.txt")
  
  rec_method <- evaluation_results$recommended_method
  full_perf <- evaluation_results$full_performance
  
  guide_text <- paste0(
    "IMPLEMENTATION GUIDE\n",
    "===================\n\n",
    "Recommended Method: ", rec_method$method_id, "\n\n",
    "1. ITEM ORDERING\n",
    "   Method: ", rec_method$ordering, "\n"
  )
  
  # Add ordering-specific guidance
  if (rec_method$ordering == "auc") {
    guide_text <- paste0(guide_text,
                         "   - Items are ordered by individual AUC values\n",
                         "   - Administer items in the specified order\n")
  } else if (rec_method$ordering == "incremental_auc") {
    guide_text <- paste0(guide_text,
                         "   - Items ordered by incremental predictive value\n",
                         "   - Each item adds maximum information given previous items\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n2. STOPPING RULES\n",
                       "   Method: ", rec_method$reduction, "\n"
  )
  
  # Add reduction-specific guidance
  if (rec_method$reduction == "dc") {
    guide_text <- paste0(guide_text,
                         "   - Stop when classification cannot change\n",
                         "   - Calculate bounds on possible total score\n")
  } else if (grepl("^sc_", rec_method$reduction)) {
    guide_text <- paste0(guide_text,
                         "   - Stop when probability threshold is reached\n",
                         "   - γ₀ = ", rec_method$gamma_0, " (low-risk threshold)\n",
                         "   - γ₁ = ", rec_method$gamma_1, " (high-risk threshold)\n")
  }
  
  # Fix:/Update: (Added base rate section)
  guide_text <- paste0(guide_text,
                       "\n3. BASE RATE CONSIDERATIONS\n"
  )
  
  if (!is.null(full_perf$base_rate_adjusted) && full_perf$base_rate_adjusted) {
    guide_text <- paste0(guide_text,
                         "   - Reference base rate(s) provided\n",
                         "   - Probability calibration applied\n")
  } else {
    guide_text <- paste0(guide_text,
                         "   - Base rate(s) from training data\n",
                         "   - No probability calibration applied\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n4. DEPLOYMENT REQUIREMENTS\n",
                       "   - Software: R with required packages\n",
                       "   - Training data: Maintain for model updates\n",
                       "   - Monitoring: Track real-world performance\n",
                       "\n5. SAFETY CONSIDERATIONS\n"
  )
  
  if (config$constraints$stop_low_only) {
    guide_text <- paste0(guide_text,
                         "   - Stop-low only mode is active\n",
                         "   - Full assessment required for potential high-risk cases\n")
  }
  
  guide_text <- paste0(guide_text,
                       "   - Monitor false negative rate in practice\n",
                       "   - Consider full assessment if uncertainty is high\n",
                       "   - Allow clinical override when necessary\n"
  )
  
  writeLines(guide_text, guide_file)
}

#' Generate Evaluation Metadata
#'
#' @param evaluation_results Evaluation results
#' @param config Configuration
#' @param output_dir Output directory
generate_evaluation_metadata <- function(evaluation_results, config, output_dir) {
  
  metadata_file <- file.path(output_dir, "evaluation_metadata.txt")
  
  metadata_text <- paste0(
    "EVALUATION METADATA\n",
    "==================\n\n",
    "Evaluation Type: ", evaluation_results$evaluation_type, "\n",
    "Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
  )
  
  if (evaluation_results$evaluation_type == "two_step") {
    metadata_text <- paste0(metadata_text,
                            "TWO-STEP EVALUATION PROCESS\n",
                            "---------------------------\n",
                            "Step 1: Screening Phase\n",
                            "  - All methods ranked by Composite Score\n",
                            "  - Composite Score = ",
                            sprintf("%.1f", config$screening_weights$w1_fnr * 100), "% × (1-FNR) + ",
                            sprintf("%.1f", config$screening_weights$w2_accuracy * 100), "% × Balanced Accuracy + ",
                            sprintf("%.1f", config$screening_weights$w3_efficiency * 100), "% × Efficiency\n",
                            "  - Top ", config$top_candidates, " methods selected for detailed analysis\n\n",
                            "Step 2: Detailed Analysis\n",
                            "  - Top candidates evaluated using Utility Score\n",
                            "  - Utility Score = ",
                            sprintf("%.1f", config$utility_weights$alpha_fnr * 100), "% × (1-FNR) + ",
                            sprintf("%.1f", config$utility_weights$beta_accuracy * 100), "% × Balanced Accuracy + ",
                            sprintf("%.1f", config$utility_weights$gamma_efficiency * 100), "% × Efficiency\n",
                            "  - Best method selected from Pareto-optimal set\n\n",
                            "OUTPUT SORTING\n",
                            "--------------\n",
                            "All output files are sorted by Composite Score (descending)\n",
                            "This reflects the initial screening criterion\n")
  } else {
    metadata_text <- paste0(metadata_text,
                            "SINGLE-STEP EVALUATION PROCESS\n",
                            "------------------------------\n",
                            "  - All methods evaluated using Utility Score\n",
                            "  - Utility Score = ",
                            sprintf("%.1f", config$utility_weights$alpha_fnr * 100), "% × (1-FNR) + ",
                            sprintf("%.1f", config$utility_weights$beta_accuracy * 100), "% × Balanced Accuracy + ",
                            sprintf("%.1f", config$utility_weights$gamma_efficiency * 100), "% × Efficiency\n",
                            "  - Best method selected from Pareto-optimal set\n\n",
                            "OUTPUT SORTING\n",
                            "--------------\n",
                            "All output files are sorted by Utility Score (descending)\n",
                            "This reflects the selection criterion\n")
  }
  
  metadata_text <- paste0(metadata_text,
                          "\nFILE DESCRIPTIONS\n",
                          "-----------------\n",
                          "- performance_matrix.csv: All methods with performance metrics, scores, and rankings\n",
                          "- method_comparison_full.csv: Same as performance_matrix.csv\n",
                          "- method_comparison_summary.csv: Key metrics with all ranking columns\n",
                          "- performance_report.html: Top methods with visual formatting\n",
                          "- construct_performance_report.html: Construct-level metrics for top methods\n",
                          "- executive_summary.txt: Recommended method and key findings\n",
                          "- implementation_guide.txt: Deployment instructions\n",
                          "- evaluation_metadata.txt: This file\n")
  
  writeLines(metadata_text, metadata_file)
}

#' Print Evaluation Summary
#'
#' @param evaluation_results Evaluation results object
#' @param ... Additional arguments (not used)
#' @export
print.evaluation_results <- function(evaluation_results, ...) {
  cat("========================================\n")
  cat("Curtailment Method Evaluation Results\n")
  cat("========================================\n\n")
  
  cat("Evaluation Type:", evaluation_results$evaluation_type, "\n")
  cat("Total Methods Evaluated:", nrow(evaluation_results$performance_matrix), "\n")
  
  if (evaluation_results$evaluation_type == "two_step") {
    cat("Top Candidates Selected:", 
        nrow(evaluation_results$screening_results$top_candidates), "\n")
  }
  
  cat("Pareto-Optimal Methods:", 
      nrow(evaluation_results$detailed_results$pareto_optimal), "\n\n")
  
  cat("RECOMMENDED METHOD\n")
  cat("------------------\n")
  rec <- evaluation_results$recommended_method
  cat("Method ID:", rec$method_id, "\n")
  cat("Ordering:", rec$ordering, "\n")
  cat("Reduction:", rec$reduction, "\n")
  if (!is.na(rec$gamma_0)) {
    cat("Parameters: γ₀ =", rec$gamma_0, ", γ₁ =", rec$gamma_1, "\n")
  }
  cat("\nPerformance:\n")
  cat("  FNR:", sprintf("%.3f", rec$fnr), "\n")
  cat("  Sensitivity:", sprintf("%.3f", rec$sensitivity), "\n")
  cat("  Specificity:", sprintf("%.3f", rec$specificity), "\n")
  cat("  Accuracy:", sprintf("%.3f", rec$accuracy), "\n")
  cat("  Balanced Accuracy:", sprintf("%.3f", rec$balanced_accuracy), "\n")
  cat("  Item Reduction:", sprintf("%.1f%%", rec$reduction_pct), "\n")
  cat("  Mean Items Used:", sprintf("%.1f", rec$mean_items_used), "\n")
  cat("\nScores:\n")
  cat("  Composite Score:", sprintf("%.3f", rec$composite_score))
  if (evaluation_results$evaluation_type == "two_step") {
    cat(" (used for screening)\n")
  } else {
    cat("\n")
  }
  cat("  Utility Score:", sprintf("%.3f", rec$utility_score), " (used for final selection)\n")
  
  invisible(evaluation_results)
}

# ============================================================================
# Enhanced Performance Calculation Helper Functions
# ============================================================================

#' Calculate Full Reduction Performance (with construct metrics)
#'
#' @param reduction_results Reduction results from Module 4
#' @param prepared_data Prepared data from Module 1
#' @return List with overall and construct-level performance metrics
calculate_reduction_performance_full <- function(reduction_results, prepared_data) {
  
  # First calculate overall performance using existing function
  overall_perf <- calculate_reduction_performance(reduction_results, prepared_data)
  
  # For multi-construct, ensure we have detailed construct metrics and reduction info
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    
    # Calculate construct-specific reduction information
    construct_reduction_info <- list()
    
    # Check if we have items_administered matrix
    if (!is.null(reduction_results$items_administered)) {
      # Extract items administered matrix
      items_administered <- reduction_results$items_administered
      
      # Get column names (ordered items)
      if (!is.null(colnames(items_administered))) {
        ordered_items <- colnames(items_administered)
      } else {
        # Try to get from reduction_results
        if (!is.null(reduction_results$ordered_items)) {
          ordered_items <- reduction_results$ordered_items
          colnames(items_administered) <- ordered_items
        } else {
          # Cannot determine item names
          warning("Cannot determine item names for construct-specific reduction calculation")
          ordered_items <- NULL
        }
      }
      
      if (!is.null(ordered_items)) {
        # Calculate reduction for each construct
        for (construct_name in names(prepared_data$config$constructs)) {
          # Get items for this construct
          construct_items <- prepared_data$config$constructs[[construct_name]]
          
          # Find which columns correspond to this construct's items
          construct_item_indices <- which(ordered_items %in% construct_items)
          
          if (length(construct_item_indices) > 0) {
            # Extract items administered for this construct
            construct_items_administered <- items_administered[, construct_item_indices, drop = FALSE]
            
            # Calculate metrics
            n_items_total_construct <- length(construct_items)
            n_items_used_construct <- rowSums(construct_items_administered)
            mean_items_used_construct <- mean(n_items_used_construct)
            reduction_pct_construct <- 100 * (1 - mean_items_used_construct / n_items_total_construct)
            
            construct_reduction_info[[construct_name]] <- list(
              n_items_total = n_items_total_construct,
              mean_items_used = mean_items_used_construct,
              median_items_used = median(n_items_used_construct),
              sd_items_used = sd(n_items_used_construct),
              reduction_pct = reduction_pct_construct,
              items_by_person = n_items_used_construct
            )
          } else {
            # No items found for this construct
            construct_reduction_info[[construct_name]] <- list(
              n_items_total = length(construct_items),
              mean_items_used = NA,
              median_items_used = NA,
              sd_items_used = NA,
              reduction_pct = NA,
              items_by_person = NA
            )
          }
        }
        
        overall_perf$construct_reduction_info <- construct_reduction_info
      }
    }
    
    # Ensure construct metrics are calculated if not present
    if (is.null(overall_perf$construct_metrics)) {
      construct_metrics <- list()
      
      for (construct_name in names(prepared_data$config$constructs)) {
        # Get true outcomes for this construct
        outcome_col <- paste0(construct_name, "_outcome")
        true_outcomes <- prepared_data$splits$test[[outcome_col]]
        
        # Get predicted outcomes for this construct
        if (!is.null(reduction_results$construct_classifications)) {
          pred_outcomes <- reduction_results$construct_classifications[[construct_name]]
        } else {
          # Fallback if method doesn't provide construct-specific classifications
          pred_outcomes <- reduction_results$classifications
        }
        
        # Calculate metrics for this construct
        tp <- sum(true_outcomes == 1 & pred_outcomes == 1, na.rm = TRUE)
        tn <- sum(true_outcomes == 0 & pred_outcomes == 0, na.rm = TRUE)
        fp <- sum(true_outcomes == 0 & pred_outcomes == 1, na.rm = TRUE)
        fn <- sum(true_outcomes == 1 & pred_outcomes == 0, na.rm = TRUE)
        total <- tp + tn + fp + fn
        
        if (total > 0) {
          sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA
          specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA
          
          construct_metrics[[construct_name]] <- list(
            tp = tp, tn = tn, fp = fp, fn = fn,
            sensitivity = sensitivity,
            specificity = specificity,
            fnr = if ((tp + fn) > 0) fn / (tp + fn) else NA,
            fpr = if ((tn + fp) > 0) fp / (tn + fp) else NA,
            accuracy = (tp + tn) / total,
            balanced_accuracy = if (!is.na(sensitivity) && !is.na(specificity)) {
              (sensitivity + specificity) / 2
            } else NA,
            ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA,
            npv = if ((tn + fn) > 0) tn / (tn + fn) else NA
          )
        }
      }
      
      overall_perf$construct_metrics <- construct_metrics
    }
  }
  
  return(overall_perf)
}

#' #' Calculate Reduction Performance (Original)
#' #'
#' #' @param reduction_results Reduction results from Module 4
#' #' @param prepared_data Prepared data from Module 1
#' #' @return List of performance metrics
#' calculate_reduction_performance <- function(reduction_results, prepared_data) {
#'   if (prepared_data$config$questionnaire_type == "multi-construct") {
#'     # For multi-construct: evaluate each construct separately then aggregate
#'     construct_metrics <- list()
#'     for (construct_name in names(prepared_data$config$constructs)) {
#'       # Get true outcomes for this construct
#'       outcome_col <- paste0(construct_name, "_outcome")
#'       true_outcomes <- prepared_data$splits$test[[outcome_col]]
#'       # Get predicted outcomes for this construct
#'       if (!is.null(reduction_results$construct_classifications)) {
#'         pred_outcomes <- reduction_results$construct_classifications[[construct_name]]
#'       } else {
#'         # Fallback if method doesn't provide construct-specific classifications
#'         pred_outcomes <- reduction_results$classifications
#'       }
#'       # Calculate metrics for this construct
#'       tp <- sum(true_outcomes == 1 & pred_outcomes == 1, na.rm = TRUE)
#'       tn <- sum(true_outcomes == 0 & pred_outcomes == 0, na.rm = TRUE)
#'       fp <- sum(true_outcomes == 0 & pred_outcomes == 1, na.rm = TRUE)
#'       fn <- sum(true_outcomes == 1 & pred_outcomes == 0, na.rm = TRUE)
#'       total <- tp + tn + fp + fn
#'       if (total > 0) {
#'         sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA
#'         specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA
#'         
#'         construct_metrics[[construct_name]] <- list(
#'           tp = tp, tn = tn, fp = fp, fn = fn,
#'           sensitivity = sensitivity,
#'           specificity = specificity,
#'           fnr = if ((tp + fn) > 0) fn / (tp + fn) else NA,
#'           fpr = if ((tn + fp) > 0) fp / (tn + fp) else NA,
#'           accuracy = (tp + tn) / total,
#'           balanced_accuracy = if (!is.na(sensitivity) && !is.na(specificity)) {
#'             (sensitivity + specificity) / 2
#'           } else NA,
#'           ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA,
#'           npv = if ((tn + fn) > 0) tn / (tn + fn) else NA
#'         )
#'       }
#'     }
#'     # Aggregate metrics across constructs
#'     total_tp <- sum(sapply(construct_metrics, function(x) x$tp))
#'     total_tn <- sum(sapply(construct_metrics, function(x) x$tn))
#'     total_fp <- sum(sapply(construct_metrics, function(x) x$fp))
#'     total_fn <- sum(sapply(construct_metrics, function(x) x$fn))
#'     total <- total_tp + total_tn + total_fp + total_fn
#'     sensitivity <- if ((total_tp + total_fn) > 0) total_tp / (total_tp + total_fn) else NA
#'     specificity <- if ((total_tn + total_fp) > 0) total_tn / (total_tn + total_fp) else NA
#'     fnr <- 1 - sensitivity
#'     fpr <- 1 - specificity
#'     accuracy <- if (total > 0) (total_tp + total_tn) / total else NA
#'     balanced_accuracy <- if (!is.na(sensitivity) && !is.na(specificity)) {
#'       (sensitivity + specificity) / 2
#'     } else NA
#'     ppv <- if ((total_tp + total_fp) > 0) total_tp / (total_tp + total_fp) else NA
#'     npv <- if ((total_tn + total_fn) > 0) total_tn / (total_tn + total_fn) else NA
#'   } else {
#'     # For unidimensional: simple comparison
#'     outcome_col <- prepared_data$config$outcome_column
#'     true_outcomes <- prepared_data$splits$test[[outcome_col]]
#'     pred_outcomes <- reduction_results$classifications
#'     tp <- sum(true_outcomes == 1 & pred_outcomes == 1, na.rm = TRUE)
#'     tn <- sum(true_outcomes == 0 & pred_outcomes == 0, na.rm = TRUE)
#'     fp <- sum(true_outcomes == 0 & pred_outcomes == 1, na.rm = TRUE)
#'     fn <- sum(true_outcomes == 1 & pred_outcomes == 0, na.rm = TRUE)
#'     total <- tp + tn + fp + fn
#'     sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA
#'     specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA
#'     fnr <- 1 - sensitivity
#'     fpr <- 1 - specificity
#'     accuracy <- if (total > 0) (tp + tn) / total else NA
#'     balanced_accuracy <- if (!is.na(sensitivity) && !is.na(specificity)) {
#'       (sensitivity + specificity) / 2
#'     } else NA
#'     ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA
#'     npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA
#'   }
#'   # Efficiency metrics
#'   n_items_total <- ncol(reduction_results$items_administered)
#'   mean_items_used <- mean(reduction_results$n_items_used)
#'   median_items_used <- median(reduction_results$n_items_used)
#'   sd_items_used <- sd(reduction_results$n_items_used)
#'   reduction_pct <- 100 * (1 - mean_items_used / n_items_total)
#'   # Items used by outcome
#'   items_by_outcome <- list()
#'   if (prepared_data$config$questionnaire_type == "multi-construct") {
#'     # Any positive outcome
#'     any_positive <- apply(do.call(cbind, lapply(names(prepared_data$config$constructs),
#'                                                 function(c) prepared_data$splits$test[[paste0(c, "_outcome")]])),
#'                           1, function(x) any(x == 1, na.rm = TRUE))
#'     items_by_outcome$positive <- reduction_results$n_items_used[any_positive]
#'     items_by_outcome$negative <- reduction_results$n_items_used[!any_positive]
#'   } else {
#'     outcome_col <- prepared_data$config$outcome_column
#'     outcomes <- prepared_data$splits$test[[outcome_col]]
#'     items_by_outcome$positive <- reduction_results$n_items_used[outcomes == 1]
#'     items_by_outcome$negative <- reduction_results$n_items_used[outcomes == 0]
#'   }
#'   return(list(
#'     # Confusion matrix
#'     confusion_matrix = matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE,
#'                               dimnames = list(c("Pred_High", "Pred_Low"),
#'                                               c("True_High", "True_Low"))),
#'     # Classification metrics
#'     sensitivity = sensitivity,
#'     specificity = specificity,
#'     fnr = fnr,
#'     fpr = fpr,
#'     accuracy = accuracy,
#'     balanced_accuracy = balanced_accuracy,
#'     ppv = ppv,
#'     npv = npv,
#'     # Efficiency metrics
#'     mean_items_used = mean_items_used,
#'     median_items_used = median_items_used,
#'     sd_items_used = sd_items_used,
#'     reduction_pct = reduction_pct,
#'     items_by_outcome = items_by_outcome,
#'     # Sample size
#'     n_total = total,
#'     # Construct-specific metrics (if applicable)
#'     construct_metrics = if (exists("construct_metrics")) construct_metrics else NULL,
#'     # Fix:/Update: (Added base rate reporting to performance calculation)
#'     # Base rate information
#'     sample_base_rate = if (prepared_data$config$questionnaire_type == "multi-construct") {
#'       mean(unlist(prepared_data$config$sample_base_rates))
#'     } else {
#'       prepared_data$config$sample_base_rates$total
#'     },
#'     reference_base_rate = if (prepared_data$config$questionnaire_type == "multi-construct") {
#'       mean(unlist(prepared_data$config$reference_base_rates))
#'     } else {
#'       prepared_data$config$reference_base_rates$total
#'     },
#'     base_rate_adjusted = !isTRUE(all.equal(
#'       prepared_data$config$sample_base_rates,
#'       prepared_data$config$reference_base_rates
#'     ))
#'   ))
#' }

# Fix: (Updated version of calculate_reduction_performance function to handle edge cases when some constructs might have NULL or missing metrics)
#' Calculate Reduction Performance (Original) (Fixed)
#'
#' @param reduction_results Reduction results from Module 4
#' @param prepared_data Prepared data from Module 1
#' @return List of performance metrics
calculate_reduction_performance <- function(reduction_results, prepared_data) {
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    # For multi-construct: evaluate each construct separately then aggregate
    construct_metrics <- list()
    
    # Initialize aggregation variables
    total_tp <- 0
    total_tn <- 0
    total_fp <- 0
    total_fn <- 0
    valid_constructs <- 0
    
    for (construct_name in names(prepared_data$config$constructs)) {
      # Get true outcomes for this construct
      outcome_col <- paste0(construct_name, "_outcome")
      true_outcomes <- prepared_data$splits$test[[outcome_col]]
      
      # Get predicted outcomes for this construct
      if (!is.null(reduction_results$construct_classifications) && 
          construct_name %in% names(reduction_results$construct_classifications)) {
        pred_outcomes <- reduction_results$construct_classifications[[construct_name]]
      } else {
        # Fallback if method doesn't provide construct-specific classifications
        pred_outcomes <- reduction_results$classifications
      }
      
      # Skip if no valid predictions
      if (is.null(pred_outcomes) || all(is.na(pred_outcomes))) {
        construct_metrics[[construct_name]] <- list(
          tp = 0, tn = 0, fp = 0, fn = 0,
          sensitivity = NA, specificity = NA,
          fnr = NA, fpr = NA, accuracy = NA,
          balanced_accuracy = NA, ppv = NA, npv = NA,
          valid = FALSE
        )
        next
      }
      
      # Calculate metrics for this construct
      tp <- sum(true_outcomes == 1 & pred_outcomes == 1, na.rm = TRUE)
      tn <- sum(true_outcomes == 0 & pred_outcomes == 0, na.rm = TRUE)
      fp <- sum(true_outcomes == 0 & pred_outcomes == 1, na.rm = TRUE)
      fn <- sum(true_outcomes == 1 & pred_outcomes == 0, na.rm = TRUE)
      total <- tp + tn + fp + fn
      
      if (total > 0) {
        sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
        specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
        
        construct_metrics[[construct_name]] <- list(
          tp = tp, tn = tn, fp = fp, fn = fn,
          sensitivity = sensitivity,
          specificity = specificity,
          fnr = if ((tp + fn) > 0) fn / (tp + fn) else 1,
          fpr = if ((tn + fp) > 0) fp / (tn + fp) else 0,
          accuracy = (tp + tn) / total,
          balanced_accuracy = (sensitivity + specificity) / 2,
          ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA,
          npv = if ((tn + fn) > 0) tn / (tn + fn) else NA,
          valid = TRUE
        )
        
        # Add to totals for aggregation
        total_tp <- total_tp + tp
        total_tn <- total_tn + tn
        total_fp <- total_fp + fp
        total_fn <- total_fn + fn
        valid_constructs <- valid_constructs + 1
      } else {
        construct_metrics[[construct_name]] <- list(
          tp = 0, tn = 0, fp = 0, fn = 0,
          sensitivity = NA, specificity = NA,
          fnr = NA, fpr = NA, accuracy = NA,
          balanced_accuracy = NA, ppv = NA, npv = NA,
          valid = FALSE
        )
      }
    }
    
    # Calculate aggregate metrics
    total <- total_tp + total_tn + total_fp + total_fn
    
    if (total > 0 && valid_constructs > 0) {
      sensitivity <- if ((total_tp + total_fn) > 0) total_tp / (total_tp + total_fn) else 0
      specificity <- if ((total_tn + total_fp) > 0) total_tn / (total_tn + total_fp) else 0
      fnr <- 1 - sensitivity
      fpr <- 1 - specificity
      accuracy <- (total_tp + total_tn) / total
      balanced_accuracy <- (sensitivity + specificity) / 2
      ppv <- if ((total_tp + total_fp) > 0) total_tp / (total_tp + total_fp) else NA
      npv <- if ((total_tn + total_fn) > 0) total_tn / (total_tn + total_fn) else NA
    } else {
      # No valid predictions across any constructs
      sensitivity <- NA
      specificity <- NA
      fnr <- NA
      fpr <- NA
      accuracy <- NA
      balanced_accuracy <- NA
      ppv <- NA
      npv <- NA
    }
  } else {
    # For unidimensional: simple comparison
    outcome_col <- prepared_data$config$outcome_column
    true_outcomes <- prepared_data$splits$test[[outcome_col]]
    pred_outcomes <- reduction_results$classifications
    
    # Check for valid predictions
    if (is.null(pred_outcomes) || all(is.na(pred_outcomes))) {
      return(list(
        confusion_matrix = matrix(0, nrow = 2, ncol = 2,
                                  dimnames = list(c("Pred_High", "Pred_Low"),
                                                  c("True_High", "True_Low"))),
        sensitivity = NA, specificity = NA,
        fnr = NA, fpr = NA, accuracy = NA,
        balanced_accuracy = NA, ppv = NA, npv = NA,
        mean_items_used = mean(reduction_results$n_items_used, na.rm = TRUE),
        median_items_used = median(reduction_results$n_items_used, na.rm = TRUE),
        sd_items_used = sd(reduction_results$n_items_used, na.rm = TRUE),
        reduction_pct = 0,
        items_by_outcome = list(positive = numeric(0), negative = numeric(0)),
        n_total = 0,
        construct_metrics = NULL
      ))
    }
    
    tp <- sum(true_outcomes == 1 & pred_outcomes == 1, na.rm = TRUE)
    tn <- sum(true_outcomes == 0 & pred_outcomes == 0, na.rm = TRUE)
    fp <- sum(true_outcomes == 0 & pred_outcomes == 1, na.rm = TRUE)
    fn <- sum(true_outcomes == 1 & pred_outcomes == 0, na.rm = TRUE)
    total <- tp + tn + fp + fn
    
    sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
    fnr <- 1 - sensitivity
    fpr <- 1 - specificity
    accuracy <- if (total > 0) (tp + tn) / total else 0
    balanced_accuracy <- (sensitivity + specificity) / 2
    ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA
    npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA
  }
  
  # Efficiency metrics
  n_items_total <- ncol(reduction_results$items_administered)
  mean_items_used <- mean(reduction_results$n_items_used, na.rm = TRUE)
  median_items_used <- median(reduction_results$n_items_used, na.rm = TRUE)
  sd_items_used <- sd(reduction_results$n_items_used, na.rm = TRUE)
  reduction_pct <- 100 * (1 - mean_items_used / n_items_total)
  
  # Items used by outcome
  items_by_outcome <- list()
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    # Any positive outcome
    any_positive <- tryCatch({
      outcome_matrix <- do.call(cbind, lapply(names(prepared_data$config$constructs),
                                              function(c) {
                                                col_name <- paste0(c, "_outcome")
                                                if (col_name %in% names(prepared_data$splits$test)) {
                                                  prepared_data$splits$test[[col_name]]
                                                } else {
                                                  rep(NA, nrow(prepared_data$splits$test))
                                                }
                                              }))
      apply(outcome_matrix, 1, function(x) any(x == 1, na.rm = TRUE))
    }, error = function(e) {
      rep(FALSE, length(reduction_results$n_items_used))
    })
    
    items_by_outcome$positive <- reduction_results$n_items_used[any_positive]
    items_by_outcome$negative <- reduction_results$n_items_used[!any_positive]
  } else {
    outcome_col <- prepared_data$config$outcome_column
    outcomes <- prepared_data$splits$test[[outcome_col]]
    items_by_outcome$positive <- reduction_results$n_items_used[outcomes == 1]
    items_by_outcome$negative <- reduction_results$n_items_used[outcomes == 0]
  }
  
  # Create confusion matrix
  if (!is.na(total) && total > 0) {
    confusion_matrix <- matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE,
                               dimnames = list(c("Pred_High", "Pred_Low"),
                                               c("True_High", "True_Low")))
  } else {
    confusion_matrix <- matrix(0, nrow = 2, ncol = 2,
                               dimnames = list(c("Pred_High", "Pred_Low"),
                                               c("True_High", "True_Low")))
  }
  
  return(list(
    # Confusion matrix
    confusion_matrix = confusion_matrix,
    # Classification metrics
    sensitivity = sensitivity,
    specificity = specificity,
    fnr = fnr,
    fpr = fpr,
    accuracy = accuracy,
    balanced_accuracy = balanced_accuracy,
    ppv = ppv,
    npv = npv,
    # Efficiency metrics
    mean_items_used = mean_items_used,
    median_items_used = median_items_used,
    sd_items_used = sd_items_used,
    reduction_pct = reduction_pct,
    items_by_outcome = items_by_outcome,
    # Sample size
    n_total = if(!is.na(total)) total else 0,
    # Construct-specific metrics (if applicable)
    construct_metrics = if (exists("construct_metrics")) construct_metrics else NULL,
    # Base rate information
    sample_base_rate = if (prepared_data$config$questionnaire_type == "multi-construct") {
      mean(unlist(prepared_data$config$sample_base_rates), na.rm = TRUE)
    } else {
      prepared_data$config$sample_base_rates$total
    },
    reference_base_rate = if (prepared_data$config$questionnaire_type == "multi-construct") {
      mean(unlist(prepared_data$config$reference_base_rates), na.rm = TRUE)
    } else {
      prepared_data$config$reference_base_rates$total
    },
    base_rate_adjusted = !isTRUE(all.equal(
      prepared_data$config$sample_base_rates,
      prepared_data$config$reference_base_rates
    ))
  ))
}

# ============================================================================
# Additional Helper Functions
# ============================================================================

#' Compare Two Methods Head-to-Head
#'
#' @param method1_id First method ID
#' @param method2_id Second method ID
#' @param all_results All combination results
#' @param prepared_data Prepared data
#' @return Comparison results
#' @export
compare_methods <- function(method1_id, method2_id, all_results, prepared_data) {
  
  # Get results for both methods
  result1 <- all_results[[method1_id]]
  result2 <- all_results[[method2_id]]
  
  if (is.null(result1) || "error" %in% names(result1)) {
    stop(paste("Method", method1_id, "not found or had errors"))
  }
  if (is.null(result2) || "error" %in% names(result2)) {
    stop(paste("Method", method2_id, "not found or had errors"))
  }
  
  # Calculate performance for both
  perf1 <- calculate_reduction_performance(result1$reduction_result, prepared_data)
  perf2 <- calculate_reduction_performance(result2$reduction_result, prepared_data)
  
  # Compare classifications
  class1 <- result1$reduction_result$classifications
  class2 <- result2$reduction_result$classifications
  
  # McNemar's test for paired comparison
  contingency_table <- table(class1, class2)
  mcnemar_result <- mcnemar.test(contingency_table)
  
  # Create comparison summary
  comparison <- list(
    method1 = list(
      id = method1_id,
      performance = perf1,
      mean_items = perf1$mean_items_used
    ),
    method2 = list(
      id = method2_id,
      performance = perf2,
      mean_items = perf2$mean_items_used
    ),
    agreement = sum(class1 == class2) / length(class1),
    mcnemar_test = mcnemar_result,
    contingency_table = contingency_table
  )
  
  return(comparison)
}

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}