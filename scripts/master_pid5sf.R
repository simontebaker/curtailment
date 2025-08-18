# ============================================================================
# Set Directory Paths and Input Data File Name
# ============================================================================

# Set code directory (where this script and module scripts are located)
code_dir <- "/Users/simonbaker/GitHub/sandbox/R"

# Set data directory (where the data file is located)
data_dir <- "/Users/simonbaker/GitHub/sandbox/R/data_for_curtailment"

# Set data file name
data_file <- "pid-5-sf_data_for_curtailment_84_13.csv"

# ============================================================================
# Initial Setup (Do Not Edit)
# ============================================================================

# Set data file path
data_path <- file.path(data_dir, data_file)

# Create datetime for this run
run_timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M")
run_name <- paste0("pid5sf_curtailment_", run_timestamp) # "curtailment_"

# Create output directory for this run
output_base_dir <- file.path(code_dir, "pid5sf_curtailment_output") # "curtailment_output"
if (!dir.exists(output_base_dir)) {
  dir.create(output_base_dir, recursive = TRUE)
}
run_dir <- file.path(output_base_dir, run_name)
dir.create(run_dir, recursive = TRUE)

# Set working directory to the run directory
setwd(run_dir)

# Create a log file for this run
log_file <- file.path(run_dir, "run_log.txt")
cat("Run timestamp:", run_timestamp, "\n", file = log_file)
cat("Code directory:", code_dir, "\n", file = log_file, append = TRUE)
cat("Data file path:", data_path, "\n", file = log_file, append = TRUE)
cat("Output directory:", run_dir, "\n", file = log_file, append = TRUE)
start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat("Run started at:", start_time, "\n", file = log_file, append = TRUE)

# # Open a file connection
# file_con <- file("console_output.txt", open = "wt")
# # Redirect both output streams (standard output and messages/warnings) to a file (and also show standard output in the console)
# sink(file_con, split = TRUE) # Standard output
# sink(file_con, type = "message") # Messages/warnings
# # Close both sinks and file connection when script exits
# on.exit({
#   sink(type = "message") # Close message/warning sink first
#   sink() # Close standard output sink
#   close(file_con) # Close file connection
# }, add = TRUE)

# ============================================================================
# Load Modules
# ============================================================================

cat("Loading modules...\n")
source(file.path(code_dir, "module_1_data_preparation.R"))
source(file.path(code_dir, "module_2_configuration.R"))
source(file.path(code_dir, "module_3_item_ordering.R"))
source(file.path(code_dir, "module_4_item_reduction.R"))
source(file.path(code_dir, "module_5_evaluation.R"))
source(file.path(code_dir, "module_6_optimisation.R"))
source(file.path(code_dir, "module_7_deployment.R"))
cat("Modules loaded successfully.\n")

# ============================================================================
# Module 1: Data Preparation Module
# ============================================================================
# Purpose: Validate input data, check structure, and create train/test splits
# ============================================================================

cat("\n=========================================\n")
cat("=== Module 1: Data Preparation Module ===\n")
cat("=========================================\n\n")

# Facet definitions (Facet names and corresponding item/question numbers)
facet_definitions <- list(
  anhedonia = c("q9", "q11", "q43", "q65"),
  anxiousness = c("q24", "q36", "q48", "q78"),
  attention_seeking = c("q23", "q77", "q87", "q97"),
  callousness = c("q7", "q62", "q72", "q82"),
  deceitfulness = c("q18", "q51", "q95", "q99"),
  depressivity = c("q26", "q60", "q70", "q74"),
  distractability = c("q39", "q49", "q55", "q91"),
  eccentricity = c("q10", "q22", "q61", "q94"),
  emotional_lability = c("q41", "q53", "q71", "q81"),
  grandiosity = c("q14", "q37", "q85", "q90"),
  hostility = c("q12", "q31", "q66", "q75"),
  impulsivity = c("q2", "q5", "q6", "q8"),
  intimacy_avoidance = c("q29", "q40", "q56", "q93"),
  irresponsibility = c("q47", "q64", "q68", "q76"),
  manipulativeness = c("q35", "q44", "q69", "q100"),
  perceptual_dysregulation = c("q15", "q63", "q88", "q98"),
  perseveration = c("q19", "q25", "q32", "q46"),
  restricted_affectivity = c("q28", "q30", "q73", "q83"),
  rigid_perfectionism = c("q33", "q42", "q80", "q89"),
  risk_taking = c("q13", "q16", "q21", "q67"),
  separation_insecurity = c("q17", "q45", "q58", "q79"),
  submissiveness = c("q3", "q4", "q20", "q92"),
  suspiciousness = c("q1", "q38", "q50", "q86"),
  unusual_beliefs_and_experiences = c("q34", "q54", "q59", "q96"),
  withdrawal = c("q27", "q52", "q57", "q84")
)

# Cutoffs for "Elevated" (84.13rd percentile)
facet_cutoffs <- list(
  anhedonia = 7,
  anxiousness = 7,
  attention_seeking = 6,
  callousness = 4,
  deceitfulness = 5,
  depressivity = 5,
  distractability = 7,
  eccentricity = 7,
  emotional_lability = 7,
  grandiosity = 6,
  hostility = 7,
  impulsivity = 6,
  intimacy_avoidance = 6,
  irresponsibility = 4,
  manipulativeness = 6,
  perceptual_dysregulation = 4,
  perseveration = 6,
  restricted_affectivity = 7,
  rigid_perfectionism = 7,
  risk_taking = 7,
  separation_insecurity = 6,
  submissiveness = 8,
  suspiciousness = 7,
  unusual_beliefs_and_experiences = 6,
  withdrawal = 7
)

# # Cutoffs for "Clinically Significant" (93.32nd percentile)
# facet_cutoffs <- list(
#   anhedonia = 8,
#   anxiousness = 9,
#   attention_seeking = 8,
#   callousness = 5,
#   deceitfulness = 6,
#   depressivity = 6,
#   distractability = 8,
#   eccentricity = 8,
#   emotional_lability = 9,
#   grandiosity = 7,
#   hostility = 8,
#   impulsivity = 7,
#   intimacy_avoidance = 7,
#   irresponsibility = 5,
#   manipulativeness = 8,
#   perceptual_dysregulation = 5,
#   perseveration = 8,
#   restricted_affectivity = 8,
#   rigid_perfectionism = 9,
#   risk_taking = 8,
#   separation_insecurity = 8,
#   submissiveness = 9,
#   suspiciousness = 8,
#   unusual_beliefs_and_experiences = 7,
#   withdrawal = 9
# )

# # Configuration
# config_module_1 <- list(
#   questionnaire_type = "multi-construct",
#   id_column = "client_id",
#   constructs = facet_definitions,
#   cutoffs = facet_cutoffs
# )

# Option 1: Let framework calculate base rates from training data (default)
config_module_1 <- list(
  questionnaire_type = "multi-construct",
  id_column = "client_id",
  constructs = facet_definitions,
  cutoffs = facet_cutoffs
  # base_rates not specified - will be calculated from training data
)

# # Option 2: Let framework use the provided reference base rates to adjust probability calculations
# # Note: When reference base rates are provided, the framework automatically adjusts probability calculations
# # for SC methods (SC-EP, SC-SOR, SC-MOR) using Bayes' theorem. This recalibrates the conditional probabilities
# # from the training context (e.g., clinical population) to the reference context (e.g., general population),
# # ensuring that probability thresholds (gamma_0, gamma_1) maintain their intended meaning when deployed in
# # populations with different prevalence rates.
#
# # Specify reference base rates
# facet_base_rates <- list(
#   anhedonia = 0.05, # 5% in reference sample
#   anxiousness = 0.08, # 8% in reference sample
#   attention_seeking = 0.03, # 3% in reference sample
#   # ... etc
# )
#
# config_module_1 <- list(
#   questionnaire_type = "multi-construct",
#   id_column = "client_id",
#   constructs = facet_definitions,
#   cutoffs = facet_cutoffs,
#   base_rates = facet_base_rates
# )

# Prepare data (Module 1)
cat("Preparing data...\n")
prepared_data <- prepare_data(
  data_path = data_path,
  config = config_module_1,
  split_method = "random",
  split_ratio = 0.6,
  seed = 42
)

cat("\nData prepared.\n")

# ============================================================================
# End of Module 1
# ============================================================================

# ============================================================================
# Module 2: Configuration Module
# ============================================================================
# Purpose: Define and configure item ordering methods, reduction methods,
#          parameters, and generate valid method combinations
# ============================================================================

# Note on ordering method behavior:
# - For unidimensional questionnaires: All ordering methods work globally across all items
# - For multi-construct questionnaires: Ordering methods work within each construct
#   (constructs are presented in the order defined in the data configuration)
# - Exception: "domain_blocked" optimizes both construct order and within-construct order

cat("\n======================================\n")
cat("=== Module 2: Configuration Module ===\n")
cat("======================================\n\n")

# Configuration for stop-low only with custom gamma values
cat("Creating analysis configuration...\n\n")
config_module_2 <- create_analysis_config(
  questionnaire_type = prepared_data$config$questionnaire_type,
  ordering_methods = c("original", "auc", "incremental_auc", "correlation", "forward_stepwise"),
  reduction_methods = c("none", "dc", "sc_ep"),
  two_step_mode = TRUE,
  top_candidates = 15,
  constraints = list(
    stop_low_only = FALSE,
    min_items_per_construct = 1,
    complete_triggered_constructs = FALSE
  ),
  # fnr_threshold = 0.1,
  cutoff_method = "fixed"
)
print.curtailment_config(config_module_2)

# Custom gamma values for stop-low
# gamma_values <- list(
#   list(gamma_0 = 0.90, gamma_1 = 1.00),
#   list(gamma_0 = 0.95, gamma_1 = 1.00),
#   list(gamma_0 = 0.99, gamma_1 = 1.00)
# )
# gamma_values <- list(
#   list(gamma_0 = 0.95, gamma_1 = 1.00)
# )
gamma_values <- list(
  list(gamma_0 = 0.95, gamma_1 = 0.95)
)

# Generate combinations with custom gamma values
method_combinations <- generate_method_combinations(
  config_module_2,
  gamma_values
)
cat(sprintf("\nGenerated %d method combinations.\n", nrow(method_combinations)))

# Validate configuration
cat("\nValidating configuration...\n")
validation <- validate_configuration(config_module_2)
if (length(validation$warnings) > 0) {
  cat("Warnings:\n")
  for (w in validation$warnings) cat(paste("-", w, "\n"))
}
cat("Configuration is valid.\n")

# Assess regression method suitability if applicable
regression_assessment <- assess_regression_methods(
  analysis_config = config_module_2,
  data_config = prepared_data$config,
  n_samples = nrow(prepared_data$splits$train)
)

# Print assessment results
print_regression_assessment(regression_assessment, verbose = FALSE)

# ============================================================================
# End of Module 2
# ============================================================================

# ============================================================================
# Module 3: Item Ordering Module Testing
# ============================================================================
# Purpose: Test all ordering methods specified in the configuration
# ============================================================================

cat("\n======================================\n")
cat("=== Module 3: Item Ordering Module ===\n")
cat("======================================\n\n")

# Store results for all item ordering methods
ordering_results <- list()

# Test each item ordering method from the configuration
cat("Testing item ordering methods:\n")

for (method in config_module_2$ordering_methods) {
  cat(sprintf("  %s... ", method))
  
  tryCatch({
    # Apply ordering method
    ordered_result <- order_items(
      data = prepared_data$splits$train,
      config = prepared_data$config,
      method = method,
      interleave = TRUE # Added interleave parameter
    )
    
    # Store results
    ordering_results[[method]] <- ordered_result
    cat("✓\n")
    
  }, error = function(e) {
    cat("✗\n")
    cat(sprintf("    Error: %s\n", e$message))
    ordering_results[[method]] <- list(error = e$message)
  })
}

# Summary
successful <- sum(!sapply(ordering_results, function(x) "error" %in% names(x)))
cat(sprintf("\n%d/%d item ordering methods completed successfully.\n", 
            successful, length(config_module_2$ordering_methods)))

# Save results
saveRDS(ordering_results, file.path(run_dir, "ordering_results.rds")) # saveRDS(ordering_results, "ordering_results.rds")
cat("\nItem ordering results saved to 'ordering_results.rds'\n")

# ============================================================================
# End of Module 3
# ============================================================================

# ============================================================================
# Module 4: Item Reduction Module
# ============================================================================
# Purpose: Apply all reduction methods to ordered items from Module 3
# ============================================================================

cat("\n=======================================\n")
cat("=== Module 4: Item Reduction Module ===\n")
cat("=======================================\n\n")

# Initialize results storage
all_combination_results <- list()

# Counter for progress
total_combinations <- nrow(method_combinations)
completed <- 0

# # Enable debug output
# DEBUG_CURTAILMENT <- FALSE # Make TRUE to see debug output

# Test each method combination
cat("Testing method combinations:\n")

for (i in seq_len(nrow(method_combinations))) {
  combo <- method_combinations[i, ]
  
  cat(sprintf("  %s... ", combo$method_id))
  
  # Check if ordering exists and succeeded
  if (!(combo$ordering %in% names(ordering_results)) || 
      "error" %in% names(ordering_results[[combo$ordering]])) {
    cat("✗\n")
    cat(sprintf("    Error: Ordering method failed\n"))
    all_combination_results[[combo$method_id]] <- list(
      combination = combo,
      error = "Ordering method failed"
    )
    next
  }
  
  # Get ordered items
  ordered_items <- ordering_results[[combo$ordering]]$ordered_items
  
  # Set up method parameters
  method_params <- list()
  if (!is.na(combo$gamma_0)) method_params$gamma_0 <- combo$gamma_0
  if (!is.na(combo$gamma_1)) method_params$gamma_1 <- combo$gamma_1
  
  # # Fix: (Merge the constraints from config_module_2 into prepared_data$config before calling the reduction methods)
  # # Create a config (i.e., config_module_4) that combines prepared_data$config with the constraints specified in config_module_2 (i.e., config_module_2$constraints)
  # config_module_4 <- prepared_data$config
  # config_module_4$constraints <- config_module_2$constraints
  
  # Fix: (Merge prepared_data$config and config_module_2 into config_module_4 before calling the reduction methods)
  # Create a config (i.e., config_module_4) that contains the fields in prepared_data$config and the fields in config_module_2
  config_module_4 <- modifyList(prepared_data$config, config_module_2)
  # Preserve the class attribute from config_module_2
  class(config_module_4) <- class(config_module_2)
  
  # # Try to apply reduction method
  # tryCatch({
  #   # Training phase
  #   training_params <- train_reduction_method(
  #     method = combo$reduction,
  #     ordered_items = ordered_items,
  #     train_data = prepared_data$splits$train,
  #     config = config_module_4, # config = prepared_data$config,
  #     method_params = method_params
  #   )
  #   
  #   # Test phase
  #   test_results <- apply_reduction_method(
  #     method = combo$reduction,
  #     ordered_items = ordered_items,
  #     test_data = prepared_data$splits$test,
  #     config = config_module_4, # config = prepared_data$config,
  #     method_params = method_params,
  #     training_params = training_params
  #   )
  #   
  #   # Store results
  #   all_combination_results[[combo$method_id]] <- list(
  #     combination = combo,
  #     ordering_result = ordering_results[[combo$ordering]],
  #     reduction_result = test_results,
  #     training_params = training_params
  #   )
  #   
  #   completed <- completed + 1
  #   cat("✓\n")
  #   
  # }, error = function(e) {
  #   cat("✗\n")
  #   cat(sprintf("    Error: %s\n", e$message))
  #   all_combination_results[[combo$method_id]] <- list(
  #     combination = combo,
  #     error = e$message
  #   )
  # })
  
  # Fix: (New method combination testing loop to handle convergence failures)
  # Try to apply reduction method
  tryCatch({
    # Training phase
    training_params <- train_reduction_method(
      method = combo$reduction,
      ordered_items = ordered_items,
      train_data = prepared_data$splits$train,
      config = config_module_4,
      method_params = method_params
    )
    
    # Check if training failed due to model convergence (for regression methods)
    if (combo$reduction %in% c("sc_sor", "sc_mor")) {
      # Check for model failures in training params
      model_failed <- FALSE
      error_msg <- NULL
      
      # Check if model_result exists and indicates failure
      if (!is.null(training_params$model_result)) {
        if (!is.null(training_params$model_result$success) && 
            !training_params$model_result$success) {
          model_failed <- TRUE
          error_msg <- training_params$model_result$error_message
        }
      }
      # Also check if the training params indicate an error
      else if (!is.null(training_params$error)) {
        model_failed <- TRUE
        error_msg <- training_params$error
      }
      
      if (model_failed) {
        # Mark as error
        all_combination_results[[combo$method_id]] <- list(
          combination = combo,
          error = error_msg %||% "Regression model convergence failure"
        )
        cat("✗\n")
        cat(sprintf("    Error: %s\n", error_msg %||% "Regression model convergence failure"))
        next
      }
    }
    
    # Test phase
    test_results <- apply_reduction_method(
      method = combo$reduction,
      ordered_items = ordered_items,
      test_data = prepared_data$splits$test,
      config = config_module_4,
      method_params = method_params,
      training_params = training_params
    )
    
    # Check if test phase had errors
    if (!is.null(test_results$error)) {
      all_combination_results[[combo$method_id]] <- list(
        combination = combo,
        error = test_results$error
      )
      cat("✗\n")
      cat(sprintf("    Error: %s\n", test_results$error))
      next
    }
    
    # Additional check for NA classifications which indicate failure
    if (all(is.na(test_results$classifications))) {
      all_combination_results[[combo$method_id]] <- list(
        combination = combo,
        error = "All classifications are NA - method failed"
      )
      cat("✗\n")
      cat(sprintf("    Error: All classifications are NA - method failed\n"))
      next
    }
    
    # Store results
    all_combination_results[[combo$method_id]] <- list(
      combination = combo,
      ordering_result = ordering_results[[combo$ordering]],
      reduction_result = test_results,
      training_params = training_params
    )
    
    completed <- completed + 1
    cat("✓\n")
    
  }, error = function(e) {
    cat("✗\n")
    cat(sprintf("    Error: %s\n", e$message))
    all_combination_results[[combo$method_id]] <- list(
      combination = combo,
      error = e$message
    )
  })
}

# Summary
cat(sprintf("\n%d/%d method combinations completed successfully.\n", 
            completed, total_combinations))

# Save results
saveRDS(all_combination_results, file.path(run_dir, "all_combination_results.rds")) # saveRDS(all_combination_results, "all_combination_results.rds")
cat("\nAll method combination results saved to 'all_combination_results.rds'\n")

# ============================================================================
# End of Module 4
# ============================================================================

# ============================================================================
# Module 5: Evaluation Module
# ============================================================================
# Purpose: Evaluate all method combinations and generate reports
# ============================================================================

cat("\n=====================================\n")
cat("=== Module 5: Evaluation Module ===\n")
cat("=====================================\n\n")

# Source the evaluation module
# cat("Loading evaluation module...\n")
# source("module_5_evaluation.R")

# Evaluate all method combinations
cat("Starting comprehensive evaluation...\n")
evaluation_results <- evaluate_all_methods(
  results_path = file.path(run_dir, "all_combination_results.rds"), # results_path = "all_combination_results.rds",
  prepared_data = prepared_data,
  config = config_module_2,
  output_dir = file.path(run_dir, "evaluation_results") # output_dir = "evaluation_results"
)

# Print summary of results
# print(evaluation_results)

# # ============================================================================
# # Optional: Additional Analyses
# # ============================================================================
# 
# # 1. Examine specific method comparisons
# if (TRUE) {  # Set to TRUE to run
#   cat("\n\nComparing top two methods...\n")
#   top_two <- evaluation_results$performance_matrix[
#     order(evaluation_results$performance_matrix$balanced_accuracy * 
#             (1 - evaluation_results$performance_matrix$fnr), 
#           decreasing = TRUE)[1:2], "method_id"
#   ]
#   
#   comparison <- compare_methods(
#     method1_id = top_two[1],
#     method2_id = top_two[2],
#     all_results = all_combination_results,
#     prepared_data = prepared_data
#   )
#   
#   cat("Method 1:", comparison$method1$id, "\n")
#   cat("  - FNR:", sprintf("%.3f", comparison$method1$performance$fnr), "\n")
#   cat("  - Accuracy:", sprintf("%.3f", comparison$method1$performance$accuracy), "\n")
#   cat("  - Mean items:", sprintf("%.1f", comparison$method1$mean_items), "\n")
#   
#   cat("\nMethod 2:", comparison$method2$id, "\n")
#   cat("  - FNR:", sprintf("%.3f", comparison$method2$performance$fnr), "\n")
#   cat("  - Accuracy:", sprintf("%.3f", comparison$method2$performance$accuracy), "\n")
#   cat("  - Mean items:", sprintf("%.1f", comparison$method2$mean_items), "\n")
#   
#   cat("\nAgreement:", sprintf("%.1f%%", comparison$agreement * 100), "\n")
#   cat("McNemar's test p-value:", sprintf("%.4f", comparison$mcnemar_test$p.value), "\n")
# }
# 
# # 2. Examine construct-specific performance (for multi-construct)
# if (prepared_data$config$questionnaire_type == "multi-construct") {
#   cat("\n\nConstruct-specific performance for recommended method:\n")
#   rec_perf <- evaluation_results$full_performance
#   
#   if (!is.null(rec_perf$construct_metrics)) {
#     construct_summary <- do.call(rbind, lapply(names(rec_perf$construct_metrics), 
#                                                function(c) {
#                                                  metrics <- rec_perf$construct_metrics[[c]]
#                                                  data.frame(
#                                                    construct = c,
#                                                    sensitivity = metrics$sensitivity,
#                                                    specificity = metrics$specificity,
#                                                    fnr = metrics$fnr,
#                                                    accuracy = metrics$accuracy,
#                                                    stringsAsFactors = FALSE
#                                                  )
#                                                }))
#     
#     # Sort by FNR (worst performing first)
#     construct_summary <- construct_summary[order(construct_summary$fnr, 
#                                                  decreasing = TRUE), ]
#     
#     cat("\nConstructs with highest FNR:\n")
#     print(head(construct_summary, 5))
#     
#     # Save full construct analysis
#     write.csv(construct_summary, 
#               file.path("evaluation_results", "construct_performance.csv"),
#               row.names = FALSE)
#   }
# }
# 
# # 3. Sensitivity analysis for gamma parameters
# if (any(grepl("^sc_", evaluation_results$performance_matrix$reduction))) {
#   cat("\n\nGamma parameter sensitivity analysis:\n")
#   
#   sc_methods <- evaluation_results$performance_matrix[
#     grepl("^sc_", evaluation_results$performance_matrix$reduction), 
#   ]
#   
#   # Group by reduction method and analyze gamma effects
#   for (method in unique(sc_methods$reduction)) {
#     method_subset <- sc_methods[sc_methods$reduction == method, ]
#     
#     if (nrow(method_subset) > 1) {
#       cat("\n", method, ":\n")
#       cat("  Gamma_0 range:", range(method_subset$gamma_0, na.rm = TRUE), "\n")
#       cat("  FNR range:", range(method_subset$fnr, na.rm = TRUE), "\n")
#       cat("  Reduction % range:", range(method_subset$reduction_pct, na.rm = TRUE), "\n")
#       
#       # Check correlation between gamma and performance
#       if (length(unique(method_subset$gamma_0)) > 1) {
#         cor_gamma_fnr <- cor(method_subset$gamma_0, method_subset$fnr, 
#                              use = "complete.obs")
#         cat("  Correlation (gamma_0, FNR):", sprintf("%.3f", cor_gamma_fnr), "\n")
#       }
#     }
#   }
# }
# 
# # 4. Export key results for further analysis
# cat("\n\nExporting key results...\n")
# 
# # Recommended method details
# saveRDS(evaluation_results$recommended_method, 
#         file.path("evaluation_results", "recommended_method.rds"))
# 
# # Pareto-optimal methods
# write.csv(evaluation_results$detailed_results$pareto_optimal,
#           file.path("evaluation_results", "pareto_optimal_methods.csv"),
#           row.names = FALSE)
# 
# # Performance matrix for all methods
# write.csv(evaluation_results$performance_matrix,
#           file.path("evaluation_results", "all_methods_performance.csv"),
#           row.names = FALSE)
# 
# cat("\n✅ Evaluation module completed successfully!\n")
# cat("Results saved to: evaluation_results/\n")
# cat("\nKey outputs:\n")
# cat("  - Executive summary: evaluation_results/executive_summary.txt\n")
# cat("  - Performance report: evaluation_results/performance_report.html\n")
# cat("  - Visualizations: evaluation_results/visualizations/\n")
# cat("  - Implementation guide: evaluation_results/implementation_guide.txt\n")

# ============================================================================
# End of Module 5
# ============================================================================

# ============================================================================
# Module 6: Optimization Module (OPTIONAL)
# ============================================================================
# Purpose: Optimize confidence parameters (gamma values) for methods that support it
# ============================================================================

# Check if the recommended method supports optimization
recommended_method <- evaluation_results$recommended_method
supports_optimization <- recommended_method$reduction %in% c("sc_ep", "sc_sor", "sc_mor")

if (supports_optimization) {
  cat("\n=====================================\n")
  cat("=== Module 6: Optimization Module ===\n")
  cat("=====================================\n\n")
  cat("The recommended method supports gamma optimization.\n")
  cat("Do you want to optimize the gamma values? (y/n): ")
  
  # For non-interactive mode, set this to "y" or "n"
  optimize_choice <- "y"  # Change this for your use case
  
  if (tolower(optimize_choice) == "y") {
    # Run optimization
    optimization_results <- optimize_method(
      method_id = NULL, # If NULL, use recommended method, or else specify a method_id (e.g., "incremental_auc_sc_ep_g0_0.95_g1_1.00")
      evaluation_results = evaluation_results,
      all_combination_results = all_combination_results,
      prepared_data = prepared_data,
      config = config_module_4,
      code_dir = code_dir,
      optimization_config = list(
        base_rate_threshold = 0.20,
        fnr_threshold_high_prevalence = 0.05,  # Maximum fnr for high prevalence constructs
        fnr_threshold_low_prevalence = 0.05,   # Maximum fnr for low prevalence constructs
        gamma_0_high_prevalence = 0.70,  # Minimum gamma_0 for high prevalence constructs
        gamma_1_high_prevalence = 0.70,  # Minimum gamma_1 for high prevalence constructs
        gamma_0_low_prevalence = 0.80,   # Minimum gamma_0 for low prevalence constructs
        gamma_1_low_prevalence = 0.80,   # Minimum gamma_1 for low prevalence constructs
        gamma_search_step = 0.01,        # Step size for binary search
        use_parallel = TRUE
      ),
      output_dir = "optimization_results"
    )
    optimization_performed <- TRUE
  } else {
    # Do not run optimization
    cat("Skipping optimization.\n")
    optimization_performed <- FALSE
  }
} else {
  cat("\n=====================================\n")
  cat("=== Module 6: Optimization Module ===\n")
  cat("=====================================\n\n")
  cat("The recommended method (", recommended_method$reduction, ") does not support optimization.\n")
  cat("Skipping optimization module.\n")
  optimization_performed <- FALSE
}

# ============================================================================
# End of Module 6
# ============================================================================

# ============================================================================
# Module 7: Deployment Module
# ============================================================================
# Purpose: Generate deployment-ready outputs
# ============================================================================

cat("\n=====================================\n")
cat("=== Module 7: Deployment Module ===\n")
cat("=====================================\n\n")

# Create item_definitions for PID-5-SF
# Define the typical rating scale for PID-5-SF
rateValues <- list(
  list(value = 0, text = "Very False or Often False"),
  list(value = 1, text = "Sometimes False or Somewhat False"),
  list(value = 2, text = "Sometimes True or Somewhat True"),
  list(value = 3, text = "Very True or Often True")
)

# Create item texts vector
item_texts_vector <- c(
  "Plenty of people are out to get me.",
  "I feel like I act totally on impulse.",
  "I change what I do depending on what others want.",
  "I usually do what others think I should do.",
  "I usually do things on impulse without thinking about what might happen as a result.",
  "Even though I know better, I can't stop making rash decisions.",
  "I really don't care if I make other people suffer.",
  "I always do things on the spur of the moment.",
  "Nothing seems to interest me very much.",
  "People have told me that I think about things in a really strange way.",
  "I almost never enjoy life.",
  "I am easily angered.",
  "I have no limits when it comes to doing dangerous things.",
  "To be honest, I'm just more important than other people.",
  "It's weird, but sometimes ordinary objects seem to be a different shape than usual.",
  "I do a lot of things that others consider risky.",
  "I worry a lot about being alone.",
  "I often make up things about myself to help me get what I want.",
  "I keep approaching things the same way, even when it isn't working.",
  "I do what other people tell me to do.",
  "I like to take risks.",
  "Others seem to think I'm quite odd or unusual.",
  "I love getting the attention of other people.",
  "I worry a lot about terrible things that might happen.",
  "I have trouble changing how I'm doing something even if what I'm doing isn't going well.",
  "The world would be better off if I were dead.",
  "I keep my distance from people.",
  "I don't get emotional.",
  "I prefer to keep romance out of my life.",
  "I don't show emotions strongly.",
  "I have a very short temper.",
  "I get fixated on certain things and can't stop.",
  "If something I do isn't absolutely perfect, it's simply not acceptable.",
  "I often have unusual experiences, such as sensing the presence of someone who isn't actually there.",
  "I'm good at making people do what I want them to do.",
  "I'm always worrying about something.",
  "I'm better than almost everyone else.",
  "I'm always on my guard for someone trying to trick or harm me.",
  "I have trouble keeping my mind focused on what needs to be done.",
  "I'm just not very interested in having sexual relationships.",
  "I get emotional easily, often for very little reason.",
  "Even though it drives other people crazy, I insist on absolute perfection in everything I do.",
  "I almost never feel happy about my day-to-day activities.",
  "Sweet-talking others helps me get what I want.",
  "I fear being alone in life more than anything else.",
  "I get stuck on one way of doing things, even when it's clear it won't work.",
  "I'm often pretty careless with my own and others' things.",
  "I am a very anxious person.",
  "I am easily distracted.",
  "It seems like I'm always getting a raw deal from others.",
  "I don't hesitate to cheat if it gets me ahead.",
  "I don't like spending time with others.",
  "I never know where my emotions will go from moment to moment.",
  "I have seen things that weren't really there.",
  "I can't focus on things for very long.",
  "I steer clear of romantic relationships.",
  "I'm not interested in making friends.",
  "I'll do just about anything to keep someone from abandoning me.",
  "Sometimes I can influence other people just by sending my thoughts to them.",
  "Life looks pretty bleak to me.",
  "I think about things in odd ways that don't make sense to most people.",
  "I don't care if my actions hurt others.",
  "Sometimes I feel controlled by thoughts that belong to someone else.",
  "I make promises that I don't really intend to keep.",
  "Nothing seems to make me feel good.",
  "I get irritated easily by all sorts of things.",
  "I do what I want regardless of how unsafe it might be.",
  "I often forget to pay my bills.",
  "I'm good at conning people.",
  "Everything seems pointless to me.",
  "I get emotional over every little thing.",
  "It's no big deal if I hurt other peoples' feelings.",
  "I never show emotions to others.",
  "I have no worth as a person.",
  "I am usually pretty hostile.",
  "I've skipped town to avoid responsibilities.",
  "I like being a person who gets noticed.",
  "I'm always fearful or on edge about bad things that might happen.",
  "I never want to be alone.",
  "I keep trying to make things perfect, even when I've gotten them as good as they're likely to get.",
  "My emotions are unpredictable.",
  "I don't care about other peoples' problems.",
  "I don't react much to things that seem to make others emotional.",
  "I avoid social events.",
  "I deserve special treatment.",
  "I suspect that even my so-called friends betray me a lot.",
  "I crave attention.",
  "Sometimes I think someone else is removing thoughts from my head.",
  "I simply won't put up with things being out of their proper places.",
  "I often have to deal with people who are less important than me.",
  "I get pulled off-task by even minor distractions.",
  "I try to do what others want me to do.",
  "I prefer being alone to having a close romantic partner.",
  "I often have thoughts that make sense to me but that other people say are strange.",
  "I use people to get what I want.",
  "I've had some really weird experiences that are very difficult to explain.",
  "I like to draw attention to myself.",
  "Things around me often feel unreal, or more real than usual.",
  "I'll stretch the truth if it's to my advantage.",
  "It is easy for me to take advantage of others."
)

# Create item_definitions list
item_definitions <- list()
for (i in 1:100) {
  item_id <- paste0("q", i)
  item_definitions[[item_id]] <- list(
    item_id = item_id,
    item_text = item_texts_vector[i],
    rateValues = rateValues
  )
}

survey_config = list(
  title = "PID-5-SF",
  description = "This is a list of things different people might say about themselves. We are interested in how you would describe yourself. There are no right or wrong answers, so you can describe yourself as honestly as possible. We'd like you to take your time and read each statement carefully, selecting the response that best describes you.",
  autoGenerate = FALSE,
  displayMode = "buttons"
)

# Generate deployment package
if (optimization_performed) {
  # Deploy the optimized method
  deployment_package <- generate_deployment_package(
    optimization_results = optimization_results,
    prepared_data = prepared_data,
    item_definitions = item_definitions,
    survey_config = survey_config,
    output_dir = "deployment"
  )
} else {
  # Deploy the recommended method without optimization
  deployment_package <- generate_deployment_package(
    evaluation_results = evaluation_results,
    all_combination_results = all_combination_results,
    prepared_data = prepared_data,
    method_id = NULL, # if NULL, the recommended method will be used
    item_definitions = item_definitions,
    survey_config = survey_config,
    output_dir = "deployment"
  )
}

# ============================================================================
# End of Module 7
# ============================================================================

# # ============================================================================
# # Save run configuration and summary
# # ============================================================================
# 
# # Save configuration for reproducibility
# run_config <- list(
#   run_timestamp = run_timestamp,
#   code_dir = code_dir,
#   data_path = data_path,
#   run_dir = run_dir,
#   config_module_1 = config_module_1,
#   config_module_2 = config_module_2,
#   gamma_values = gamma_values,
#   method_combinations = method_combinations,
#   R_version = R.version.string,
#   platform = Sys.info()
# )
# 
# saveRDS(run_config, file.path(run_dir, "run_configuration.rds"))
# 
# end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
# run_time_mins <- floor(as.numeric(difftime(end_time, start_time, units = "mins")))
# cat("Run ended at:", end_time, "\n", file = log_file, append = TRUE)
# cat("Run time:", run_time_mins, "minutes\n", file = log_file, append = TRUE)
# 
# # Create a summary file
# summary_file <- file.path(run_dir, "run_summary.txt")
# cat("\nRUN SUMMARY\n", file = summary_file)
# cat("===========\n\n", file = summary_file, append = TRUE)
# cat("Run timestamp:", run_timestamp, "\n", file = summary_file, append = TRUE)
# cat("Code directory:", code_dir, "\n", file = summary_file, append = TRUE)
# cat("Data file path:", data_path, "\n", file = summary_file, append = TRUE)
# cat("Output directory:", run_dir, "\n", file = summary_file, append = TRUE)
# cat("Run started at:", start_time, "\n", file = summary_file, append = TRUE)
# cat("Run ended at:", end_time, "\n", file = summary_file, append = TRUE)
# cat("Run time:", run_time_mins, "minutes\n", file = summary_file, append = TRUE)
# cat("\nConfiguration:\n", file = summary_file, append = TRUE)
# cat("- Ordering methods:", paste(config_module_2$ordering_methods, collapse = ", "), "\n", file = summary_file, append = TRUE)
# cat("- Reduction methods:", paste(config_module_2$reduction_methods, collapse = ", "), "\n", file = summary_file, append = TRUE)
# cat("- Total combinations tested:", nrow(method_combinations), "\n", file = summary_file, append = TRUE)
# cat("- Successful combinations:", completed, "\n", file = summary_file, append = TRUE)
# 
# cat("\n✅ Analysis completed\n")
# cat("Results saved to:", run_dir, "\n")
# cat("\nKey outputs:\n")
# cat("  - Run configuration: run_configuration.rds\n")
# cat("  - Run summary: run_summary.txt\n")
# cat("  - Ordering results: ordering_results.rds\n")
# cat("  - All results: all_combination_results.rds\n")
# cat("  - Evaluation results: evaluation_results/\n")

# ============================================================================
# Save run configuration and summary
# ============================================================================

# Save configuration for reproducibility
run_config <- list(
  run_timestamp = run_timestamp,
  code_dir = code_dir,
  data_path = data_path,
  run_dir = run_dir,
  config_module_1 = config_module_1,
  config_module_2 = config_module_2,
  gamma_values = gamma_values,
  method_combinations = method_combinations,
  optimization_performed = optimization_performed,
  optimization_config = if(optimization_performed) optimization_results$optimization_config else NULL,
  deployed_method_id = deployment_package$method_id,
  deployed_method_optimized = deployment_package$is_optimized,
  R_version = R.version.string,
  platform = Sys.info()
)

saveRDS(run_config, file.path(run_dir, "run_configuration.rds"))

end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
run_time_mins <- floor(as.numeric(difftime(end_time, start_time, units = "mins")))
cat("Run ended at:", end_time, "\n", file = log_file, append = TRUE)
cat("Run time:", run_time_mins, "minutes\n", file = log_file, append = TRUE)

# Create a summary file
summary_file <- file.path(run_dir, "run_summary.txt")
cat("\nRUN SUMMARY\n", file = summary_file)
cat("===========\n\n", file = summary_file, append = TRUE)
cat("Run timestamp:", run_timestamp, "\n", file = summary_file, append = TRUE)
cat("Code directory:", code_dir, "\n", file = summary_file, append = TRUE)
cat("Data file path:", data_path, "\n", file = summary_file, append = TRUE)
cat("Output directory:", run_dir, "\n", file = summary_file, append = TRUE)
cat("Run started at:", start_time, "\n", file = summary_file, append = TRUE)
cat("Run ended at:", end_time, "\n", file = summary_file, append = TRUE)
cat("Run time:", run_time_mins, "minutes\n", file = summary_file, append = TRUE)
cat("\nConfiguration:\n", file = summary_file, append = TRUE)
cat("- Ordering methods:", paste(config_module_2$ordering_methods, collapse = ", "), "\n", file = summary_file, append = TRUE)
cat("- Reduction methods:", paste(config_module_2$reduction_methods, collapse = ", "), "\n", file = summary_file, append = TRUE)
cat("- Total combinations tested:", nrow(method_combinations), "\n", file = summary_file, append = TRUE)
cat("- Successful combinations:", completed, "\n", file = summary_file, append = TRUE)

# Update summary file
if (optimization_performed) {
  cat("\n\nOPTIMIZATION RESULTS\n", file = summary_file, append = TRUE)
  cat("-------------------\n", file = summary_file, append = TRUE)
  cat("Original method: ", optimization_results$original_method_id, "\n",
      file = summary_file, append = TRUE)
  cat("Optimization performed: Yes\n", file = summary_file, append = TRUE)
  
  # Add optimization statistics
  analysis <- optimization_results$optimization_analysis
  n_constructs <- length(analysis$construct_results)
  n_optimized <- sum(sapply(analysis$construct_results,
                            function(x) x$success && !isTRUE(x$optimization_failed)))
  n_failed <- sum(sapply(analysis$construct_results,
                         function(x) isTRUE(x$optimization_failed)))
  
  cat("Constructs successfully optimized: ", n_optimized, " of ", n_constructs, "\n",
      file = summary_file, append = TRUE)
  if (n_failed > 0) {
    cat("Constructs failed optimization: ", n_failed, "\n",
        file = summary_file, append = TRUE)
  }
  
  # Performance improvement
  orig_fnr <- optimization_results$original_performance$fnr
  opt_fnr <- optimization_results$optimized_performance$fnr
  cat("Overall FNR: ", sprintf("%.3f → %.3f (%.1f%% reduction)",
                               orig_fnr, opt_fnr,
                               100 * (orig_fnr - opt_fnr) / orig_fnr), "\n",
      file = summary_file, append = TRUE)
}

cat("\n✅ Analysis completed\n")
cat("Results saved to:", run_dir, "\n")

# Restore code_dir as working directory
setwd(code_dir)

# Stop redirecting output
# sink()