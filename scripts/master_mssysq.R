# ============================================================================
# Set Directory Paths and Input Data File Name
# ============================================================================

# Set code directory (where the module scripts are located)
code_dir <- "/Users/simonbaker/GitHub/curtailment/R"

# Set data directory (where the data file is located)
data_dir <- "/Users/simonbaker/GitHub/curtailment/data"

# Set data file name
data_file <- "mss-ysq_data_for_curtailment.csv"

# Set output base directory
output_base_dir <- "/Users/simonbaker/GitHub/curtailment/output"

# Set output subdirectory name (e.g., the assessment's abbreviated name)
output_sub_dir <- "mssysq"

# ============================================================================
# Initial Setup (Do Not Edit)
# ============================================================================

# Set data file path
data_path <- file.path(data_dir, data_file)

# Create datetime for this run
run_timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M")
run_name <- paste0("curtailment_", run_timestamp)

# Create output directory for this assessment and this run
output_dir <- file.path(output_base_dir, output_sub_dir)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
run_dir <- file.path(output_dir, run_name)
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
source(file.path(code_dir, "module_8_deployment_validation.R"))
cat("Modules loaded successfully.\n")

# ============================================================================
# Module 1: Data Preparation Module
# ============================================================================
# Purpose: Validate input data, check structure, and create train/test splits
# ============================================================================

cat("\n=========================================\n")
cat("=== Module 1: Data Preparation Module ===\n")
cat("=========================================\n\n")

# # Schema definitions (Schema names and corresponding item/question numbers)
# schema_definitions <- list(
#   Abandonment_Anxious_Attachment = c("q1", "q2", "q3", "q4"),
#   Excessive_Self_Reliance_Avoidant_Attachment = c("q5", "q6", "q7", "q8"),
#   Emotional_Deprivation = c("q9", "q10", "q11", "q12"),
#   Mistrust_of_Others = c("q13", "q14", "q15", "q16"),
#   Others_are_Dangerous_Malevolent = c("q17", "q18", "q19", "q20"),
#   Social_Isolation_Outsider = c("q21", "q22", "q23", "q24"),
#   Defectiveness_Shame = c("q25", "q26", "q27", "q28"),
#   Vulnerability_to_Dangerous_World = c("q29", "q30", "q31", "q32"),
#   Dependence = c("q33", "q34", "q35", "q36"),
#   Failure_Achievement_Inferiority = c("q37", "q38", "q39", "q40"),
#   Low_Self_Efficacy_Weakness = c("q41", "q42", "q43", "q44"),
#   Fatalistic_External_Locus_of_Control = c("q45", "q46", "q47", "q48"),
#   Enmeshment_Diffuse_Boundaries = c("q49", "q50", "q51", "q52"),
#   Subjugation = c("q53", "q54", "q55", "q56"),
#   Self_Sacrifice = c("q57", "q58", "q59", "q60"),
#   Approval_Seeking = c("q61", "q62", "q63", "q64"),
#   Emotional_Inhibition = c("q65", "q66", "q67", "q68"),
#   Pessimism_Negativity = c("q69", "q70", "q71", "q72"),
#   Unrelenting_Standards = c("q73", "q74", "q75", "q76"),
#   Punitiveness_Unforgiving_of_Self = c("q77", "q78", "q79", "q80"),
#   Punitiveness_Unforgiving_of_Others = c("q81", "q82", "q83", "q84"),
#   Entitlement_Specialness = c("q85", "q86", "q87", "q88"),
#   Full_Control = c("q89", "q90", "q91", "q92"),
#   Over_Reliance_on_Emotions = c("q93", "q94", "q95", "q96"),
#   Unfairness = c("q97", "q98", "q99", "q100"),
#   Meaningless_World = c("q101", "q102", "q103", "q104"),
#   Lack_of_Coherent_Identity = c("q105", "q106", "q107", "q108")
# )

# Schema definitions (Schema names and corresponding item/question numbers)
schema_definitions <- list(
  Abandonment_Anxious_Attachment = c("q1", "q2", "q3", "q4"),
  Emotional_Deprivation = c("q5", "q6", "q7", "q8"),
  Mistrust_of_Others = c("q9", "q10", "q11", "q12"),
  Social_Isolation_Outsider = c("q13", "q14", "q15", "q16"),
  Defectiveness_Shame = c("q17", "q18", "q19", "q20"),
  Vulnerability_to_Dangerous_World = c("q21", "q22", "q23", "q24"),
  Dependence = c("q25", "q26", "q27", "q28"),
  Low_Self_Efficacy_Weakness = c("q29", "q30", "q31", "q32"),
  Failure_Achievement_Inferiority = c("q33", "q34", "q35", "q36"),
  Enmeshment_Diffuse_Boundaries = c("q37", "q38", "q39", "q40"),
  Subjugation_Submission_to_Others = c("q41", "q42", "q43", "q44"),
  Self_Sacrifice = c("q45", "q46", "q47", "q48"),
  Approval_Seeking_Excessive_Need_to_be_Liked = c("q49", "q50", "q51", "q52"),
  Emotional_Inhibition = c("q53", "q54", "q55", "q56"),
  Pessimism_Negativity = c("q57", "q58", "q59", "q60"),
  Unrelenting_Standards = c("q61", "q62", "q63", "q64"),
  Punitiveness_Unforgiving_of_Self = c("q65", "q66", "q67", "q68"),
  Punitiveness_Unforgiving_of_Others = c("q69", "q70", "q71", "q72"),
  Entitlement_Specialness = c("q73", "q74", "q75", "q76")
)

# # Cutoffs for "Moderate" (average score = 2.5; i.e., raw score = 10)
# schema_cutoffs <- list(
#   Abandonment_Anxious_Attachment = 10,
#   Excessive_Self_Reliance_Avoidant_Attachment = 10,
#   Emotional_Deprivation = 10,
#   Mistrust_of_Others = 10,
#   Others_are_Dangerous_Malevolent = 10,
#   Social_Isolation_Outsider = 10,
#   Defectiveness_Shame = 10,
#   Vulnerability_to_Dangerous_World = 10,
#   Dependence = 10,
#   Failure_Achievement_Inferiority = 10,
#   Low_Self_Efficacy_Weakness = 10,
#   Fatalistic_External_Locus_of_Control = 10,
#   Enmeshment_Diffuse_Boundaries = 10,
#   Subjugation = 10,
#   Self_Sacrifice = 10,
#   Approval_Seeking = 10,
#   Emotional_Inhibition = 10,
#   Pessimism_Negativity = 10,
#   Unrelenting_Standards = 10,
#   Punitiveness_Unforgiving_of_Self = 10,
#   Punitiveness_Unforgiving_of_Others = 10,
#   Entitlement_Specialness = 10,
#   Full_Control = 10,
#   Over_Reliance_on_Emotions = 10,
#   Unfairness = 10,
#   Meaningless_World = 10,
#   Lack_of_Coherent_Identity = 10
# )

# Cutoffs for "Moderate" (average score = 2.5; i.e., raw score = 10)
schema_cutoffs <- list(
  Abandonment_Anxious_Attachment = 10,
  Emotional_Deprivation = 10,
  Mistrust_of_Others = 10,
  Social_Isolation_Outsider = 10,
  Defectiveness_Shame = 10,
  Vulnerability_to_Dangerous_World = 10,
  Dependence = 10,
  Low_Self_Efficacy_Weakness = 10,
  Failure_Achievement_Inferiority = 10,
  Enmeshment_Diffuse_Boundaries = 10,
  Subjugation_Submission_to_Others = 10,
  Self_Sacrifice = 10,
  Approval_Seeking_Excessive_Need_to_be_Liked = 10,
  Emotional_Inhibition = 10,
  Pessimism_Negativity = 10,
  Unrelenting_Standards = 10,
  Punitiveness_Unforgiving_of_Self = 10,
  Punitiveness_Unforgiving_of_Others = 10,
  Entitlement_Specialness = 10
)

# Option 1: Let framework calculate base rates from training data (default)
config_module_1 <- list(
  questionnaire_type = "multi-construct",
  id_column = "id", #"client_id"
  constructs = schema_definitions,
  cutoffs = schema_cutoffs
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
# schema_base_rates <- list(
#   Abandonment_Anxious_Attachment = 0.05, # 5% in reference sample
#   Excessive_Self_Reliance_Avoidant_Attachment = 0.08, # 8% in reference sample
#   Emotional_Deprivation = 0.03, # 3% in reference sample
#   # ... etc
# )
#
# config_module_1 <- list(
#   questionnaire_type = "multi-construct",
#   id_column = "client_id",
#   constructs = schema_definitions,
#   cutoffs = schema_cutoffs,
#   base_rates = schema_base_rates
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
  ordering_methods = c("original", "auc", "incremental_auc"),
  reduction_methods = c("none", "sc_ep"),
  two_step_mode = TRUE,
  top_candidates = 6,
  constraints = list(
    stop_low_only = TRUE,
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
gamma_values <- list(
  list(gamma_0 = 0.95, gamma_1 = 1.00)
)
# gamma_values <- list(
#   list(gamma_0 = 0.95, gamma_1 = 0.95)
# )

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
saveRDS(ordering_results, file.path(run_dir, "ordering_results.rds"))
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
  
  # Merge prepared_data$config and config_module_2 into config_module_4 before calling the reduction methods
  # Create a config (i.e., config_module_4) that contains the fields in prepared_data$config and the fields in config_module_2
  config_module_4 <- modifyList(prepared_data$config, config_module_2)
  # Preserve the class attribute from config_module_2
  class(config_module_4) <- class(config_module_2)
  
  # Try to apply reduction method
  # Note: This method combination testing loop is supposed to handle convergence failures
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
saveRDS(all_combination_results, file.path(run_dir, "all_combination_results.rds"))
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

# Evaluate all method combinations
cat("Starting comprehensive evaluation...\n")
evaluation_results <- evaluate_all_methods(
  results_path = file.path(run_dir, "all_combination_results.rds"),
  prepared_data = prepared_data,
  config = config_module_2,
  output_dir = file.path(run_dir, "evaluation_results")
)

# Print summary of results
# print(evaluation_results)

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
        gamma_1_high_prevalence = 1.00,  # Minimum gamma_1 for high prevalence constructs
        gamma_0_low_prevalence = 0.80,   # Minimum gamma_0 for low prevalence constructs
        gamma_1_low_prevalence = 1.00,   # Minimum gamma_1 for low prevalence constructs
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

# Create item_definitions for Maladaptive Schema Scale - Young Schema Aligned (MSS-YSQ)
# Questions/Items q3, q5, q12, q15, q22, q28, q31, q34, q47, q53, q58, q61, q64, q65, q69 are reverse-scored

# Define the typical rating scale for MSS-YSQ
rateValues <- list(
  list(value = 0, text = "Strongly disagree"),
  list(value = 1, text = "Disagree"),
  list(value = 2, text = "Neutral"),
  list(value = 3, text = "Agree"),
  list(value = 4, text = "Strongly agree")
)

# Define the reverse-scored rating scale for MSS-YSQ
reverse_rateValues <- list(
  list(value = 4, text = "Strongly disagree"),
  list(value = 3, text = "Disagree"),
  list(value = 2, text = "Neutral"),
  list(value = 1, text = "Agree"),
  list(value = 0, text = "Strongly agree")
)

# Create item_definitions list
item_definitions <- list(
  q1 = list(
    item_id = "q1",
    item_text = "I fear that my important relationships will end unexpectedly.",
    rateValues = rateValues
  ),
  
  q2 = list(
    item_id = "q2",
    item_text = "I worry that people I love can't be there for me in a committed way.",
    rateValues = rateValues
  ),
  
  q3 = list(
    item_id = "q3",
    item_text = "I feel confident that other people will be there for me when I need them.",
    rateValues = reverse_rateValues
  ),
  
  q4 = list(
    item_id = "q4",
    item_text = "I worry about losing people that I rely on.",
    rateValues = rateValues
  ),
  
  q5 = list(
    item_id = "q5",
    item_text = "I have others I can depend on for advice and emotional support.",
    rateValues = reverse_rateValues
  ),
  
  q6 = list(
    item_id = "q6",
    item_text = "If I was in trouble, I wouldn't know who to call.",
    rateValues = rateValues
  ),
  
  q7 = list(
    item_id = "q7",
    item_text = "Other people don't care about my emotional needs.",
    rateValues = rateValues
  ),
  
  q8 = list(
    item_id = "q8",
    item_text = "I feel unsupported by others, so I wouldn't share my emotions.",
    rateValues = rateValues
  ),
  
  q9 = list(
    item_id = "q9",
    item_text = "People usually conceal their real intentions.",
    rateValues = rateValues
  ),
  
  q10 = list(
    item_id = "q10",
    item_text = "I don't trust people.",
    rateValues = rateValues
  ),
  
  q11 = list(
    item_id = "q11",
    item_text = "I don't believe what people say at face value.",
    rateValues = rateValues
  ),
  
  q12 = list(
    item_id = "q12",
    item_text = "People usually tell the truth.",
    rateValues = reverse_rateValues
  ),
  
  q13 = list(
    item_id = "q13",
    item_text = "I'm inherently different from everyone else.",
    rateValues = rateValues
  ),
  
  q14 = list(
    item_id = "q14",
    item_text = "I haven't met anyone that thinks like me.",
    rateValues = rateValues
  ),
  
  q15 = list(
    item_id = "q15",
    item_text = "I am typically accepted by people.",
    rateValues = reverse_rateValues
  ),
  
  q16 = list(
    item_id = "q16",
    item_text = "I am an outsider.",
    rateValues = rateValues
  ),
  
  q17 = list(
    item_id = "q17",
    item_text = "If people knew the real me, they wouldn't like me.",
    rateValues = rateValues
  ),
  
  q18 = list(
    item_id = "q18",
    item_text = "I am inherently defective.",
    rateValues = rateValues
  ),
  
  q19 = list(
    item_id = "q19",
    item_text = "My flaws make me unlovable.",
    rateValues = rateValues
  ),
  
  q20 = list(
    item_id = "q20",
    item_text = "I have reasons to be ashamed of myself and my character.",
    rateValues = rateValues
  ),
  
  q21 = list(
    item_id = "q21",
    item_text = "I'm afraid of venturing too far because there are so many bad things happening.",
    rateValues = rateValues
  ),
  
  q22 = list(
    item_id = "q22",
    item_text = "The world is safe for me.",
    rateValues = reverse_rateValues
  ),
  
  q23 = list(
    item_id = "q23",
    item_text = "The world is a dangerous and unforgiving place, and I worry it will spiral into catastrophe.",
    rateValues = rateValues
  ),
  
  q24 = list(
    item_id = "q24",
    item_text = "The world is a bad place and will harm me.",
    rateValues = rateValues
  ),
  
  q25 = list(
    item_id = "q25",
    item_text = "I cannot take care of myself, so I need others to take care of me.",
    rateValues = rateValues
  ),
  
  q26 = list(
    item_id = "q26",
    item_text = "I feel incapable of managing daily tasks without help from others.",
    rateValues = rateValues
  ),
  
  q27 = list(
    item_id = "q27",
    item_text = "I often worry about making decisions on my own and prefer someone else to make them for me.",
    rateValues = rateValues
  ),
  
  q28 = list(
    item_id = "q28",
    item_text = "I feel confident making decisions on my own.",
    rateValues = reverse_rateValues
  ),
  
  q29 = list(
    item_id = "q29",
    item_text = "If a task is difficult, I'm unlikely to be able to accomplish it.",
    rateValues = rateValues
  ),
  
  q30 = list(
    item_id = "q30",
    item_text = "I can rarely come up with solutions to my own problems.",
    rateValues = rateValues
  ),
  
  q31 = list(
    item_id = "q31",
    item_text = "I can handle anything that comes my way.",
    rateValues = reverse_rateValues
  ),
  
  q32 = list(
    item_id = "q32",
    item_text = "Most problems are too hard for me to deal with.",
    rateValues = rateValues
  ),
  
  q33 = list(
    item_id = "q33",
    item_text = "Most other people have achieved more than me.",
    rateValues = rateValues
  ),
  
  q34 = list(
    item_id = "q34",
    item_text = "I feel proud of my accomplishments.",
    rateValues = reverse_rateValues
  ),
  
  q35 = list(
    item_id = "q35",
    item_text = "I feel inferior when I think of the accomplishments of others.",
    rateValues = rateValues
  ),
  
  q36 = list(
    item_id = "q36",
    item_text = "I compare my achievements with others and feel that I am not as successful.",
    rateValues = rateValues
  ),
  
  q37 = list(
    item_id = "q37",
    item_text = "I am responsible for the emotions of the person I am closest to.",
    rateValues = rateValues
  ),
  
  q38 = list(
    item_id = "q38",
    item_text = "With those closest to me, I don't know where my needs and emotions end and where theirs begin.",
    rateValues = rateValues
  ),
  
  q39 = list(
    item_id = "q39",
    item_text = "I am so close to someone it feels like I have merged with them.",
    rateValues = rateValues
  ),
  
  q40 = list(
    item_id = "q40",
    item_text = "The needs of the person closest to me consume me.",
    rateValues = rateValues
  ),
  
  q41 = list(
    item_id = "q41",
    item_text = "Other people know better than I do.",
    rateValues = rateValues
  ),
  
  q42 = list(
    item_id = "q42",
    item_text = "I should always do as I'm told.",
    rateValues = rateValues
  ),
  
  q43 = list(
    item_id = "q43",
    item_text = "Other people know what is best for me.",
    rateValues = rateValues
  ),
  
  q44 = list(
    item_id = "q44",
    item_text = "I feel like I have to let others take control in relationships.",
    rateValues = rateValues
  ),
  
  q45 = list(
    item_id = "q45",
    item_text = "I always prioritise others no matter what's going on for me.",
    rateValues = rateValues
  ),
  
  q46 = list(
    item_id = "q46",
    item_text = "I believe it is my duty to listen to other people's problems.",
    rateValues = rateValues
  ),
  
  q47 = list(
    item_id = "q47",
    item_text = "My needs are as important as other people's needs.",
    rateValues = reverse_rateValues
  ),
  
  q48 = list(
    item_id = "q48",
    item_text = "No matter how much I give to others, I can never give enough.",
    rateValues = rateValues
  ),
  
  q49 = list(
    item_id = "q49",
    item_text = "Gaining the approval of others is often more important to me than following my own desires.",
    rateValues = rateValues
  ),
  
  q50 = list(
    item_id = "q50",
    item_text = "I want people to like me, so I would tend to agree with people even if I know they are factually wrong.",
    rateValues = rateValues
  ),
  
  q51 = list(
    item_id = "q51",
    item_text = "Even if I don't like someone, I still strongly desire for them to like me.",
    rateValues = rateValues
  ),
  
  q52 = list(
    item_id = "q52",
    item_text = "I find it hard to make a decision unless I know what other people think.",
    rateValues = rateValues
  ),
  
  q53 = list(
    item_id = "q53",
    item_text = "Tuning into my emotions is helpful.",
    rateValues = reverse_rateValues
  ),
  
  q54 = list(
    item_id = "q54",
    item_text = "My emotions do more harm than good.",
    rateValues = rateValues
  ),
  
  q55 = list(
    item_id = "q55",
    item_text = "Emotions are not useful, so I need to ignore them.",
    rateValues = rateValues
  ),
  
  q56 = list(
    item_id = "q56",
    item_text = "It is dangerous to feel emotions too strongly.",
    rateValues = rateValues
  ),
  
  q57 = list(
    item_id = "q57",
    item_text = "Things almost always go wrong for me.",
    rateValues = rateValues
  ),
  
  q58 = list(
    item_id = "q58",
    item_text = "In uncertain times, I usually expect the best.",
    rateValues = reverse_rateValues
  ),
  
  q59 = list(
    item_id = "q59",
    item_text = "Things inevitably don't go my way, so I prefer to expect the worst to avoid disappointment.",
    rateValues = rateValues
  ),
  
  q60 = list(
    item_id = "q60",
    item_text = "I am pessimistic about the future.",
    rateValues = rateValues
  ),
  
  q61 = list(
    item_id = "q61",
    item_text = "If I make a mistake, I can let it go easily.",
    rateValues = reverse_rateValues
  ),
  
  q62 = list(
    item_id = "q62",
    item_text = "Achieving high standards is more important than my own happiness.",
    rateValues = rateValues
  ),
  
  q63 = list(
    item_id = "q63",
    item_text = "I should always perform at an extremely high level.",
    rateValues = rateValues
  ),
  
  q64 = list(
    item_id = "q64",
    item_text = "It is ok for me not to be a high performer.",
    rateValues = reverse_rateValues
  ),
  
  q65 = list(
    item_id = "q65",
    item_text = "I try to be compassionate and understanding to myself when I make a mistake.",
    rateValues = reverse_rateValues
  ),
  
  q66 = list(
    item_id = "q66",
    item_text = "If something goes wrong, I shouldn't get away with it.",
    rateValues = rateValues
  ),
  
  q67 = list(
    item_id = "q67",
    item_text = "If I fail, I should suffer the consequences.",
    rateValues = rateValues
  ),
  
  q68 = list(
    item_id = "q68",
    item_text = "It doesn't matter how small a mistake I make is, I deserve to be punished for it.",
    rateValues = rateValues
  ),
  
  q69 = list(
    item_id = "q69",
    item_text = "I try to be compassionate and understanding to others when they make a mistake.",
    rateValues = reverse_rateValues
  ),
  
  q70 = list(
    item_id = "q70",
    item_text = "People should be held to account for their failings.",
    rateValues = rateValues
  ),
  
  q71 = list(
    item_id = "q71",
    item_text = "If someone fails, they should face the consequences.",
    rateValues = rateValues
  ),
  
  q72 = list(
    item_id = "q72",
    item_text = "People deserve to be disciplined for their mistakes.",
    rateValues = rateValues
  ),
  
  q73 = list(
    item_id = "q73",
    item_text = "When I ask someone for something they should agree to it.",
    rateValues = rateValues
  ),
  
  q74 = list(
    item_id = "q74",
    item_text = "I am above the usual rules that others follow.",
    rateValues = rateValues
  ),
  
  q75 = list(
    item_id = "q75",
    item_text = "Other people should appreciate how unique I am.",
    rateValues = rateValues
  ),
  
  q76 = list(
    item_id = "q76",
    item_text = "I deserve special privileges.",
    rateValues = rateValues
  )
)

survey_config = list(
  title = "MSS-YSQ-Dynamic",
  description = "Below are statements that you might agree or disagree with. Please read each statement and indicate your level of agreement. Try not to spend too much time on a single question or be overly factual in your responses, rather base your answers on what you intuitively feel.",
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
    use_pattern_boundaries = TRUE,  # NEW PARAMETER - TRUE (default) enables pattern-specific for SC-EP, FALSE  forces sum-score boundaries even for SC-EP
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
    use_pattern_boundaries = TRUE,  # NEW PARAMETER - TRUE (default) enables pattern-specific for SC-EP, FALSE  forces sum-score boundaries even for SC-EP
    output_dir = "deployment"
  )
}

# ============================================================================
# End of Module 7
# ============================================================================

# ============================================================================
# Module 8: Deployment Validation Module
# ============================================================================
# Purpose: Validate deployment artifacts by simulating the actual JSON logic
#          and comparing actual performance to predicted performance
# ============================================================================

cat("\n==============================================\n")
cat("=== Module 8: Deployment Validation Module ===\n")
cat("==============================================\n\n")

# validation_results <- validate_deployment(
#   deployment_package = deployment_package,
#   prepared_data = prepared_data,
#   evaluation_results = evaluation_results,
#   optimization_results = optimization_results,
#   output_dir = "deployment_validation"
# )

# Run validation on a random subset
validate_deployment_subset <- function(n_sample = 100) {
  
  # Get test data and sample
  test_data_full <- prepared_data$splits$test
  
  # Random sample
  set.seed(123)  # For reproducibility
  sample_indices <- sample(1:nrow(test_data_full), min(n_sample, nrow(test_data_full)))
  test_data_subset <- test_data_full[sample_indices, ]
  
  cat("Running validation on", nrow(test_data_subset), "respondents (sampled from", nrow(test_data_full), ")\n\n")
  
  # Run validation with subset
  validation_results <- validate_deployment(
    deployment_package = deployment_package,
    prepared_data = prepared_data,
    evaluation_results = evaluation_results,
    optimization_results = optimization_results,
    validation_data = test_data_subset,  # Use subset
    output_dir = "deployment_validation"
  )
  
  return(validation_results)
}

# Run with 100 respondents
validation_results <- validate_deployment_subset(100)

# ============================================================================
# End of Module 8
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