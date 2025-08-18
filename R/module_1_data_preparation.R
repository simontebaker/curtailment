# ============================================================================
# Module 1: Data Preparation Module
# ============================================================================
# Purpose: Validate input data, check structure, and create train/test splits
# ============================================================================

#' Validate Input Data Structure
#'
#' @param data A data frame containing item responses and outcomes
#' @param config A list containing questionnaire configuration
#' @return A list with validation results and cleaned data
#' @export
validate_input_data <- function(data, config) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    data = data
  )
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                   "Input data must be a data frame")
    return(validation_results)
  }
  
  # Extract expected columns from config
  all_items <- unlist(config$constructs)
  outcome_cols <- paste0(names(config$constructs), "_outcome")
  
  # For unidimensional, there's only one outcome
  if (config$questionnaire_type == "unidimensional") {
    outcome_cols <- config$outcome_column
  }
  
  # Check for required columns
  missing_items <- setdiff(all_items, names(data))
  if (length(missing_items) > 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   paste("Missing item columns:", paste(missing_items, collapse = ", ")))
  }
  
  missing_outcomes <- setdiff(outcome_cols, names(data))
  if (length(missing_outcomes) > 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   paste("Missing outcome columns:", paste(missing_outcomes, collapse = ", ")))
  }
  
  # Check for ID
  id_column <- config$id_column %||% "id"  # Use config or use "id" by default
  if (!id_column %in% names(data)) {
    validation_results$warnings <- c(validation_results$warnings,
                                     paste0("No ", id_column, " column found. Creating sequential IDs."))
    data[[id_column]] <- seq_len(nrow(data))
  }
  
  # Validate item responses are numeric
  item_cols <- intersect(all_items, names(data))
  non_numeric_items <- item_cols[!sapply(data[item_cols], is.numeric)]
  if (length(non_numeric_items) > 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                   paste("Non-numeric item columns:", paste(non_numeric_items, collapse = ", ")))
  }
  
  # Validate outcomes are binary
  existing_outcomes <- intersect(outcome_cols, names(data))
  for (outcome in existing_outcomes) {
    unique_vals <- unique(data[[outcome]][!is.na(data[[outcome]])])
    if (!all(unique_vals %in% c(0, 1))) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors,
                                     paste(outcome, "is not binary (0/1)"))
    }
  }
  
  # Check for missing data
  item_missing <- sapply(data[item_cols], function(x) sum(is.na(x)))
  if (any(item_missing > 0)) {
    validation_results$warnings <- c(validation_results$warnings,
                                     paste("Missing data found in items:", 
                                           paste(names(item_missing)[item_missing > 0], collapse = ", ")))
  }
  
  # Fix:/Update: (Added base rate validation section)
  # Validate base rates if provided
  if (!is.null(config$base_rates)) {
    # Check that base_rates is a list
    if (!is.list(config$base_rates)) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors,
                                     "base_rates must be a list")
    } else {
      # For multi-construct, check that base rates match constructs
      if (config$questionnaire_type == "multi-construct") {
        missing_base_rates <- setdiff(names(config$constructs), names(config$base_rates))
        extra_base_rates <- setdiff(names(config$base_rates), names(config$constructs))
        
        if (length(missing_base_rates) > 0) {
          validation_results$warnings <- c(validation_results$warnings,
                                           paste("Missing base rates for constructs:", 
                                                 paste(missing_base_rates, collapse = ", "),
                                                 ". Will use training data prevalence."))
        }
        
        if (length(extra_base_rates) > 0) {
          validation_results$warnings <- c(validation_results$warnings,
                                           paste("Extra base rates provided for non-existent constructs:", 
                                                 paste(extra_base_rates, collapse = ", ")))
        }
      }
      
      # Validate base rate values
      for (br_name in names(config$base_rates)) {
        br_value <- config$base_rates[[br_name]]
        if (!is.numeric(br_value) || br_value < 0 || br_value > 1) {
          validation_results$valid <- FALSE
          validation_results$errors <- c(validation_results$errors,
                                         paste("Base rate for", br_name, 
                                               "must be numeric between 0 and 1"))
        }
      }
    }
  }
  
  validation_results$data <- data
  return(validation_results)
}

# Helper function for NULL-coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create Train/Test Split
#'
#' @param data Validated data frame
#' @param method Split method: "random", "specified", or "kfold"
#' @param split_ratio Proportion for training data (for random split)
#' @param train_rows Vector of row indices for training (for specified split)
#' @param k Number of folds (for k-fold CV)
#' @param seed Random seed for reproducibility
#' @return A list containing split information
#' @export
create_data_split <- function(data, 
                              method = c("random", "specified", "kfold"),
                              split_ratio = 0.7,
                              train_rows = NULL,
                              k = 5,
                              seed = 123) {
  
  method <- match.arg(method)
  n <- nrow(data)
  
  set.seed(seed)
  
  if (method == "random") {
    # Simple random split
    train_idx <- sample(n, size = floor(n * split_ratio), replace = FALSE)
    test_idx <- setdiff(seq_len(n), train_idx)
    
    splits <- list(
      train = data[train_idx, ],
      test = data[test_idx, ],
      train_idx = train_idx,
      test_idx = test_idx,
      method = "random",
      n_splits = 1
    )
    
  } else if (method == "specified") {
    # User-specified rows
    if (is.null(train_rows)) {
      stop("train_rows must be provided for specified split method")
    }
    
    test_idx <- setdiff(seq_len(n), train_rows)
    
    splits <- list(
      train = data[train_rows, ],
      test = data[test_idx, ],
      train_idx = train_rows,
      test_idx = test_idx,
      method = "specified",
      n_splits = 1
    )
    
  } else if (method == "kfold") {
    # K-fold cross-validation
    fold_indices <- sample(rep(seq_len(k), length.out = n))
    
    splits <- list(
      folds = fold_indices,
      k = k,
      method = "kfold",
      n_splits = k,
      get_fold = function(fold_num) {
        test_idx <- which(fold_indices == fold_num)
        train_idx <- which(fold_indices != fold_num)
        list(
          train = data[train_idx, ],
          test = data[test_idx, ],
          train_idx = train_idx,
          test_idx = test_idx
        )
      }
    )
  }
  
  return(splits)
}

#' Prepare Data for Analysis
#'
#' @param data_path Path to data file or data frame
#' @param config Questionnaire configuration list
#' @param split_method Method for creating train/test split
#' @param ... Additional arguments passed to create_data_split
#' @return A list containing validated data and split information
#' @export
prepare_data <- function(data_path, config, split_method = "random", ...) {
  
  # Load data if path provided
  if (is.character(data_path)) {
    if (grepl("\\.csv$", data_path)) {
      data <- read.csv(data_path, stringsAsFactors = FALSE)
    } else if (grepl("\\.rds$", data_path)) {
      data <- readRDS(data_path)
    } else {
      stop("Unsupported file format. Use .csv or .rds")
    }
  } else {
    data <- data_path
  }
  
  # Validate data
  validation <- validate_input_data(data, config)
  
  if (!validation$valid) {
    stop(paste("Data validation failed:\n", 
               paste(validation$errors, collapse = "\n")))
  }
  
  if (length(validation$warnings) > 0) {
    warning(paste("Data validation warnings:\n",
                  paste(validation$warnings, collapse = "\n")))
  }
  
  # Create splits
  splits <- create_data_split(validation$data, method = split_method, ...)
  
  cat(sprintf("\nSplits: %d training, %d test samples\n", 
              nrow(splits$train), 
              nrow(splits$test)))
  
  # Fix:/Update: (Updated prepare_data() function to handle base rates)
  # Handle base rates
  if (is.null(config$base_rates)) {
    # Calculate from training data
    calculated_base_rates <- calculate_base_rates(splits$train, config)
    config$base_rates <- calculated_base_rates
    cat("\nBase rates calculated from training data:\n")
    for (br_name in names(calculated_base_rates)) {
      cat(sprintf("  %s: %.3f\n", br_name, calculated_base_rates[[br_name]]))
    }
  } else {
    # Use provided base rates
    cat("\nUsing provided reference base rates:\n")
    for (br_name in names(config$base_rates)) {
      cat(sprintf("  %s: %.3f\n", br_name, config$base_rates[[br_name]]))
    }
    
    # Fill in any missing base rates from training data
    if (config$questionnaire_type == "multi-construct") {
      for (construct_name in names(config$constructs)) {
        if (!construct_name %in% names(config$base_rates)) {
          outcome_col <- paste0(construct_name, "_outcome")
          if (outcome_col %in% names(splits$train)) {
            config$base_rates[[construct_name]] <- mean(splits$train[[outcome_col]], na.rm = TRUE)
            cat(sprintf("  %s: %.3f (calculated from training data)\n", 
                            construct_name, config$base_rates[[construct_name]]))
          }
        }
      }
    }
  }
  
  # Store both reference and sample base rates
  config$reference_base_rates <- config$base_rates
  config$sample_base_rates <- calculate_base_rates(splits$train, config)
  
  return(list(
    data = validation$data,
    splits = splits,
    config = config,
    validation = validation
  ))
}

# Fix:/Update: (Added new function to calculate base rates from data)
#' Calculate Base Rates from Training Data
#'
#' @param train_data Training data frame
#' @param config Data configuration
#' @return List of base rates
#' @export
calculate_base_rates <- function(train_data, config) {
  
  if (config$questionnaire_type == "unidimensional") {
    # Single outcome
    outcome_col <- config$outcome_column
    if (outcome_col %in% names(train_data)) {
      base_rate <- mean(train_data[[outcome_col]], na.rm = TRUE)
      return(list(total = base_rate))
    } else {
      warning("Outcome column not found. Using 0.5 as default base rate.")
      return(list(total = 0.5))
    }
  } else {
    # Multi-construct
    base_rates <- list()
    for (construct_name in names(config$constructs)) {
      outcome_col <- paste0(construct_name, "_outcome")
      if (outcome_col %in% names(train_data)) {
        base_rates[[construct_name]] <- mean(train_data[[outcome_col]], na.rm = TRUE)
      } else {
        warning(paste("Outcome column", outcome_col, "not found. Using 0.5 as default."))
        base_rates[[construct_name]] <- 0.5
      }
    }
    return(base_rates)
  }
}

# ============================================================================
# Example Usage
# ============================================================================

# # Example configuration for PID-5-SF
# config_pid5sf <- list(
#   questionnaire_type = "multi-construct",
#   constructs = list(
#     anhedonia = c("q1", "q2", "q3", "q4"),
#     anxiousness = c("q5", "q6", "q7", "q8")
#     # ... add all 25 facets
#   )
# )
# 
# # Example configuration for unidimensional questionnaire
# config_unidim <- list(
#   questionnaire_type = "unidimensional",
#   constructs = list(
#     total = paste0("q", 1:20)
#   ),
#   outcome_column = "depression_outcome"
# )
# 
# # Prepare data
# prepared_data <- prepare_data(
#   data_path = "pid5sf_data.csv",
#   config = config_pid5sf,
#   split_method = "kfold",
#   k = 5,
#   seed = 42
# )

# # Facet definitions (Facet names and corresponding item/question numbers)
# facet_definitions <- list(
#   anhedonia = c("q9", "q11", "q43", "q65"),
#   anxiousness = c("q24", "q36", "q48", "q78"),
#   attention_seeking = c("q23", "q77", "q87", "q97"),
#   callousness = c("q7", "q62", "q72", "q82"),
#   deceitfulness = c("q18", "q51", "q95", "q99"),
#   depressivity = c("q26", "q60", "q70", "q74"),
#   distractability = c("q39", "q49", "q55", "q91"),
#   eccentricity = c("q10", "q22", "q61", "q94"),
#   emotional_lability = c("q41", "q53", "q71", "q81"),
#   grandiosity = c("q14", "q37", "q85", "q90"),
#   hostility = c("q12", "q31", "q66", "q75"),
#   impulsivity = c("q2", "q5", "q6", "q8"),
#   intimacy_avoidance = c("q29", "q40", "q56", "q93"),
#   irresponsibility = c("q47", "q64", "q68", "q76"),
#   manipulativeness = c("q35", "q44", "q69", "q100"),
#   perceptual_dysregulation = c("q15", "q63", "q88", "q98"),
#   perseveration = c("q19", "q25", "q32", "q46"),
#   restricted_affectivity = c("q28", "q30", "q73", "q83"),
#   rigid_perfectionism = c("q33", "q42", "q80", "q89"),
#   risk_taking = c("q13", "q16", "q21", "q67"),
#   separation_insecurity = c("q17", "q45", "q58", "q79"),
#   submissiveness = c("q3", "q4", "q20", "q92"),
#   suspiciousness = c("q1", "q38", "q50", "q86"),
#   unusual_beliefs_and_experiences = c("q34", "q54", "q59", "q96"),
#   withdrawal = c("q27", "q52", "q57", "q84")
# )

# # Configuration
# config_pid5sf <- list(
#   questionnaire_type = "multi-construct",
#   id_column = "client_id",
#   constructs = facet_definitions
# )

# # Prepare data
# prepared_data <- prepare_data(
#   data_path = "pid-5-sf_data_for_curtailment.csv",
#   config = config_pid5sf,
#   split_method = "random",
#   split_ratio = 0.6,
#   seed = 42
# )
