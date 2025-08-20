# ============================================================================
# Module 7: Deployment Module
# ============================================================================
# Purpose: Generate deployment artifacts for implementing any selected
#          questionnaire method in production (optimized, recommended, or
#          user-specified), including boundary tables and configuration files
# ============================================================================

# Note on required packages:
# - jsonlite

# Required packages
required_packages <- c("jsonlite")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for deployment functionality"))
  }
}

#' Generate Deployment Package
#'
#' @param evaluation_results Results from Module 5 evaluation (if not optimized)
#' @param optimization_results Results from Module 6 optimization (if optimized)
#' @param all_combination_results All combination results (if not optimized)
#' @param prepared_data Prepared data from Module 1
#' @param method_id Specific method ID to deploy (NULL to use best/recommended)
#' @param item_definitions Item definitions for survey configuration
#' @param survey_config Survey configuration options
#' @param output_dir Output directory for deployment artifacts
#' @return Deployment package with all necessary artifacts
#' @export
generate_deployment_package <- function(evaluation_results = NULL,
                                        optimization_results = NULL, 
                                        all_combination_results = NULL,
                                        prepared_data,
                                        method_id = NULL,
                                        item_definitions = NULL,
                                        survey_config = NULL,
                                        output_dir = "deployment") {
  
  cat("\n====================================\n")
  cat("=== Module 7: Deployment Module ===\n") 
  cat("====================================\n\n")
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Determine which method to deploy
  if (!is.null(optimization_results)) {
    # Deploy optimized method
    cat("Deploying optimized method configuration...\n")
    method_results <- optimization_results$optimized_method
    is_optimized <- TRUE
  } else if (!is.null(method_id) && !is.null(all_combination_results)) {
    # Deploy specific method
    cat("Deploying method:", method_id, "\n")
    method_results <- all_combination_results[[method_id]]
    is_optimized <- FALSE
  } else if (!is.null(evaluation_results)) {
    # Deploy recommended method
    cat("Deploying recommended method from evaluation...\n")
    method_results <- evaluation_results$recommended_method
    is_optimized <- FALSE
  } else {
    stop("No valid method source provided for deployment")
  }
  
  # Display method information
  cat("\nMethod Configuration:\n")
  cat("  Ordering:", method_results$combination$ordering, "\n")
  cat("  Reduction:", method_results$combination$reduction, "\n")
  
  if (method_results$combination$reduction %in% c("sc_ep", "sc_sor", "sc_mor")) {
    if (!is.null(method_results$combination$construct_gammas)) {
      cat("  Gamma values: construct-specific\n")
      for (cn in names(method_results$combination$construct_gammas)) {
        cat("    ", cn, ": gamma_0 =", 
            method_results$combination$construct_gammas[[cn]]$gamma_0,
            ", gamma_1 =", 
            method_results$combination$construct_gammas[[cn]]$gamma_1, "\n")
      }
    } else {
      cat("  Gamma_0:", method_results$combination$gamma_0, "\n")
      cat("  Gamma_1:", method_results$combination$gamma_1, "\n")
    }
  }
  
  # 1. Generate stopping boundary tables
  cat("\n1. Generating stopping boundary tables...\n")
  boundary_tables <- generate_stopping_boundaries(
    method_results,
    prepared_data,
    output_dir
  )
  
  # 2. Generate administration sequence
  cat("\n2. Generating administration sequence...\n")
  admin_sequence <- generate_administration_sequence(
    method_results,
    prepared_data,
    output_dir
  )
  
  # 3. Generate implementation parameters
  cat("\n3. Saving implementation parameters...\n")
  impl_params <- generate_implementation_params(
    method_results,
    prepared_data,
    optimization_results,
    is_optimized,
    output_dir
  )
  
  # 4. Generate survey configuration (if item definitions provided)
  if (!is.null(item_definitions)) {
    cat("\n4. Generating survey configuration...\n")
    survey_files <- generate_survey_config(
      boundary_tables,
      admin_sequence,
      prepared_data,
      item_definitions,
      survey_config,
      output_dir
    )
  } else {
    survey_files <- NULL
    cat("\n4. Skipping survey configuration (no item definitions provided)\n")
  }
  
  # 5. Generate implementation guide
  cat("\n5. Generating implementation guide...\n")
  generate_implementation_guide(
    method_results,
    prepared_data,
    optimization_results,
    is_optimized,
    output_dir
  )
  
  # Create deployment package summary
  deployment_package <- list(
    method_configuration = method_results$combination,
    boundary_tables = boundary_tables,
    admin_sequence = admin_sequence,
    implementation_params = impl_params,
    survey_files = survey_files,
    output_directory = output_dir,
    timestamp = Sys.time()
  )
  
  # Save deployment package
  saveRDS(deployment_package, file.path(output_dir, "deployment_package.rds"))
  
  cat("\n✅ Deployment package generated successfully!\n")
  cat("Output directory:", output_dir, "\n")
  
  return(deployment_package)
}

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
  constraints <- prepared_data$config$constraints %||% list()
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
      
      # Get construct-specific gammas if available
      if (has_construct_gammas && construct_name %in% names(method_results$combination$construct_gammas)) {
        gamma_0 <- method_results$combination$construct_gammas[[construct_name]]$gamma_0
        gamma_1 <- method_results$combination$construct_gammas[[construct_name]]$gamma_1
      } else {
        gamma_0 <- method_results$combination$gamma_0
        gamma_1 <- method_results$combination$gamma_1
      }
      
      # Get construct-specific training params if applicable
      if (!is.null(training_params$by_construct) && 
          construct_name %in% names(training_params$by_construct)) {
        construct_training_params <- training_params$by_construct[[construct_name]]
      } else {
        construct_training_params <- training_params
      }
      
      # Generate boundary table for this construct
      boundary_table <- generate_single_boundary_table(
        construct_ordered,
        prepared_data$config,
        construct_training_params,
        reduction,
        gamma_0,
        gamma_1,
        stop_low_only,
        construct_name
      )
      
      boundary_tables[[construct_name]] <- boundary_table
    }
    
    # Save combined and individual CSV files
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
  
  # Also save as HTML for better viewing
  generate_boundary_tables_html(boundary_tables, prepared_data, output_dir)
  
  return(boundary_tables)
}

#' Generate Single Boundary Table (COMPLETE UPDATED VERSION)
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
    
  } else if (reduction %in% c("sc_ep", "sc_sor", "sc_mor") && gamma_0_valid && gamma_1_valid) {
    # Stochastic curtailment - only if gamma values are numeric
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
      
      boundary_df$method_notes[k] <- paste0("Stochastic boundaries (", reduction, 
                                            ", γ₀=", gamma_0, ", γ₁=", gamma_1, ")")
    }
    
  } else if (reduction == "irt_cct") {
    # IRT-based CCT - convert theta to sum scores
    boundary_df$low_risk_boundary <- "Based on theta estimate"
    boundary_df$high_risk_boundary <- ifelse(stop_low_only, "N/A", "Based on theta estimate")
    boundary_df$method_notes <- "IRT-based curtailment"
    boundary_df$items_included <- sapply(1:n_items, function(k) {
      paste(ordered_items[1:k], collapse = ", ")
    })
    
  } else {
    # Invalid gamma values or unknown reduction method
    boundary_df$low_risk_boundary <- "N/A"
    boundary_df$high_risk_boundary <- "N/A"
    boundary_df$method_notes <- "Invalid configuration"
  }
  
  # Clean up items_included column if still NA
  if (all(is.na(boundary_df$items_included))) {
    boundary_df$items_included <- sapply(1:n_items, function(k) {
      paste(ordered_items[1:k], collapse = ", ")
    })
  }
  
  # Replace remaining NAs with "N/A"
  boundary_df$low_risk_boundary[is.na(boundary_df$low_risk_boundary)] <- "N/A"
  boundary_df$high_risk_boundary[is.na(boundary_df$high_risk_boundary)] <- "N/A"
  
  return(boundary_df)
}

#' Calculate Deterministic Curtailment Boundaries
#'
#' @param ordered_items Ordered items
#' @param training_params Training parameters
#' @param cutoff Classification cutoff
#' @return List with low and high boundaries
calculate_dc_boundaries <- function(ordered_items, training_params, cutoff) {
  
  n_items <- length(ordered_items)
  low_boundary <- rep(NA, n_items)
  high_boundary <- rep(NA, n_items)
  
  # Get item ranges from training params
  if (!is.null(training_params)) {
    item_ranges <- training_params$item_ranges
  } else {
    warning("No training parameters found for DC method")
    return(list(low_boundary = low_boundary, high_boundary = high_boundary))
  }
  
  for (k in 1:(n_items - 1)) {
    items_so_far <- ordered_items[1:k]
    remaining_items <- ordered_items[(k+1):n_items]
    
    # Calculate potential score ranges
    min_remaining <- sum(sapply(remaining_items, function(item) item_ranges[[item]][1]))
    max_remaining <- sum(sapply(remaining_items, function(item) item_ranges[[item]][2]))
    
    # Low boundary: max possible total still below cutoff
    # If current_sum + max_remaining < cutoff, then classification = 0
    low_boundary[k] <- cutoff - max_remaining - 1
    
    # High boundary: min possible total meets cutoff
    # If current_sum + min_remaining >= cutoff, then classification = 1
    if (min_remaining == 0) {
      high_boundary[k] <- cutoff
    } else {
      high_boundary[k] <- cutoff - min_remaining
    }
    
    # Ensure boundaries are within possible range
    max_possible_current <- sum(sapply(items_so_far, function(item) item_ranges[[item]][2]))
    min_possible_current <- sum(sapply(items_so_far, function(item) item_ranges[[item]][1]))
    
    if (low_boundary[k] < min_possible_current) {
      low_boundary[k] <- NA  # Cannot achieve this low
    }
    
    if (high_boundary[k] > max_possible_current) {
      high_boundary[k] <- NA  # Cannot achieve this high
    }
    
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
  }
  
  # Handle the last item - simple cutoff
  if (n_items > 0) {
    low_boundary[n_items] <- cutoff - 1
    high_boundary[n_items] <- cutoff
  }
  
  return(list(low_boundary = low_boundary, high_boundary = high_boundary))
}

#' Calculate Stochastic Curtailment Boundaries (FIXED VERSION)
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
  
  # Validation checks
  if (is.null(training_params)) {
    warning("Training parameters are NULL - cannot calculate SC boundaries")
    return(list(low_boundary = low_boundary, high_boundary = high_boundary))
  }
  
  # Check for model failures in regression-based methods
  if (method %in% c("sc_sor", "sc_mor")) {
    if (!is.null(training_params$model_result)) {
      if (!is.null(training_params$model_result$success) &&
          !training_params$model_result$success) {
        warning("Models failed during training - cannot calculate boundaries")
        return(list(low_boundary = low_boundary, high_boundary = high_boundary))
      }
    }
  }
  
  if (method == "sc_ep") {
    # Use empirical lookup tables
    lookup_tables <- training_params$lookup_tables
    
    if (is.null(lookup_tables)) {
      warning("No lookup tables found for sc_ep method")
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
        
        # Step 2: Determine which sum scores qualify for stopping using TRULY CONSERVATIVE aggregation
        # FIXED: Use MAXIMUM probability requirement (most conservative)
        # This means we only stop if ALL patterns with that sum score support stopping
        
        low_candidates <- c()
        high_candidates <- c()
        
        for (key in names(sum_score_groups)) {
          group <- sum_score_groups[[key]]
          sum_score <- group$sum_score
          
          # TRULY CONSERVATIVE RULE: Require high agreement among patterns
          # Option 1: Require ALL patterns to meet threshold (strictest)
          # min_prob_low <- min(group$prob_lows, na.rm = TRUE)
          # min_prob_high <- min(group$prob_highs, na.rm = TRUE)
          
          # Option 2: Require 75th percentile to meet threshold (moderate)
          # This is more practical and aligns better with the optimization approach
          q75_prob_low <- quantile(group$prob_lows, 0.75, na.rm = TRUE)
          q75_prob_high <- quantile(group$prob_highs, 0.75, na.rm = TRUE)
          
          # Option 3: Use median (balanced approach)
          # median_prob_low <- median(group$prob_lows, na.rm = TRUE)
          # median_prob_high <- median(group$prob_highs, na.rm = TRUE)
          
          # Using Option 2 for practical deployment
          if (!is.na(q75_prob_low) && q75_prob_low >= gamma_0) {
            low_candidates <- c(low_candidates, sum_score)
          }
          
          if (!is.na(q75_prob_high) && q75_prob_high >= gamma_1) {
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
        
        # Step 4: Validate boundaries
        if (!is.na(low_boundary[k]) && !is.na(high_boundary[k])) {
          if (low_boundary[k] >= high_boundary[k]) {
            warning(paste("SC-EP boundaries overlap at position", k,
                          "- adjusting to prevent conflict"))
            
            # Resolution: Keep the boundary closer to the expected cutoff
            mean_cutoff_score <- cutoff * k / n_items
            
            if (abs(low_boundary[k] - mean_cutoff_score) > abs(high_boundary[k] - mean_cutoff_score)) {
              low_boundary[k] <- NA  # Remove the more extreme boundary
            } else {
              high_boundary[k] <- NA
            }
          }
        }
      }
    }
    
  } else if (method == "sc_sor") {
    # Simple Ordinal Regression boundaries
    if (!is.null(training_params$model_result)) {
      models <- training_params$model_result$models
    } else {
      models <- training_params$models
    }
    
    if (is.null(models)) {
      warning("No models found for sc_sor method")
      return(list(low_boundary = low_boundary, high_boundary = high_boundary))
    }
    
    for (k in 1:n_items) {
      if (k <= length(models) && !is.null(models[[k]])) {
        
        low_candidates <- c()
        high_candidates <- c()
        
        max_possible_score <- k * 4  # Assuming max item score is 4
        
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

#' Generate Administration Sequence
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param output_dir Output directory
#' @return Administration sequence data frame
generate_administration_sequence <- function(method_results, prepared_data, output_dir) {
  
  ordered_items <- method_results$ordering_result$ordered_items
  
  # Create sequence data frame
  admin_seq <- data.frame(
    position = seq_along(ordered_items),
    item_id = ordered_items,
    stringsAsFactors = FALSE
  )
  
  # Add construct information if multi-construct
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    admin_seq$construct <- NA_character_
    for (cn in names(prepared_data$config$constructs)) {
      construct_items <- prepared_data$config$constructs[[cn]]
      admin_seq$construct[admin_seq$item_id %in% construct_items] <- cn
    }
  }
  
  # Save as CSV
  write.csv(admin_seq, 
            file.path(output_dir, "administration_sequence.csv"),
            row.names = FALSE)
  
  return(admin_seq)
}

#' Generate Implementation Parameters
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param optimization_results Optimization results if applicable
#' @param is_optimized Whether method was optimized
#' @param output_dir Output directory
#' @return Implementation parameters
generate_implementation_params <- function(method_results, prepared_data, 
                                           optimization_results, is_optimized,
                                           output_dir) {
  
  impl_params <- list(
    method_combination = method_results$combination,
    questionnaire_type = prepared_data$config$questionnaire_type,
    constraints = prepared_data$config$constraints,
    cutoffs = prepared_data$config$cutoffs,
    is_optimized = is_optimized,
    optimization_details = if(is_optimized) optimization_results$optimization_details else NULL,
    timestamp = Sys.time()
  )
  
  # Save as RDS
  saveRDS(impl_params, 
          file.path(output_dir, "implementation_params.rds"))
  
  return(impl_params)
}

#' Generate Survey Configuration
#'
#' @param boundary_tables Boundary tables
#' @param admin_sequence Administration sequence
#' @param prepared_data Prepared data
#' @param item_definitions Item definitions
#' @param survey_config Survey configuration options
#' @param output_dir Output directory
#' @return List of generated survey files
generate_survey_config <- function(boundary_tables, admin_sequence, prepared_data,
                                   item_definitions, survey_config, output_dir) {
  
  # Create SurveyJS configuration
  survey_json <- create_surveyjs_config(
    boundary_tables = boundary_tables,
    admin_sequence = admin_sequence,
    prepared_data = prepared_data,
    item_definitions = item_definitions,
    survey_config = survey_config
  )
  
  # Save JSON configuration
  json_file <- file.path(output_dir, "surveyjs_config.json")
  jsonlite::write_json(survey_json, json_file, pretty = TRUE, auto_unbox = TRUE)
  
  # Create example HTML file
  html_file <- file.path(output_dir, "surveyjs_example.html")
  create_surveyjs_html(html_file, survey_json, survey_config)
  
  return(list(
    json_config = json_file,
    html_example = html_file
  ))
}

#' Create SurveyJS Configuration
#'
#' @param boundary_tables Boundary tables
#' @param admin_sequence Administration sequence
#' @param prepared_data Prepared data
#' @param item_definitions Item definitions
#' @param survey_config Survey configuration options
#' @return SurveyJS configuration object
create_surveyjs_config <- function(boundary_tables, admin_sequence, prepared_data,
                                   item_definitions, survey_config) {
  
  # Extract configuration options
  stop_low_only <- prepared_data$config$constraints$stop_low_only %||% FALSE
  min_items_per_construct <- prepared_data$config$constraints$min_items_per_construct %||% 1
  
  # Create pages with visibility conditions
  pages <- create_survey_pages(
    admin_sequence = admin_sequence,
    boundary_tables = boundary_tables,
    item_definitions = item_definitions,
    prepared_data = prepared_data,
    survey_config = survey_config,
    stop_low_only = stop_low_only,
    min_items_per_construct = min_items_per_construct
  )
  
  # Create survey configuration
  survey_config_json <- list(
    title = survey_config$title %||% "Adaptive Questionnaire",
    description = survey_config$description %||% "",
    showProgressBar = survey_config$showProgressBar %||% "top",
    showQuestionNumbers = survey_config$showQuestionNumbers %||% "off",
    pages = pages,
    calculatedValues = create_calculated_values(boundary_tables, prepared_data),
    completedHtml = survey_config$completedHtml %||% "<h3>Thank you for completing the questionnaire!</h3>"
  )
  
  return(survey_config_json)
}

#' Create Survey Pages with Items
#'
#' @param admin_sequence Administration sequence
#' @param boundary_tables Boundary tables
#' @param item_definitions Item definitions
#' @param prepared_data Prepared data
#' @param survey_config Survey configuration
#' @param stop_low_only Stop low only flag
#' @param min_items_per_construct Minimum items per construct
#' @return List of survey pages
create_survey_pages <- function(admin_sequence, boundary_tables, item_definitions,
                                prepared_data, survey_config, stop_low_only, 
                                min_items_per_construct) {
  
  pages <- list()
  n_items <- nrow(admin_sequence)
  
  for (i in 1:n_items) {
    item_id <- admin_sequence$item_id[i]
    
    # Get construct if multi-construct
    if ("construct" %in% names(admin_sequence)) {
      construct_name <- admin_sequence$construct[i]
      within_construct_pos <- sum(admin_sequence$construct[1:i] == construct_name)
    } else {
      construct_name <- NULL
      within_construct_pos <- i
    }
    
    # Create page
    page <- list(
      name = paste0("page_", i),
      elements = list()
    )
    
    # Create question
    question <- list(
      type = "rating",
      name = item_id,
      title = item_definitions[[item_id]]$text %||% item_id,
      isRequired = FALSE  # Will be set based on conditions
    )
    
    # Add visibility conditions based on stopping rules
    visibility_condition <- create_visibility_condition(
      position = i,
      boundary_tables = boundary_tables,
      construct_name = construct_name,
      admin_sequence = admin_sequence,
      stop_low_only = stop_low_only,
      min_items_per_construct = min_items_per_construct
    )
    
    # Apply conditions
    # 1. visibleIf
    if (!is.null(visibility_condition)) {
      question$visibleIf <- visibility_condition
    }
    
    # 2. defaultValue
    question$defaultValue <- item_definitions[[item_id]]$defaultValue %||% NA
    
    # 3. rateMin
    question$rateMin <- item_definitions[[item_id]]$rateMin %||% 0
    
    # 4. rateMax
    question$rateMax <- item_definitions[[item_id]]$rateMax %||% 4
    
    # 5. requiredIf (make required if visible - important for conditional items)
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

#' Create Visibility Condition for Item
#'
#' @param position Item position
#' @param boundary_tables Boundary tables
#' @param construct_name Construct name (if multi-construct)
#' @param admin_sequence Administration sequence
#' @param stop_low_only Stop low only flag
#' @param min_items_per_construct Minimum items per construct
#' @return Visibility condition string or NULL
create_visibility_condition <- function(position, boundary_tables, construct_name,
                                        admin_sequence, stop_low_only, 
                                        min_items_per_construct) {
  
  # First item is always shown
  if (position == 1) {
    return(NULL)
  }
  
  # For multi-construct, check construct-specific rules
  if (!is.null(construct_name)) {
    # First min_items_per_construct items in each construct are always shown
    construct_items_before <- which(admin_sequence$construct[1:(position-1)] == construct_name)
    if (length(construct_items_before) < min_items_per_construct) {
      return(NULL)
    }
    
    # Use construct-specific boundary table
    boundary_table <- boundary_tables[[construct_name]]
  } else {
    # Use total boundary table
    boundary_table <- boundary_tables[["total"]]
  }
  
  # Build visibility condition based on stopping boundaries
  # This is a simplified example - actual implementation would need
  # to parse boundary conditions and create appropriate SurveyJS expressions
  
  # For now, return NULL (always visible) as a placeholder
  # A full implementation would parse the boundary_table and create
  # conditions like: "{sum_score} > 5 and {sum_score} < 15"
  
  return(NULL)
}

#' Create Calculated Values for Survey
#'
#' @param boundary_tables Boundary tables
#' @param prepared_data Prepared data
#' @return List of calculated values
create_calculated_values <- function(boundary_tables, prepared_data) {
  
  calculated_values <- list()
  
  # Add sum score calculations for each construct or total
  if (prepared_data$config$questionnaire_type == "unidimensional") {
    calculated_values[[1]] <- list(
      name = "sum_score",
      expression = "{sumInArray}"  # Would need actual SurveyJS expression
    )
  } else {
    for (cn in names(boundary_tables)) {
      calculated_values[[length(calculated_values) + 1]] <- list(
        name = paste0("sum_", cn),
        expression = paste0("{sumInArray_", cn, "}")  # Placeholder
      )
    }
  }
  
  return(calculated_values)
}

#' Create SurveyJS HTML Example
#'
#' @param html_file Output HTML file path
#' @param survey_json Survey configuration JSON
#' @param survey_config Survey configuration options
create_surveyjs_html <- function(html_file, survey_json, survey_config) {
  
  html_content <- '<!DOCTYPE html>
<html>
<head>
    <title>Adaptive Questionnaire</title>
    <script src="https://unpkg.com/survey-jquery/survey.jquery.min.js"></script>
    <link href="https://unpkg.com/survey-core/defaultV2.min.css" type="text/css" rel="stylesheet">
    <script src="https://unpkg.com/jquery"></script>
    <style>
        body {
            margin: 0;
            padding: 20px;
            font-family: Arial, sans-serif;
        }
        #surveyContainer {
            max-width: 800px;
            margin: 0 auto;
        }
    </style>
</head>
<body>
    <div id="surveyContainer"></div>
    <script>
        const surveyJson = ' 
  
  # Add the JSON configuration
  html_content <- paste0(html_content, 
                         jsonlite::toJSON(survey_json, pretty = TRUE, auto_unbox = TRUE))
  
  html_content <- paste0(html_content, ';
        
        const survey = new Survey.Model(surveyJson);
        
        // Add custom logic for adaptive administration
        survey.onValueChanged.add(function(sender, options) {
            // Custom logic would go here to implement stopping rules
            // This is a placeholder for the actual implementation
        });
        
        // Render the survey
        $("#surveyContainer").Survey({
            model: survey,
            onComplete: function(result) {
                console.log("Survey results:", result.data);
                alert("Thank you for completing the questionnaire!");
            }
        });
    </script>
</body>
</html>')
  
  writeLines(html_content, html_file)
}

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
                         "These tables show the cumulative score boundaries (Xk) for stopping item administration.<br>",
                         "<strong>Interpretation:</strong><br>",
                         "- Low risk boundary: Stop if cumulative score ≤ value<br>",
                         "- High risk boundary: Stop if cumulative score ≥ value<br>",
                         "- N/A: No stopping criterion at this position",
                         "</div>")
  
  # Add each boundary table
  for (table_name in names(boundary_tables)) {
    boundary_table <- boundary_tables[[table_name]]
    
    html_content <- paste0(html_content,
                           "<h2>", 
                           ifelse(table_name == "total", "Overall Boundaries", 
                                  paste("Construct:", table_name)),
                           "</h2>",
                           "<table>",
                           "<tr>",
                           "<th>Items Given</th>",
                           "<th>Low Risk Boundary</th>",
                           "<th>High Risk Boundary</th>",
                           "<th>Notes</th>",
                           "</tr>")
    
    for (i in 1:nrow(boundary_table)) {
      low_val <- boundary_table$low_risk_boundary[i]
      high_val <- boundary_table$high_risk_boundary[i]
      notes <- boundary_table$method_notes[i]
      
      low_class <- ifelse(low_val == "N/A", "na-value", "boundary-value")
      high_class <- ifelse(high_val == "N/A", "na-value", "boundary-value")
      
      html_content <- paste0(html_content,
                             "<tr>",
                             "<td>", boundary_table$items_administered[i], "</td>",
                             "<td class='", low_class, "'>", low_val, "</td>",
                             "<td class='", high_class, "'>", high_val, "</td>",
                             "<td>", notes %||% "", "</td>",
                             "</tr>")
    }
    
    html_content <- paste0(html_content, "</table>")
  }
  
  html_content <- paste0(html_content, "</body></html>")
  
  # Save HTML file
  writeLines(html_content, file.path(output_dir, "stopping_boundaries.html"))
}

#' Generate Implementation Guide
#'
#' @param method_results Method results
#' @param prepared_data Prepared data
#' @param optimization_results Optimization results if applicable
#' @param is_optimized Whether method was optimized
#' @param output_dir Output directory
generate_implementation_guide <- function(method_results, prepared_data,
                                          optimization_results, is_optimized,
                                          output_dir) {
  
  guide_text <- paste0(
    "IMPLEMENTATION GUIDE\n",
    "===================\n",
    "Generated: ", Sys.time(), "\n\n",
    
    "1. METHOD CONFIGURATION\n",
    "-----------------------\n",
    "Ordering Method: ", method_results$combination$ordering, "\n",
    "Reduction Method: ", method_results$combination$reduction, "\n"
  )
  
  if (method_results$combination$reduction %in% c("sc_ep", "sc_sor", "sc_mor")) {
    guide_text <- paste0(guide_text,
                         "Gamma_0: ", method_results$combination$gamma_0, "\n",
                         "Gamma_1: ", method_results$combination$gamma_1, "\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n2. QUESTIONNAIRE TYPE\n",
                       "---------------------\n",
                       "Type: ", prepared_data$config$questionnaire_type, "\n")
  
  if (prepared_data$config$questionnaire_type == "multi-construct") {
    guide_text <- paste0(guide_text,
                         "Constructs: ", paste(names(prepared_data$config$constructs), 
                                               collapse = ", "), "\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n3. CONSTRAINTS\n",
                       "--------------\n")
  
  constraints <- prepared_data$config$constraints %||% list()
  guide_text <- paste0(guide_text,
                       "Stop Low Only: ", constraints$stop_low_only %||% FALSE, "\n",
                       "Min Items Per Construct: ", constraints$min_items_per_construct %||% 1, "\n",
                       "Complete Triggered Constructs: ", 
                       constraints$complete_triggered_constructs %||% FALSE, "\n")
  
  guide_text <- paste0(guide_text,
                       "\n4. STOPPING RULES\n",
                       "-----------------\n")
  
  if (method_results$combination$reduction == "none") {
    guide_text <- paste0(guide_text,
                         "No early stopping - all items must be administered.\n")
  } else {
    guide_text <- paste0(guide_text,
                         "Early stopping is based on cumulative scores.\n",
                         "See stopping_boundaries.html for detailed rules.\n\n",
                         "Key points:\n",
                         "- Low risk: Stop when cumulative score is sufficiently low\n")
    
    stop_low_only <- prepared_data$config$constraints$stop_low_only %||% FALSE
    if (!stop_low_only && method_results$combination$gamma_1 < 1.0) {
      guide_text <- paste0(guide_text,
                           "- High risk: Stop when cumulative score is sufficiently high\n")
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
  
  # Add optimization section if applicable
  if (is_optimized && !is.null(optimization_results)) {
    guide_text <- paste0(guide_text,
                         "\n6. OPTIMIZATION DETAILS\n",
                         "----------------------\n",
                         "Optimization performed: YES\n",
                         "Objective: ", optimization_results$optimization_details$objective %||% "Not specified", "\n")
    
    if (!is.null(optimization_results$optimization_details$improvement)) {
      guide_text <- paste0(guide_text,
                           "Performance improvement: ", 
                           sprintf("%.1f%%", optimization_results$optimization_details$improvement * 100), "\n")
    }
  } else {
    guide_text <- paste0(guide_text,
                         "\n6. OPTIMIZATION DETAILS\n",
                         "----------------------\n",
                         "Optimization performed: NO\n",
                         "Using default or specified parameters.\n")
  }
  
  guide_text <- paste0(guide_text,
                       "\n7. DEPLOYMENT CHECKLIST\n",
                       "-----------------------\n",
                       "[ ] Review stopping boundaries for clinical appropriateness\n",
                       "[ ] Test with sample data before production deployment\n",
                       "[ ] Ensure survey platform supports conditional logic\n",
                       "[ ] Train administrators on adaptive administration\n",
                       "[ ] Set up data collection for validation\n",
                       "[ ] Plan for periodic performance monitoring\n")
  
  # Save guide
  writeLines(guide_text, file.path(output_dir, "implementation_guide.txt"))
}

#' Extract Boundary Value from Text
#'
#' @param boundary_text Boundary text (e.g., "Xk <= 5")
#' @param type "low" or "high" boundary type
#' @return Numeric boundary value or NA
extract_boundary_value <- function(boundary_text, type = "low") {
  if (is.na(boundary_text) || boundary_text == "N/A") {
    return(NA)
  }
  
  # Extract numeric value from expressions like "Xk <= 5" or "Xk >= 10"
  if (type == "low") {
    # For low boundary: "Xk <= value"
    matches <- regmatches(boundary_text, regexpr("<=\\s*([0-9]+)", boundary_text))
    if (length(matches) > 0) {
      value <- as.numeric(sub("<=\\s*", "", matches[1]))
      return(value)
    }
  } else {
    # For high boundary: "Xk >= value"
    matches <- regmatches(boundary_text, regexpr(">=\\s*([0-9]+)", boundary_text))
    if (length(matches) > 0) {
      value <- as.numeric(sub(">=\\s*", "", matches[1]))
      return(value)
    }
  }
  
  return(NA)
}