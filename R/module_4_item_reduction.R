# ============================================================================
# Module 4: Item Reduction Module
# ============================================================================
# Purpose: Implement curtailment algorithms that determine when to stop item
#          administration based on accumulated responses. Each method works
#          with any ordering method from Module 3.
# ============================================================================

# Note on required packages:
# - MASS

# Required packages
required_packages <- c("MASS")  # For ordinal regression
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for full functionality"))
  }
}

#' Apply Item Reduction Method
#'
#' @param ordered_items Vector of item names in administration order
#' @param data Training or test data frame
#' @param config Configuration
#' @param method Reduction method name
#' @param method_params Additional parameters for the reduction method
#' @param training_params Pre-computed parameters from training (NULL for training phase)
#' @return A list containing reduction results and method-specific information
#' @export
reduce_items <- function(ordered_items, data, config, method = "none", 
                         method_params = list(), training_params = NULL) {
  
  # Validate inputs
  if (!method %in% c("none", "dc", "sc_ep", "sc_sor", "sc_mor", "irt_cct")) {
    stop(paste("Unknown reduction method:", method))
  }
  
  # Validate cutoffs are provided in config
  if (is.null(config$cutoffs)) {
    stop("Cutoffs must be specified in config")
  }
  
  # Extract constraints from config if available
  constraints <- config$constraints %||% list()
  stop_low_only <- constraints$stop_low_only %||% FALSE
  min_items_per_construct <- constraints$min_items_per_construct %||% 1
  complete_triggered_constructs <- constraints$complete_triggered_constructs %||% FALSE
  
  # Route based on questionnaire type
  if (config$questionnaire_type == "unidimensional") {
    # For unidimensional: use global cutoff and apply method across all items
    cutoff <- config$cutoffs[["total"]] %||% config$cutoffs[[1]]
    
    result <- switch(method,
                     none = reduce_none(ordered_items, data, config, method_params, cutoff),
                     dc = reduce_dc(ordered_items, data, config, method_params, cutoff, 
                                    training_params, stop_low_only, min_items_per_construct),
                     sc_ep = reduce_sc_ep(ordered_items, data, config, method_params, cutoff, 
                                          training_params, stop_low_only, min_items_per_construct),
                     sc_sor = reduce_sc_sor(ordered_items, data, config, method_params, cutoff, 
                                            training_params, stop_low_only, min_items_per_construct),
                     sc_mor = reduce_sc_mor(ordered_items, data, config, method_params, cutoff, 
                                            training_params, stop_low_only, min_items_per_construct),
                     irt_cct = reduce_irt_cct(ordered_items, data, config, method_params, cutoff, 
                                              training_params, stop_low_only, min_items_per_construct)
    )
  } else {
    # For multi-construct: apply reduction separately to each construct
    result <- reduce_multi_construct(ordered_items, data, config, method, 
                                     method_params, training_params, stop_low_only, 
                                     min_items_per_construct, complete_triggered_constructs)
  }
  
  # Note: complete_triggered_constructs is now handled WITHIN the reduction methods,
  # not as a post-hoc adjustment
  
  # Add metadata
  result$method <- method
  result$questionnaire_type <- config$questionnaire_type
  result$constraints_applied <- constraints
  result$timestamp <- Sys.time()
  
  return(result)
}

# ============================================================================
# Multi-construct Wrapper Function
# ============================================================================

#' Apply Reduction Method to Multi-construct Questionnaire
#'
#' @param ordered_items Vector of all ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method Reduction method name
#' @param method_params Method parameters
#' @param training_params Training parameters (may be construct-specific)
#' @param stop_low_only Whether to only stop for low-risk
#' @param min_items_per_construct Minimum items per construct
#' @param complete_triggered_constructs Whether to complete triggered constructs
#' @return Combined reduction results across all constructs
reduce_multi_construct <- function(ordered_items, data, config, method, method_params,
                                   training_params, stop_low_only, min_items_per_construct,
                                   complete_triggered_constructs = FALSE) {
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize combined results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  construct_classifications <- list()
  construct_stopped_at <- list()
  
  # Initialize tracking for triggered constructs
  # Each respondent has their own list of triggered constructs
  triggered_constructs <- vector("list", n_respondents)
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    # Get items for this construct
    construct_items <- config$constructs[[construct_name]]
    construct_ordered <- intersect(ordered_items, construct_items)
    
    # Get construct-specific cutoff
    construct_cutoff <- config$cutoffs[[construct_name]]
    if (is.null(construct_cutoff)) {
      warning(paste("No cutoff found for construct:", construct_name))
      next
    }
    
    # Get construct-specific training params if available
    construct_training_params <- NULL
    if (!is.null(training_params) && construct_name %in% names(training_params)) {
      construct_training_params <- training_params[[construct_name]]
    } else if (!is.null(training_params) && !"none" == method) {
      # For backward compatibility, use global training params if not construct-specific
      construct_training_params <- training_params
    }
    
    # Apply reduction method to this construct
    # Choose between integrated and original methods based on complete_triggered_constructs
    if (complete_triggered_constructs) {
      # Use integrated methods that handle triggered construct logic
      construct_result <- switch(method,
                                 none = reduce_none(construct_ordered, data, config, method_params, construct_cutoff),
                                 dc = reduce_dc_integrated(construct_ordered, data, config, method_params, construct_cutoff, 
                                                           construct_training_params, stop_low_only, min_items_per_construct,
                                                           complete_triggered_constructs, triggered_constructs, construct_name),
                                 sc_ep = reduce_sc_ep_integrated(construct_ordered, data, config, method_params, construct_cutoff, 
                                                                 construct_training_params, stop_low_only, min_items_per_construct,
                                                                 complete_triggered_constructs, triggered_constructs, construct_name),
                                 sc_sor = reduce_sc_sor_integrated(construct_ordered, data, config, method_params, construct_cutoff, 
                                                                   construct_training_params, stop_low_only, min_items_per_construct,
                                                                   complete_triggered_constructs, triggered_constructs, construct_name),
                                 sc_mor = reduce_sc_mor_integrated(construct_ordered, data, config, method_params, construct_cutoff, 
                                                                   construct_training_params, stop_low_only, min_items_per_construct,
                                                                   complete_triggered_constructs, triggered_constructs, construct_name),
                                 irt_cct = reduce_irt_cct_integrated(construct_ordered, data, config, method_params, construct_cutoff, 
                                                                     construct_training_params, stop_low_only, min_items_per_construct,
                                                                     complete_triggered_constructs, triggered_constructs, construct_name)
      )
    } else {
      # Use original methods without triggered construct logic
      construct_result <- switch(method,
                                 none = reduce_none(construct_ordered, data, config, method_params, construct_cutoff),
                                 dc = reduce_dc(construct_ordered, data, config, method_params, construct_cutoff, 
                                                construct_training_params, stop_low_only, min_items_per_construct),
                                 sc_ep = reduce_sc_ep(construct_ordered, data, config, method_params, construct_cutoff, 
                                                      construct_training_params, stop_low_only, min_items_per_construct),
                                 sc_sor = reduce_sc_sor(construct_ordered, data, config, method_params, construct_cutoff, 
                                                        construct_training_params, stop_low_only, min_items_per_construct),
                                 sc_mor = reduce_sc_mor(construct_ordered, data, config, method_params, construct_cutoff, 
                                                        construct_training_params, stop_low_only, min_items_per_construct),
                                 irt_cct = reduce_irt_cct(construct_ordered, data, config, method_params, construct_cutoff, 
                                                          construct_training_params, stop_low_only, min_items_per_construct)
      )
    }
    
    # Update triggered_constructs from the result
    if (!is.null(construct_result$triggered_constructs)) {
      triggered_constructs <- construct_result$triggered_constructs
    }
    
    # # Fix:/Update:
    # # Map construct results back to full item matrix
    # for (item in construct_ordered) {
    #   item_idx <- which(ordered_items == item)
    #   construct_item_idx <- which(construct_ordered == item)
    #   if (length(item_idx) == 1 && length(construct_item_idx) == 1) {
    #     # Additional bounds check
    #     if (construct_item_idx <= ncol(construct_result$items_administered)) {
    #       items_administered[, item_idx] <- construct_result$items_administered[, construct_item_idx]
    #     }
    #   }
    # }
    
    # Fix:/Update: (Improved mapping code: More robust error handling; Handles edge cases; Better bounds checking)
    # Map construct results back to full item matrix
    for (item in construct_ordered) {
      item_idx <- which(ordered_items == item)
      construct_item_idx <- which(construct_ordered == item)
      
      # Check that we found valid indices
      if (length(item_idx) > 0 && length(construct_item_idx) > 0) {
        # Take first match if multiple (shouldn't happen with proper item naming)
        item_idx <- item_idx[1]
        construct_item_idx <- construct_item_idx[1]
        
        # Additional bounds check
        if (item_idx <= ncol(items_administered) && 
            construct_item_idx <= ncol(construct_result$items_administered)) {
          items_administered[, item_idx] <- construct_result$items_administered[, construct_item_idx]
        }
      }
    }
    
    # Store construct-specific results
    construct_classifications[[construct_name]] <- construct_result$classifications
    construct_stopped_at[[construct_name]] <- construct_result$stopped_at
    
    # Store training params if this is training phase
    if (is.null(training_params) && !is.null(construct_result$training_params)) {
      if (!exists("new_training_params")) new_training_params <- list()
      new_training_params[[construct_name]] <- construct_result$training_params
    }
  }
  
  # # DEBUG: Print diagnostic information for first 3 respondents
  # if (exists("DEBUG_CURTAILMENT") && DEBUG_CURTAILMENT) {
  #   cat("\n=== CURTAILMENT DEBUG INFO ===\n")
  #   cat("Method:", method, "\n")
  #   cat("stop_low_only:", stop_low_only, "\n")
  #   cat("complete_triggered_constructs:", complete_triggered_constructs, "\n")
  #   if (method %in% c("sc_ep", "sc_sor", "sc_mor")) {
  #     cat("gamma_0:", method_params$gamma_0, "gamma_1:", method_params$gamma_1, "\n")
  #   }
  #   cat("\nFirst 3 respondents:\n")
  #   for (i in 1:min(3, n_respondents)) {
  #     cat("\nRespondent", i, ":\n")
  #     cat("  Total items administered:", sum(items_administered[i,]), "of", n_items, "\n")
  #     cat("  Triggered constructs:", paste(triggered_constructs[[i]], collapse=", "), "\n")
  #     cat("  Construct classifications:\n")
  #     for (cn in names(construct_classifications)) {
  #       cat("    ", cn, ":", construct_classifications[[cn]][i], "\n")
  #     }
  #     cat("  Items per construct:\n")
  #     for (cn in names(config$constructs)) {
  #       construct_items <- config$constructs[[cn]]
  #       items_given <- sum(items_administered[i, which(ordered_items %in% construct_items)])
  #       cat("    ", cn, ":", items_given, "of", length(construct_items), "\n")
  #     }
  #   }
  #   cat("=== END DEBUG INFO ===\n\n")
  # }
  
  # Calculate overall statistics
  n_items_used <- rowSums(items_administered)
  
  # For overall classification, could use various rules:
  # Here we use "any positive" rule - if any construct is positive, overall is positive
  overall_classifications <- apply(do.call(cbind, construct_classifications), 1, 
                                   function(x) as.numeric(any(x == 1, na.rm = TRUE)))
  
  result <- list(
    items_administered = items_administered,
    classifications = overall_classifications,  # Overall classification
    construct_classifications = construct_classifications,  # Per-construct classifications
    stopped_at = n_items_used,  # Total items administered
    construct_stopped_at = construct_stopped_at,  # Per-construct stopping points
    n_items_used = n_items_used,
    triggered_constructs = triggered_constructs,  # Which constructs were triggered per respondent
    method_info = list(
      description = paste("Multi-construct reduction using", method, 
                          ifelse(complete_triggered_constructs, " (with triggered construct completion)", "")),
      n_constructs = length(config$constructs),
      constructs_assessed = names(config$constructs)
    )
  )
  
  # Add training parameters if they were created
  if (exists("new_training_params")) {
    result$training_params <- new_training_params
  } else {
    result$training_params <- training_params
  }
  
  return(result)
}

# ============================================================================
# Original Reduction Methods (for unidimensional or when complete_triggered_constructs = FALSE)
# ============================================================================

#' No Reduction - Full Administration
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters
#' @param cutoff Classification cutoff
#' @return Reduction results
reduce_none <- function(ordered_items, data, config, method_params, cutoff) {
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Calculate final classifications based on full data
  classifications <- calculate_classifications(data, ordered_items, config, cutoff)
  
  # Everyone gets all items
  items_administered <- matrix(TRUE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  
  stopped_at <- rep(n_items, n_respondents)
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rep(n_items, n_respondents),
    method_info = list(
      description = "No reduction - full questionnaire administration"
    ),
    training_params = NULL
  )
  
  return(result)
}

#' Deterministic Curtailment
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters
#' @param cutoff Classification cutoff
#' @param training_params Parameters from training phase
#' @param stop_low_only Whether to only stop for low-risk classification
#' @param min_items_per_construct Minimum items per construct
#' @return Reduction results
reduce_dc <- function(ordered_items, data, config, method_params, cutoff, 
                      training_params, stop_low_only, min_items_per_construct) {
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Get scale information from training if available, otherwise compute
  if (!is.null(training_params)) {
    item_ranges <- training_params$item_ranges
  } else {
    item_ranges <- lapply(ordered_items, function(item) {
      if (item %in% names(data)) {
        range(data[[item]], na.rm = TRUE)
      } else {
        c(0, 3)  # Default range for Likert-type items
      }
    })
    names(item_ranges) <- ordered_items
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    current_sum <- 0
    items_given <- character()
    
    # Check items sequentially
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # Check construct constraints for multi-construct
      if (config$questionnaire_type == "multi-construct") {
        if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
          # Must administer this item due to constraints
          items_administered[i, j] <- TRUE
          items_given <- c(items_given, item)
          if (item %in% names(data) && !is.na(data[i, item])) {
            current_sum <- current_sum + data[i, item]
          }
          next
        }
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        current_sum <- current_sum + data[i, item]
      }
      
      # Calculate bounds on possible total score
      if (j < n_items) {
        remaining_items <- ordered_items[(j+1):n_items]
        min_possible <- current_sum + sum(sapply(remaining_items, function(x) item_ranges[[x]][1]))
        max_possible <- current_sum + sum(sapply(remaining_items, function(x) item_ranges[[x]][2]))
      } else {
        min_possible <- current_sum
        max_possible <- current_sum
      }
      
      # Check if classification is determined
      if (max_possible < cutoff) {
        # Will definitely be low-risk
        classifications[i] <- 0
        stopped_at[i] <- j
        break
      } else if (min_possible >= cutoff) {
        # Will definitely be high-risk
        if (!stop_low_only) {
          classifications[i] <- 1
          stopped_at[i] <- j
          break
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
  }
  
  # Store training parameters if this is training phase
  if (is.null(training_params)) {
    training_params <- list(
      item_ranges = item_ranges,
      cutoff = cutoff
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    method_info = list(
      description = "Deterministic curtailment based on score bounds",
      stop_low_only = stop_low_only
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Empirical Proportions
#' #'
#' #' @param ordered_items Vector of ordered item names
#' #' @param data Data frame
#' #' @param config Configuration
#' #' @param method_params Method parameters (gamma_0, gamma_1)
#' #' @param cutoff Classification cutoff
#' #' @param training_params Parameters from training phase
#' #' @param stop_low_only Whether to only stop for low-risk classification
#' #' @param min_items_per_construct Minimum items per construct
#' #' @return Reduction results
#' reduce_sc_ep <- function(ordered_items, data, config, method_params, cutoff, 
#'                          training_params, stop_low_only, min_items_per_construct) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Build or use lookup tables
#'   if (!is.null(training_params)) {
#'     lookup_tables <- training_params$lookup_tables
#'   } else {
#'     # Build empirical probability tables from training data
#'     lookup_tables <- build_empirical_tables(data, ordered_items, config, cutoff)
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     responses <- numeric()
#'     items_given <- character()
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # Check construct constraints
#'       if (config$questionnaire_type == "multi-construct") {
#'         if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'           items_administered[i, j] <- TRUE
#'           items_given <- c(items_given, item)
#'           if (item %in% names(data) && !is.na(data[i, item])) {
#'             responses <- c(responses, data[i, item])
#'           }
#'           next
#'         }
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         responses <- c(responses, data[i, item])
#'       }
#'       
#'       # Look up conditional probabilities
#'       response_pattern <- paste(responses, collapse = "_")
#'       
#'       if (response_pattern %in% names(lookup_tables[[j]])) {
#'         probs <- lookup_tables[[j]][[response_pattern]]
#'         prob_low <- probs["prob_low"]
#'         prob_high <- probs["prob_high"]
#'         
#'         # Check stopping criteria
#'         if (prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (prob_high >= gamma_1 && !stop_low_only) {
#'           classifications[i] <- 1
#'           stopped_at[i] <- j
#'           break
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         total_score <- sum(responses)
#'         classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       lookup_tables = lookup_tables,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     method_info = list(
#'       description = "Stochastic curtailment using empirical conditional probabilities",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

#' Stochastic Curtailment - Empirical Proportions (CORRECTED)
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters (gamma_0, gamma_1)
#' @param cutoff Classification cutoff
#' @param training_params Parameters from training phase
#' @param stop_low_only Whether to only stop for low-risk classification
#' @param min_items_per_construct Minimum items per construct
#' @return Reduction results
reduce_sc_ep <- function(ordered_items, data, config, method_params, cutoff, 
                         training_params, stop_low_only, min_items_per_construct) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)  # Initialized to zeros
  classifications <- numeric(n_respondents)
  
  # Build or use lookup tables
  if (!is.null(training_params)) {
    lookup_tables <- training_params$lookup_tables
  } else {
    # Build empirical probability tables from training data
    lookup_tables <- build_empirical_tables(data, ordered_items, config, cutoff)
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    responses <- numeric()
    items_given <- character()
    items_count <- 0  # EXPLICIT COUNTER for items administered
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # Check construct constraints
      if (config$questionnaire_type == "multi-construct") {
        if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
          # Must administer this item due to constraints
          items_administered[i, j] <- TRUE
          items_given <- c(items_given, item)
          items_count <- items_count + 1  # INCREMENT COUNTER
          
          if (item %in% names(data) && !is.na(data[i, item])) {
            responses <- c(responses, data[i, item])
          }
          next  # Skip stopping check for this item
        }
      }
      
      # Administer item j
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      items_count <- items_count + 1  # INCREMENT COUNTER
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses <- c(responses, data[i, item])
      }
      
      # After administering item j, we have j responses
      # Check if we should stop based on the pattern so far
      
      # Look up conditional probabilities for pattern of length j
      response_pattern <- paste(responses, collapse = "_")
      
      if (response_pattern %in% names(lookup_tables[[j]])) {
        probs <- lookup_tables[[j]][[response_pattern]]
        prob_low <- probs["prob_low"]
        prob_high <- probs["prob_high"]
        
        # Check stopping criteria
        if (prob_low >= gamma_0) {
          # Low-risk classification - stop here
          classifications[i] <- 0
          stopped_at[i] <- items_count  # Use explicit counter
          break  # Stop administering items
        } else if (prob_high >= gamma_1 && !stop_low_only) {
          # High-risk classification - stop here (if allowed)
          classifications[i] <- 1
          stopped_at[i] <- items_count  # Use explicit counter
          break  # Stop administering items
        }
      }
      
      # If we've reached the last item and haven't stopped yet
      if (j == n_items) {
        # Calculate final classification based on total score
        total_score <- sum(responses)
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
        stopped_at[i] <- items_count  # Use explicit counter
      }
    }
    
    # SAFETY CHECK: Ensure stopped_at was set
    if (stopped_at[i] == 0) {
      # This should not happen, but if it does, use the actual count
      stopped_at[i] <- items_count
      # Calculate classification if not set
      if (length(responses) > 0) {
        total_score <- sum(responses)
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
      }
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      lookup_tables = lookup_tables,
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  # VERIFICATION: Check that stopped_at matches n_items_used
  n_items_used <- rowSums(items_administered)
  if (!all(stopped_at == n_items_used)) {
    warning("Mismatch between stopped_at and n_items_used detected!")
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = n_items_used,
    method_info = list(
      description = "Stochastic curtailment using empirical conditional probabilities",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Simple Ordinal Regression
#' #'
#' #' @param ordered_items Vector of ordered item names
#' #' @param data Data frame
#' #' @param config Configuration
#' #' @param method_params Method parameters (gamma_0, gamma_1)
#' #' @param cutoff Classification cutoff
#' #' @param training_params Parameters from training phase
#' #' @param stop_low_only Whether to only stop for low-risk classification
#' #' @param min_items_per_construct Minimum items per construct
#' #' @return Reduction results
#' reduce_sc_sor <- function(ordered_items, data, config, method_params, cutoff, 
#'                           training_params, stop_low_only, min_items_per_construct) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Fix:/Updated:
#'   # Build or use regression models
#'   if (!is.null(training_params)) {
#'     models <- training_params$models
#'     # Ensure it has correct length even from training params
#'     if (is.null(models) || length(models) < n_items) {
#'       if (is.null(models)) {
#'         models <- vector("list", n_items)
#'       } else {
#'         models <- c(models, vector("list", n_items - length(models)))
#'       }
#'     }
#'   } else {
#'     # Build regression models for each stopping point
#'     models <- build_ordinal_models(data, ordered_items, config, cutoff, type = "simple")
#'     
#'     # Handle case where build_ordinal_models returns NULL or wrong length
#'     if (is.null(models) || length(models) == 0) {
#'       models <- vector("list", n_items)
#'     } else if (length(models) < n_items) {
#'       models <- c(models, vector("list", n_items - length(models)))
#'     }
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     current_sum <- 0
#'     items_given <- character()
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # Check construct constraints
#'       if (config$questionnaire_type == "multi-construct") {
#'         if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'           items_administered[i, j] <- TRUE
#'           items_given <- c(items_given, item)
#'           if (item %in% names(data) && !is.na(data[i, item])) {
#'             current_sum <- current_sum + data[i, item]
#'           }
#'           next
#'         }
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         current_sum <- current_sum + data[i, item]
#'       }
#'       
#'       # Use model to predict probabilities
#'       # Fix:/Updated:
#'       if (j <= length(models) && !is.null(models[[j]])) {
#'         pred_probs <- predict_ordinal_probs(models[[j]], current_sum, j)
#'         
#'         # Check stopping criteria with NA handling
#'         prob_low <- pred_probs["prob_low"]
#'         prob_high <- pred_probs["prob_high"]
#'         
#'         if (!is.na(prob_low) && prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (!is.na(prob_high) && prob_high >= gamma_1 && !stop_low_only) {
#'           classifications[i] <- 1
#'           stopped_at[i] <- j
#'           break
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       models = models,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     method_info = list(
#'       description = "Stochastic curtailment using simple ordinal regression",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

# Fix: (New reduce_sc_sor function to handle convergence failures)
#' Stochastic Curtailment - Simple Ordinal Regression
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters (gamma_0, gamma_1)
#' @param cutoff Classification cutoff
#' @param training_params Parameters from training phase
#' @param stop_low_only Whether to only stop for low-risk classification
#' @param min_items_per_construct Minimum items per construct
#' @return Reduction results
reduce_sc_sor <- function(ordered_items, data, config, method_params, cutoff, 
                          training_params, stop_low_only, min_items_per_construct) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Build or use regression models
  if (!is.null(training_params)) {
    model_result <- training_params$model_result
    if (is.null(model_result)) {
      # Backward compatibility
      models <- training_params$models
      # Ensure it has correct length even from training params
      if (is.null(models) || length(models) < n_items) {
        if (is.null(models)) {
          models <- vector("list", n_items)
        } else {
          models <- c(models, vector("list", n_items - length(models)))
        }
      }
      model_result <- list(models = models, success = TRUE)
    }
  } else {
    # Build regression models for each stopping point
    model_result <- build_ordinal_models(data, ordered_items, config, cutoff, type = "simple")
  }
  
  # Check if model building was successful
  if (!is.null(model_result$success) && !model_result$success) {
    # Models failed - return an error indicator
    result <- list(
      items_administered = items_administered,
      classifications = rep(NA, n_respondents),
      stopped_at = rep(NA, n_respondents),
      n_items_used = rep(NA, n_respondents),
      method_info = list(
        description = "Stochastic curtailment using simple ordinal regression",
        error = model_result$error_message,
        model_failures = TRUE
      ),
      training_params = list(
        model_result = model_result,
        cutoff = cutoff,
        gamma_0 = gamma_0,
        gamma_1 = gamma_1
      ),
      error = model_result$error_message
    )
    return(result)
  }
  
  models <- model_result$models
  
  # Handle case where model_result$models returns NULL or wrong length
  if (is.null(models) || length(models) == 0) {
    models <- vector("list", n_items)
  } else if (length(models) < n_items) {
    models <- c(models, vector("list", n_items - length(models)))
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    current_sum <- 0
    items_given <- character()
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # Check construct constraints
      if (config$questionnaire_type == "multi-construct") {
        if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
          items_administered[i, j] <- TRUE
          items_given <- c(items_given, item)
          if (item %in% names(data) && !is.na(data[i, item])) {
            current_sum <- current_sum + data[i, item]
          }
          next
        }
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        current_sum <- current_sum + data[i, item]
      }
      
      # Use model to predict probabilities
      if (j <= length(models) && !is.null(models[[j]])) {
        pred_probs <- predict_ordinal_probs(models[[j]], current_sum, j)
        
        # Check stopping criteria with NA handling
        prob_low <- pred_probs["prob_low"]
        prob_high <- pred_probs["prob_high"]
        
        if (!is.na(prob_low) && prob_low >= gamma_0) {
          classifications[i] <- 0
          stopped_at[i] <- j
          break
        } else if (!is.na(prob_high) && prob_high >= gamma_1 && !stop_low_only) {
          classifications[i] <- 1
          stopped_at[i] <- j
          break
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      model_result = model_result,  # Store the full result
      models = models,              # Keep for backward compatibility
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    method_info = list(
      description = "Stochastic curtailment using simple ordinal regression",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Multiple Ordinal Regression
#' #'
#' #' @param ordered_items Vector of ordered item names
#' #' @param data Data frame
#' #' @param config Configuration
#' #' @param method_params Method parameters (gamma_0, gamma_1)
#' #' @param cutoff Classification cutoff
#' #' @param training_params Parameters from training phase
#' #' @param stop_low_only Whether to only stop for low-risk classification
#' #' @param min_items_per_construct Minimum items per construct
#' #' @return Reduction results
#' reduce_sc_mor <- function(ordered_items, data, config, method_params, cutoff, 
#'                           training_params, stop_low_only, min_items_per_construct) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Fix:/Updated:
#'   # Build or use regression models
#'   if (!is.null(training_params)) {
#'     models <- training_params$models
#'     # Ensure it has correct length even from training params
#'     if (is.null(models) || length(models) < n_items) {
#'       if (is.null(models)) {
#'         models <- vector("list", n_items)
#'       } else {
#'         models <- c(models, vector("list", n_items - length(models)))
#'       }
#'     }
#'   } else {
#'     # Build regression models with individual items as predictors
#'     models <- build_ordinal_models(data, ordered_items, config, cutoff, type = "multiple")
#'     
#'     # Handle case where build_ordinal_models returns NULL or wrong length
#'     if (is.null(models) || length(models) == 0) {
#'       models <- vector("list", n_items)
#'     } else if (length(models) < n_items) {
#'       models <- c(models, vector("list", n_items - length(models)))
#'     }
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     responses <- list()
#'     items_given <- character()
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # Check construct constraints
#'       if (config$questionnaire_type == "multi-construct") {
#'         if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'           items_administered[i, j] <- TRUE
#'           items_given <- c(items_given, item)
#'           if (item %in% names(data) && !is.na(data[i, item])) {
#'             responses[[item]] <- data[i, item]
#'           }
#'           next
#'         }
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         responses[[item]] <- data[i, item]
#'       }
#'       
#'       # Use model to predict probabilities
#'       # Fix:/Updated:
#'       if (j <= length(models) && !is.null(models[[j]])) {
#'         pred_probs <- predict_ordinal_probs_multiple(models[[j]], responses, items_given)
#'         
#'         # Check stopping criteria with NA handling
#'         prob_low <- pred_probs["prob_low"]
#'         prob_high <- pred_probs["prob_high"]
#'         
#'         if (!is.na(prob_low) && prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (!is.na(prob_high) && prob_high >= gamma_1 && !stop_low_only) {
#'           classifications[i] <- 1
#'           stopped_at[i] <- j
#'           break
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         total_score <- sum(unlist(responses))
#'         classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       models = models,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     method_info = list(
#'       description = "Stochastic curtailment using multiple ordinal regression",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

# Fix: (New reduce_sc_mor function to handle convergence failures)
#' Stochastic Curtailment - Multiple Ordinal Regression
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters (gamma_0, gamma_1)
#' @param cutoff Classification cutoff
#' @param training_params Parameters from training phase
#' @param stop_low_only Whether to only stop for low-risk classification
#' @param min_items_per_construct Minimum items per construct
#' @return Reduction results
reduce_sc_mor <- function(ordered_items, data, config, method_params, cutoff, 
                          training_params, stop_low_only, min_items_per_construct) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Build or use regression models
  if (!is.null(training_params)) {
    model_result <- training_params$model_result
    if (is.null(model_result)) {
      # Backward compatibility
      models <- training_params$models
      # Ensure it has correct length even from training params
      if (is.null(models) || length(models) < n_items) {
        if (is.null(models)) {
          models <- vector("list", n_items)
        } else {
          models <- c(models, vector("list", n_items - length(models)))
        }
      }
      model_result <- list(models = models, success = TRUE)
    }
  } else {
    # Build regression models with individual items as predictors
    model_result <- build_ordinal_models(data, ordered_items, config, cutoff, type = "multiple")
  }
  
  # Check if model building was successful
  if (!is.null(model_result$success) && !model_result$success) {
    # Models failed - return an error indicator
    result <- list(
      items_administered = items_administered,
      classifications = rep(NA, n_respondents),
      stopped_at = rep(NA, n_respondents),
      n_items_used = rep(NA, n_respondents),
      method_info = list(
        description = "Stochastic curtailment using multiple ordinal regression",
        error = model_result$error_message,
        model_failures = TRUE
      ),
      training_params = list(
        model_result = model_result,
        cutoff = cutoff,
        gamma_0 = gamma_0,
        gamma_1 = gamma_1
      ),
      error = model_result$error_message
    )
    return(result)
  }
  
  models <- model_result$models
  
  # Handle case where model_result$models returns NULL or wrong length
  if (is.null(models) || length(models) == 0) {
    models <- vector("list", n_items)
  } else if (length(models) < n_items) {
    models <- c(models, vector("list", n_items - length(models)))
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    responses <- list()
    items_given <- character()
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # Check construct constraints
      if (config$questionnaire_type == "multi-construct") {
        if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
          items_administered[i, j] <- TRUE
          items_given <- c(items_given, item)
          if (item %in% names(data) && !is.na(data[i, item])) {
            responses[[item]] <- data[i, item]
          }
          next
        }
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses[[item]] <- data[i, item]
      }
      
      # Use model to predict probabilities
      if (j <= length(models) && !is.null(models[[j]])) {
        pred_probs <- predict_ordinal_probs_multiple(models[[j]], responses, items_given)
        
        # Check stopping criteria with NA handling
        prob_low <- pred_probs["prob_low"]
        prob_high <- pred_probs["prob_high"]
        
        if (!is.na(prob_low) && prob_low >= gamma_0) {
          classifications[i] <- 0
          stopped_at[i] <- j
          break
        } else if (!is.na(prob_high) && prob_high >= gamma_1 && !stop_low_only) {
          classifications[i] <- 1
          stopped_at[i] <- j
          break
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        total_score <- sum(unlist(responses))
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      model_result = model_result,  # Store the full result
      models = models,              # Keep for backward compatibility
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    method_info = list(
      description = "Stochastic curtailment using multiple ordinal regression",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only
    ),
    training_params = training_params
  )
  
  return(result)
}

#' IRT-based Computerized Classification Testing
#'
#' @param ordered_items Vector of ordered item names
#' @param data Data frame
#' @param config Configuration
#' @param method_params Method parameters (irt_model, se_threshold)
#' @param cutoff Classification cutoff
#' @param training_params Parameters from training phase
#' @param stop_low_only Whether to only stop for low-risk classification
#' @param min_items_per_construct Minimum items per construct
#' @return Reduction results
reduce_irt_cct <- function(ordered_items, data, config, method_params, cutoff, 
                           training_params, stop_low_only, min_items_per_construct) {
  
  # Fix: (Removed SC-SOR fallback)
  # Check if IRT package is available
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("mirt package not available. Cannot perform IRT-based CCT.")
  }
  
  # Extract parameters
  irt_model_type <- method_params$irt_model %||% "graded" # Fix: (Changed "grm" to "graded")
  se_threshold <- method_params$se_threshold %||% 0.3
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Fit or use IRT model
  if (!is.null(training_params)) {
    irt_model <- training_params$irt_model
    theta_cutoff <- training_params$theta_cutoff
  } else {
    # Fit IRT model and determine theta cutoff
    irt_fit <- fit_irt_model(data, ordered_items, config, cutoff, irt_model_type)
    irt_model <- irt_fit$model
    theta_cutoff <- irt_fit$theta_cutoff
  }
  
  # Process each respondent using adaptive testing
  for (i in seq_len(n_respondents)) {
    responses <- numeric()
    items_given <- character()
    theta_est <- 0  # Initial theta estimate
    se_est <- Inf   # Initial standard error
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # Check construct constraints
      if (config$questionnaire_type == "multi-construct") {
        if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
          items_administered[i, j] <- TRUE
          items_given <- c(items_given, item)
          if (item %in% names(data) && !is.na(data[i, item])) {
            responses <- c(responses, data[i, item])
          }
          next
        }
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses <- c(responses, data[i, item])
      } else {
        responses <- c(responses, NA)
      }
      
      # Update theta estimate if we have enough responses
      if (length(which(!is.na(responses))) >= 3) {
        theta_update <- update_theta_estimate(irt_model, items_given, responses)
        theta_est <- theta_update$theta
        se_est <- theta_update$se
        
        # Check stopping criteria
        if (se_est < se_threshold) {
          # Confident enough to classify
          if (theta_est < theta_cutoff) {
            classifications[i] <- 0
            stopped_at[i] <- j
            break
          } else if (!stop_low_only) {
            classifications[i] <- 1
            stopped_at[i] <- j
            break
          }
          # # DEBUG: stop_low_only is preventing early stop
          # if (exists("DEBUG_CURTAILMENT") && DEBUG_CURTAILMENT && i <= 3 && stop_low_only) {
          #   cat("    stop_low_only=TRUE preventing early stop for high-risk\n")
          # }
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(theta_est >= theta_cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      irt_model = irt_model,
      theta_cutoff = theta_cutoff,
      se_threshold = se_threshold
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    method_info = list(
      description = "IRT-based computerized classification testing",
      irt_model_type = irt_model_type,
      se_threshold = se_threshold,
      theta_cutoff = theta_cutoff,
      stop_low_only = stop_low_only
    ),
    training_params = training_params
  )
  
  return(result)
}

# ============================================================================
# Integrated Reduction Methods (with complete_triggered_constructs logic)
# ============================================================================

#' Deterministic Curtailment with Integrated Triggered Construct Logic
reduce_dc_integrated <- function(ordered_items, data, config, method_params, cutoff, 
                                 training_params, stop_low_only, min_items_per_construct,
                                 complete_triggered_constructs, triggered_constructs, construct_name) {
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Get scale information from training if available, otherwise compute
  if (!is.null(training_params)) {
    item_ranges <- training_params$item_ranges
  } else {
    item_ranges <- lapply(ordered_items, function(item) {
      if (item %in% names(data)) {
        range(data[[item]], na.rm = TRUE)
      } else {
        c(0, 3)  # Default range for Likert-type items
      }
    })
    names(item_ranges) <- ordered_items
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    current_sum <- 0
    items_given <- character()
    construct_triggered <- construct_name %in% triggered_constructs[[i]]
    
    # Check items sequentially
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # If construct is triggered, must administer all items
      if (construct_triggered) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          current_sum <- current_sum + data[i, item]
        }
        # Continue to next item without checking stopping criteria
        next
      }
      
      # Check construct constraints
      if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
        # Must administer this item due to constraints
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          current_sum <- current_sum + data[i, item]
        }
        next
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        current_sum <- current_sum + data[i, item]
      }
      
      # Calculate bounds on possible total score
      if (j < n_items) {
        remaining_items <- ordered_items[(j+1):n_items]
        min_possible <- current_sum + sum(sapply(remaining_items, function(x) item_ranges[[x]][1]))
        max_possible <- current_sum + sum(sapply(remaining_items, function(x) item_ranges[[x]][2]))
      } else {
        min_possible <- current_sum
        max_possible <- current_sum
      }
      
      # Check if classification is determined
      if (max_possible < cutoff) {
        # Will definitely be low-risk
        classifications[i] <- 0
        stopped_at[i] <- j
        break
      } else if (min_possible >= cutoff) {
        # Will definitely be high-risk
        if (complete_triggered_constructs) {
          # Mark this construct as triggered
          triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
          construct_triggered <- TRUE
          # Continue administering all remaining items for this construct
        } else if (!stop_low_only) {
          classifications[i] <- 1
          stopped_at[i] <- j
          break
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
    
    # If we finished all items (either naturally or due to triggering)
    if (stopped_at[i] == 0) {
      classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
      stopped_at[i] <- sum(items_administered[i, ])
    }
  }
  
  # Store training parameters if this is training phase
  if (is.null(training_params)) {
    training_params <- list(
      item_ranges = item_ranges,
      cutoff = cutoff
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    triggered_constructs = triggered_constructs,
    method_info = list(
      description = "Deterministic curtailment with integrated triggered construct logic",
      stop_low_only = stop_low_only,
      complete_triggered_constructs = complete_triggered_constructs
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Empirical Proportions with Integrated Triggered Construct Logic
#' reduce_sc_ep_integrated <- function(ordered_items, data, config, method_params, cutoff, 
#'                                     training_params, stop_low_only, min_items_per_construct,
#'                                     complete_triggered_constructs, triggered_constructs, construct_name) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Build or use lookup tables
#'   if (!is.null(training_params)) {
#'     lookup_tables <- training_params$lookup_tables
#'   } else {
#'     # Build empirical probability tables from training data
#'     lookup_tables <- build_empirical_tables(data, ordered_items, config, cutoff)
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     responses <- numeric()
#'     items_given <- character()
#'     construct_triggered <- construct_name %in% triggered_constructs[[i]]
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # If construct is triggered, must administer all items
#'       if (construct_triggered) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           responses <- c(responses, data[i, item])
#'         }
#'         # Continue to next item without checking stopping criteria
#'         next
#'       }
#'       
#'       # Check construct constraints
#'       if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           responses <- c(responses, data[i, item])
#'         }
#'         next
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         responses <- c(responses, data[i, item])
#'       }
#'       
#'       # Look up conditional probabilities
#'       response_pattern <- paste(responses, collapse = "_")
#'       
#'       if (response_pattern %in% names(lookup_tables[[j]])) {
#'         probs <- lookup_tables[[j]][[response_pattern]]
#'         prob_low <- probs["prob_low"]
#'         prob_high <- probs["prob_high"]
#'         
#'         # Check stopping criteria
#'         if (prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (prob_high >= gamma_1) {
#'           # # DEBUG
#'           # if (exists("DEBUG_CURTAILMENT") && DEBUG_CURTAILMENT && i <= 3) {
#'           #   cat("  Respondent", i, "item", j, ": prob_high=", prob_high, 
#'           #       ">=", gamma_1, "construct=", construct_name, "\n")
#'           #   cat("    complete_triggered_constructs=", complete_triggered_constructs,
#'           #       "stop_low_only=", stop_low_only, "\n")
#'           # }
#'           
#'           if (complete_triggered_constructs) {
#'             # Mark this construct as triggered
#'             triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
#'             construct_triggered <- TRUE
#'             # Continue administering all remaining items for this construct
#'           } else if (!stop_low_only) {
#'             classifications[i] <- 1
#'             stopped_at[i] <- j
#'             break
#'           }
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         total_score <- sum(responses)
#'         classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'     
#'     # If we finished all items (either naturally or due to triggering)
#'     if (stopped_at[i] == 0) {
#'       total_score <- sum(responses)
#'       classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'       stopped_at[i] <- sum(items_administered[i, ])
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       lookup_tables = lookup_tables,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     triggered_constructs = triggered_constructs,
#'     method_info = list(
#'       description = "Stochastic curtailment (EP) with integrated triggered construct logic",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only,
#'       complete_triggered_constructs = complete_triggered_constructs
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

#' Stochastic Curtailment - Empirical Proportions with Integrated Triggered Construct Logic (CORRECTED)
reduce_sc_ep_integrated <- function(ordered_items, data, config, method_params, cutoff, 
                                    training_params, stop_low_only, min_items_per_construct,
                                    complete_triggered_constructs, triggered_constructs, construct_name) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)  # Initialized to zeros
  classifications <- numeric(n_respondents)
  
  # Build or use lookup tables
  if (!is.null(training_params)) {
    lookup_tables <- training_params$lookup_tables
  } else {
    # Build empirical probability tables from training data
    lookup_tables <- build_empirical_tables(data, ordered_items, config, cutoff)
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    responses <- numeric()
    items_given <- character()
    items_count <- 0  # EXPLICIT COUNTER for items administered
    construct_triggered <- construct_name %in% triggered_constructs[[i]]
    early_stopped <- FALSE  # Track if we stopped early
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # If construct is triggered, must administer all items
      if (construct_triggered) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        items_count <- items_count + 1  # INCREMENT COUNTER
        
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses <- c(responses, data[i, item])
        }
        # Continue to next item without checking stopping criteria
        next
      }
      
      # Check construct constraints
      if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        items_count <- items_count + 1  # INCREMENT COUNTER
        
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses <- c(responses, data[i, item])
        }
        next
      }
      
      # Administer item j
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      items_count <- items_count + 1  # INCREMENT COUNTER
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses <- c(responses, data[i, item])
      }
      
      # After administering item j, check if we should stop
      
      # Look up conditional probabilities for pattern of length j
      response_pattern <- paste(responses, collapse = "_")
      
      if (response_pattern %in% names(lookup_tables[[j]])) {
        probs <- lookup_tables[[j]][[response_pattern]]
        prob_low <- probs["prob_low"]
        prob_high <- probs["prob_high"]
        
        # Check stopping criteria
        if (prob_low >= gamma_0) {
          # Low-risk classification - stop here
          classifications[i] <- 0
          stopped_at[i] <- items_count  # Use explicit counter
          early_stopped <- TRUE
          break
        } else if (prob_high >= gamma_1) {
          if (complete_triggered_constructs) {
            # Mark this construct as triggered
            triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
            construct_triggered <- TRUE
            # Continue administering all remaining items for this construct
          } else if (!stop_low_only) {
            # High-risk classification - stop here
            classifications[i] <- 1
            stopped_at[i] <- items_count  # Use explicit counter
            early_stopped <- TRUE
            break
          }
        }
      }
      
      # If we've reached the last item
      if (j == n_items && !early_stopped) {
        total_score <- sum(responses)
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
        stopped_at[i] <- items_count  # Use explicit counter
      }
    }
    
    # Handle case where all items were administered (e.g., due to triggering)
    # but we never explicitly set stopped_at
    if (stopped_at[i] == 0) {
      stopped_at[i] <- items_count  # Use the actual count
      if (length(responses) > 0) {
        total_score <- sum(responses)
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
      }
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      lookup_tables = lookup_tables,
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  # VERIFICATION: Check that stopped_at matches n_items_used
  n_items_used <- rowSums(items_administered)
  if (!all(stopped_at == n_items_used)) {
    warning("Mismatch between stopped_at and n_items_used detected in integrated version!")
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = n_items_used,
    triggered_constructs = triggered_constructs,
    method_info = list(
      description = "Stochastic curtailment (EP) with integrated triggered construct logic",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only,
      complete_triggered_constructs = complete_triggered_constructs
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Simple Ordinal Regression with Integrated Triggered Construct Logic
#' reduce_sc_sor_integrated <- function(ordered_items, data, config, method_params, cutoff, 
#'                                      training_params, stop_low_only, min_items_per_construct,
#'                                      complete_triggered_constructs, triggered_constructs, construct_name) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Fix:/Updated:
#'   # Build or use regression models
#'   if (!is.null(training_params)) {
#'     models <- training_params$models
#'     # Ensure it has correct length even from training params
#'     if (is.null(models) || length(models) < n_items) {
#'       if (is.null(models)) {
#'         models <- vector("list", n_items)
#'       } else {
#'         models <- c(models, vector("list", n_items - length(models)))
#'       }
#'     }
#'   } else {
#'     # Build regression models for each stopping point
#'     models <- build_ordinal_models(data, ordered_items, config, cutoff, type = "simple")
#'     
#'     # Handle case where build_ordinal_models returns NULL or wrong length
#'     if (is.null(models) || length(models) == 0) {
#'       models <- vector("list", n_items)
#'     } else if (length(models) < n_items) {
#'       models <- c(models, vector("list", n_items - length(models)))
#'     }
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     current_sum <- 0
#'     items_given <- character()
#'     construct_triggered <- construct_name %in% triggered_constructs[[i]]
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # If construct is triggered, must administer all items
#'       if (construct_triggered) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           current_sum <- current_sum + data[i, item]
#'         }
#'         # Continue to next item without checking stopping criteria
#'         next
#'       }
#'       
#'       # Check construct constraints
#'       if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           current_sum <- current_sum + data[i, item]
#'         }
#'         next
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         current_sum <- current_sum + data[i, item]
#'       }
#'       
#'       # Use model to predict probabilities
#'       # Fix:/Updated:
#'       if (j <= length(models) && !is.null(models[[j]])) {
#'         pred_probs <- predict_ordinal_probs(models[[j]], current_sum, j)
#'         
#'         # Check stopping criteria with NA handling
#'         prob_low <- pred_probs["prob_low"]
#'         prob_high <- pred_probs["prob_high"]
#'         
#'         if (!is.na(prob_low) && prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (!is.na(prob_high) && prob_high >= gamma_1) {
#'           if (complete_triggered_constructs) {
#'             # Mark this construct as triggered
#'             triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
#'             construct_triggered <- TRUE
#'             # Continue administering all remaining items for this construct
#'           } else if (!stop_low_only) {
#'             classifications[i] <- 1
#'             stopped_at[i] <- j
#'             break
#'           }
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'     
#'     # If we finished all items (either naturally or due to triggering)
#'     if (stopped_at[i] == 0) {
#'       classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
#'       stopped_at[i] <- sum(items_administered[i, ])
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       models = models,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     triggered_constructs = triggered_constructs,
#'     method_info = list(
#'       description = "Stochastic curtailment (SOR) with integrated triggered construct logic",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only,
#'       complete_triggered_constructs = complete_triggered_constructs
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

# Fix: (New reduce_sc_sor_integrated function to handle convergence failures)
#' Stochastic Curtailment - Simple Ordinal Regression with Integrated Triggered Construct Logic
reduce_sc_sor_integrated <- function(ordered_items, data, config, method_params, cutoff, 
                                     training_params, stop_low_only, min_items_per_construct,
                                     complete_triggered_constructs, triggered_constructs, construct_name) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Build or use regression models
  if (!is.null(training_params)) {
    model_result <- training_params$model_result
    if (is.null(model_result)) {
      # Backward compatibility
      models <- training_params$models
      # Ensure it has correct length even from training params
      if (is.null(models) || length(models) < n_items) {
        if (is.null(models)) {
          models <- vector("list", n_items)
        } else {
          models <- c(models, vector("list", n_items - length(models)))
        }
      }
      model_result <- list(models = models, success = TRUE)
    }
  } else {
    # Build regression models for each stopping point
    model_result <- build_ordinal_models(data, ordered_items, config, cutoff, type = "simple")
  }
  
  # Check if model building was successful
  if (!is.null(model_result$success) && !model_result$success) {
    # Models failed - return an error indicator
    result <- list(
      items_administered = items_administered,
      classifications = rep(NA, n_respondents),
      stopped_at = rep(NA, n_respondents),
      n_items_used = rep(NA, n_respondents),
      triggered_constructs = triggered_constructs,
      method_info = list(
        description = "Stochastic curtailment (SOR) with integrated triggered construct logic",
        error = model_result$error_message,
        model_failures = TRUE,
        stop_low_only = stop_low_only,
        complete_triggered_constructs = complete_triggered_constructs
      ),
      training_params = list(
        model_result = model_result,
        cutoff = cutoff,
        gamma_0 = gamma_0,
        gamma_1 = gamma_1
      ),
      error = model_result$error_message
    )
    return(result)
  }
  
  models <- model_result$models
  
  # Handle case where model_result$models returns NULL or wrong length
  if (is.null(models) || length(models) == 0) {
    models <- vector("list", n_items)
  } else if (length(models) < n_items) {
    models <- c(models, vector("list", n_items - length(models)))
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    current_sum <- 0
    items_given <- character()
    construct_triggered <- construct_name %in% triggered_constructs[[i]]
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # If construct is triggered, must administer all items
      if (construct_triggered) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          current_sum <- current_sum + data[i, item]
        }
        # Continue to next item without checking stopping criteria
        next
      }
      
      # Check construct constraints
      if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          current_sum <- current_sum + data[i, item]
        }
        next
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        current_sum <- current_sum + data[i, item]
      }
      
      # Use model to predict probabilities
      if (j <= length(models) && !is.null(models[[j]])) {
        pred_probs <- predict_ordinal_probs(models[[j]], current_sum, j)
        
        # Check stopping criteria with NA handling
        prob_low <- pred_probs["prob_low"]
        prob_high <- pred_probs["prob_high"]
        
        if (!is.na(prob_low) && prob_low >= gamma_0) {
          classifications[i] <- 0
          stopped_at[i] <- j
          break
        } else if (!is.na(prob_high) && prob_high >= gamma_1) {
          if (complete_triggered_constructs) {
            # Mark this construct as triggered
            triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
            construct_triggered <- TRUE
            # Continue administering all remaining items for this construct
          } else if (!stop_low_only) {
            classifications[i] <- 1
            stopped_at[i] <- j
            break
          }
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
    
    # If we finished all items (either naturally or due to triggering)
    if (stopped_at[i] == 0) {
      classifications[i] <- ifelse(current_sum >= cutoff, 1, 0)
      stopped_at[i] <- sum(items_administered[i, ])
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      model_result = model_result,  # Store the full result
      models = models,              # Keep for backward compatibility
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    triggered_constructs = triggered_constructs,
    method_info = list(
      description = "Stochastic curtailment (SOR) with integrated triggered construct logic",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only,
      complete_triggered_constructs = complete_triggered_constructs
    ),
    training_params = training_params
  )
  
  return(result)
}

#' #' Stochastic Curtailment - Multiple Ordinal Regression with Integrated Triggered Construct Logic
#' reduce_sc_mor_integrated <- function(ordered_items, data, config, method_params, cutoff, 
#'                                      training_params, stop_low_only, min_items_per_construct,
#'                                      complete_triggered_constructs, triggered_constructs, construct_name) {
#'   
#'   # Extract gamma parameters
#'   gamma_0 <- method_params$gamma_0 %||% 0.95
#'   gamma_1 <- method_params$gamma_1 %||% 0.95
#'   
#'   n_respondents <- nrow(data)
#'   n_items <- length(ordered_items)
#'   
#'   # Initialize results
#'   items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
#'   colnames(items_administered) <- ordered_items
#'   stopped_at <- numeric(n_respondents)
#'   classifications <- numeric(n_respondents)
#'   
#'   # Fix:/Updated:
#'   # Build or use regression models
#'   if (!is.null(training_params)) {
#'     models <- training_params$models
#'     # Ensure it has correct length even from training params
#'     if (is.null(models) || length(models) < n_items) {
#'       if (is.null(models)) {
#'         models <- vector("list", n_items)
#'       } else {
#'         models <- c(models, vector("list", n_items - length(models)))
#'       }
#'     }
#'   } else {
#'     # Build regression models with individual items as predictors
#'     models <- build_ordinal_models(data, ordered_items, config, cutoff, type = "multiple")
#'     
#'     # Handle case where build_ordinal_models returns NULL or wrong length
#'     if (is.null(models) || length(models) == 0) {
#'       models <- vector("list", n_items)
#'     } else if (length(models) < n_items) {
#'       models <- c(models, vector("list", n_items - length(models)))
#'     }
#'   }
#'   
#'   # Process each respondent
#'   for (i in seq_len(n_respondents)) {
#'     responses <- list()
#'     items_given <- character()
#'     construct_triggered <- construct_name %in% triggered_constructs[[i]]
#'     
#'     for (j in seq_along(ordered_items)) {
#'       item <- ordered_items[j]
#'       
#'       # If construct is triggered, must administer all items
#'       if (construct_triggered) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           responses[[item]] <- data[i, item]
#'         }
#'         # Continue to next item without checking stopping criteria
#'         next
#'       }
#'       
#'       # Check construct constraints
#'       if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
#'         items_administered[i, j] <- TRUE
#'         items_given <- c(items_given, item)
#'         if (item %in% names(data) && !is.na(data[i, item])) {
#'           responses[[item]] <- data[i, item]
#'         }
#'         next
#'       }
#'       
#'       # Administer item
#'       items_administered[i, j] <- TRUE
#'       items_given <- c(items_given, item)
#'       
#'       if (item %in% names(data) && !is.na(data[i, item])) {
#'         responses[[item]] <- data[i, item]
#'       }
#'       
#'       # Use model to predict probabilities
#'       # Fix:/Updated:
#'       if (j <= length(models) && !is.null(models[[j]])) {
#'         pred_probs <- predict_ordinal_probs_multiple(models[[j]], responses, items_given)
#'         
#'         # Check stopping criteria with NA handling
#'         prob_low <- pred_probs["prob_low"]
#'         prob_high <- pred_probs["prob_high"]
#'         
#'         if (!is.na(prob_low) && prob_low >= gamma_0) {
#'           classifications[i] <- 0
#'           stopped_at[i] <- j
#'           break
#'         } else if (!is.na(prob_high) && prob_high >= gamma_1) {
#'           if (complete_triggered_constructs) {
#'             # Mark this construct as triggered
#'             triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
#'             construct_triggered <- TRUE
#'             # Continue administering all remaining items for this construct
#'           } else if (!stop_low_only) {
#'             classifications[i] <- 1
#'             stopped_at[i] <- j
#'             break
#'           }
#'         }
#'       }
#'       
#'       # If we've reached the end
#'       if (j == n_items) {
#'         total_score <- sum(unlist(responses))
#'         classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'         stopped_at[i] <- j
#'       }
#'     }
#'     
#'     # If we finished all items (either naturally or due to triggering)
#'     if (stopped_at[i] == 0) {
#'       total_score <- sum(unlist(responses))
#'       classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
#'       stopped_at[i] <- sum(items_administered[i, ])
#'     }
#'   }
#'   
#'   # Store training parameters
#'   if (is.null(training_params)) {
#'     training_params <- list(
#'       models = models,
#'       cutoff = cutoff,
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1
#'     )
#'   }
#'   
#'   result <- list(
#'     items_administered = items_administered,
#'     classifications = classifications,
#'     stopped_at = stopped_at,
#'     n_items_used = rowSums(items_administered),
#'     triggered_constructs = triggered_constructs,
#'     method_info = list(
#'       description = "Stochastic curtailment (MOR) with integrated triggered construct logic",
#'       gamma_0 = gamma_0,
#'       gamma_1 = gamma_1,
#'       stop_low_only = stop_low_only,
#'       complete_triggered_constructs = complete_triggered_constructs
#'     ),
#'     training_params = training_params
#'   )
#'   
#'   return(result)
#' }

# Fix: (New reduce_sc_mor_integrated function to handle convergence failures)
#' Stochastic Curtailment - Multiple Ordinal Regression with Integrated Triggered Construct Logic
reduce_sc_mor_integrated <- function(ordered_items, data, config, method_params, cutoff, 
                                     training_params, stop_low_only, min_items_per_construct,
                                     complete_triggered_constructs, triggered_constructs, construct_name) {
  
  # Extract gamma parameters
  gamma_0 <- method_params$gamma_0 %||% 0.95
  gamma_1 <- method_params$gamma_1 %||% 0.95
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Build or use regression models
  if (!is.null(training_params)) {
    model_result <- training_params$model_result
    if (is.null(model_result)) {
      # Backward compatibility
      models <- training_params$models
      # Ensure it has correct length even from training params
      if (is.null(models) || length(models) < n_items) {
        if (is.null(models)) {
          models <- vector("list", n_items)
        } else {
          models <- c(models, vector("list", n_items - length(models)))
        }
      }
      model_result <- list(models = models, success = TRUE)
    }
  } else {
    # Build regression models with individual items as predictors
    model_result <- build_ordinal_models(data, ordered_items, config, cutoff, type = "multiple")
  }
  
  # Check if model building was successful
  if (!is.null(model_result$success) && !model_result$success) {
    # Models failed - return an error indicator
    result <- list(
      items_administered = items_administered,
      classifications = rep(NA, n_respondents),
      stopped_at = rep(NA, n_respondents),
      n_items_used = rep(NA, n_respondents),
      triggered_constructs = triggered_constructs,
      method_info = list(
        description = "Stochastic curtailment (MOR) with integrated triggered construct logic",
        error = model_result$error_message,
        model_failures = TRUE,
        stop_low_only = stop_low_only,
        complete_triggered_constructs = complete_triggered_constructs
      ),
      training_params = list(
        model_result = model_result,
        cutoff = cutoff,
        gamma_0 = gamma_0,
        gamma_1 = gamma_1
      ),
      error = model_result$error_message
    )
    return(result)
  }
  
  models <- model_result$models
  
  # Handle case where model_result$models returns NULL or wrong length
  if (is.null(models) || length(models) == 0) {
    models <- vector("list", n_items)
  } else if (length(models) < n_items) {
    models <- c(models, vector("list", n_items - length(models)))
  }
  
  # Process each respondent
  for (i in seq_len(n_respondents)) {
    responses <- list()
    items_given <- character()
    construct_triggered <- construct_name %in% triggered_constructs[[i]]
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # If construct is triggered, must administer all items
      if (construct_triggered) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses[[item]] <- data[i, item]
        }
        # Continue to next item without checking stopping criteria
        next
      }
      
      # Check construct constraints
      if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses[[item]] <- data[i, item]
        }
        next
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses[[item]] <- data[i, item]
      }
      
      # Use model to predict probabilities
      if (j <= length(models) && !is.null(models[[j]])) {
        pred_probs <- predict_ordinal_probs_multiple(models[[j]], responses, items_given)
        
        # Check stopping criteria with NA handling
        prob_low <- pred_probs["prob_low"]
        prob_high <- pred_probs["prob_high"]
        
        if (!is.na(prob_low) && prob_low >= gamma_0) {
          classifications[i] <- 0
          stopped_at[i] <- j
          break
        } else if (!is.na(prob_high) && prob_high >= gamma_1) {
          if (complete_triggered_constructs) {
            # Mark this construct as triggered
            triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
            construct_triggered <- TRUE
            # Continue administering all remaining items for this construct
          } else if (!stop_low_only) {
            classifications[i] <- 1
            stopped_at[i] <- j
            break
          }
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        total_score <- sum(unlist(responses))
        classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
    
    # If we finished all items (either naturally or due to triggering)
    if (stopped_at[i] == 0) {
      total_score <- sum(unlist(responses))
      classifications[i] <- ifelse(total_score >= cutoff, 1, 0)
      stopped_at[i] <- sum(items_administered[i, ])
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      model_result = model_result,  # Store the full result
      models = models,              # Keep for backward compatibility
      cutoff = cutoff,
      gamma_0 = gamma_0,
      gamma_1 = gamma_1
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    triggered_constructs = triggered_constructs,
    method_info = list(
      description = "Stochastic curtailment (MOR) with integrated triggered construct logic",
      gamma_0 = gamma_0,
      gamma_1 = gamma_1,
      stop_low_only = stop_low_only,
      complete_triggered_constructs = complete_triggered_constructs
    ),
    training_params = training_params
  )
  
  return(result)
}

#' IRT-based CCT with Integrated Triggered Construct Logic
reduce_irt_cct_integrated <- function(ordered_items, data, config, method_params, cutoff, 
                                      training_params, stop_low_only, min_items_per_construct,
                                      complete_triggered_constructs, triggered_constructs, construct_name) {
  
  # Fix: (Removed SC-SOR fallback)
  # Check if IRT package is available
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("mirt package not available. Cannot perform IRT-based CCT.")
  }
  
  # Extract parameters
  irt_model_type <- method_params$irt_model %||% "graded" # Fix: (Changed "grm" to "graded")
  se_threshold <- method_params$se_threshold %||% 0.3
  
  n_respondents <- nrow(data)
  n_items <- length(ordered_items)
  
  # Initialize results
  items_administered <- matrix(FALSE, nrow = n_respondents, ncol = n_items)
  colnames(items_administered) <- ordered_items
  stopped_at <- numeric(n_respondents)
  classifications <- numeric(n_respondents)
  
  # Fit or use IRT model
  if (!is.null(training_params)) {
    irt_model <- training_params$irt_model
    theta_cutoff <- training_params$theta_cutoff
  } else {
    # Fit IRT model and determine theta cutoff
    irt_fit <- fit_irt_model(data, ordered_items, config, cutoff, irt_model_type)
    irt_model <- irt_fit$model
    theta_cutoff <- irt_fit$theta_cutoff
  }
  
  # Process each respondent using adaptive testing
  for (i in seq_len(n_respondents)) {
    responses <- numeric()
    items_given <- character()
    theta_est <- 0  # Initial theta estimate
    se_est <- Inf   # Initial standard error
    construct_triggered <- construct_name %in% triggered_constructs[[i]]
    
    for (j in seq_along(ordered_items)) {
      item <- ordered_items[j]
      
      # If construct is triggered, must administer all items
      if (construct_triggered) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses <- c(responses, data[i, item])
        } else {
          responses <- c(responses, NA)
        }
        # Continue to next item without checking stopping criteria
        next
      }
      
      # Check construct constraints
      if (!check_construct_constraints(items_given, item, config, min_items_per_construct)) {
        items_administered[i, j] <- TRUE
        items_given <- c(items_given, item)
        if (item %in% names(data) && !is.na(data[i, item])) {
          responses <- c(responses, data[i, item])
        }
        next
      }
      
      # Administer item
      items_administered[i, j] <- TRUE
      items_given <- c(items_given, item)
      
      if (item %in% names(data) && !is.na(data[i, item])) {
        responses <- c(responses, data[i, item])
      } else {
        responses <- c(responses, NA)
      }
      
      # Update theta estimate if we have enough responses
      if (length(which(!is.na(responses))) >= 3) {
        theta_update <- update_theta_estimate(irt_model, items_given, responses)
        theta_est <- theta_update$theta
        se_est <- theta_update$se
        
        # Check stopping criteria
        if (se_est < se_threshold) {
          # Confident enough to classify
          if (theta_est < theta_cutoff) {
            classifications[i] <- 0
            stopped_at[i] <- j
            break
          } else {
            if (complete_triggered_constructs) {
              # Mark this construct as triggered
              triggered_constructs[[i]] <- unique(c(triggered_constructs[[i]], construct_name))
              construct_triggered <- TRUE
              # Continue administering all remaining items for this construct
            } else if (!stop_low_only) {
              classifications[i] <- 1
              stopped_at[i] <- j
              break
            }
          }
        }
      }
      
      # If we've reached the end
      if (j == n_items) {
        classifications[i] <- ifelse(theta_est >= theta_cutoff, 1, 0)
        stopped_at[i] <- j
      }
    }
    
    # If we finished all items (either naturally or due to triggering)
    if (stopped_at[i] == 0) {
      classifications[i] <- ifelse(theta_est >= theta_cutoff, 1, 0)
      stopped_at[i] <- sum(items_administered[i, ])
    }
  }
  
  # Store training parameters
  if (is.null(training_params)) {
    training_params <- list(
      irt_model = irt_model,
      theta_cutoff = theta_cutoff,
      se_threshold = se_threshold
    )
  }
  
  result <- list(
    items_administered = items_administered,
    classifications = classifications,
    stopped_at = stopped_at,
    n_items_used = rowSums(items_administered),
    triggered_constructs = triggered_constructs,
    method_info = list(
      description = "IRT-based CCT with integrated triggered construct logic",
      irt_model_type = irt_model_type,
      se_threshold = se_threshold,
      theta_cutoff = theta_cutoff,
      stop_low_only = stop_low_only,
      complete_triggered_constructs = complete_triggered_constructs
    ),
    training_params = training_params
  )
  
  return(result)
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Calculate Classifications Based on Total Score
#'
#' @param data Data frame
#' @param items Item names to include
#' @param config Configuration
#' @param cutoff Classification cutoff
#' @return Vector of classifications
calculate_classifications <- function(data, items, config, cutoff) {
  valid_items <- intersect(items, names(data))
  if (length(valid_items) == 0) {
    return(rep(NA, nrow(data)))
  }
  
  total_scores <- rowSums(data[, valid_items], na.rm = TRUE)
  # Note: >= cutoff means clinically significant (high-risk)
  classifications <- ifelse(total_scores >= cutoff, 1, 0)
  
  return(classifications)
}

#' Check Construct Constraints
#'
#' @param items_given Items already administered
#' @param next_item Next item to consider
#' @param config Configuration
#' @param min_items_per_construct Minimum items per construct
#' @return TRUE if constraints allow stopping, FALSE if must continue
check_construct_constraints <- function(items_given, next_item, config, min_items_per_construct) {
  # For unidimensional, no construct constraints
  if (config$questionnaire_type == "unidimensional") {
    return(TRUE)
  }
  
  # Find which construct the next item belongs to
  item_construct <- NULL
  for (construct in names(config$constructs)) {
    if (next_item %in% config$constructs[[construct]]) {
      item_construct <- construct
      break
    }
  }
  
  if (is.null(item_construct)) {
    return(TRUE)  # Item not found in any construct
  }
  
  # Count items already given from this construct
  construct_items <- config$constructs[[item_construct]]
  items_from_construct <- intersect(items_given, construct_items)
  
  # Must continue if we haven't met minimum
  if (length(items_from_construct) < min_items_per_construct) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Build Empirical Probability Tables
#'
#' @param data Training data
#' @param ordered_items Ordered item names
#' @param config Configuration
#' @param cutoff Classification cutoff
#' @return List of lookup tables
build_empirical_tables <- function(data, ordered_items, config, cutoff) {
  n_items <- length(ordered_items)
  lookup_tables <- vector("list", n_items)
  
  # For multi-construct, we work within a single construct
  # The ordered_items should already be construct-specific when called
  # from reduce_multi_construct
  
  # Calculate full classifications based on all items
  full_classifications <- calculate_classifications(data, ordered_items, config, cutoff)
  
  for (j in seq_len(n_items)) {
    # Get items up to position j
    items_so_far <- ordered_items[1:j]
    
    # Get unique response patterns
    response_data <- data[, items_so_far, drop = FALSE]
    patterns <- apply(response_data, 1, function(x) paste(x, collapse = "_"))
    
    # Calculate conditional probabilities
    pattern_probs <- list()
    for (pattern in unique(patterns)) {
      pattern_idx <- which(patterns == pattern)
      outcomes <- full_classifications[pattern_idx]
      
      n_low <- sum(outcomes == 0, na.rm = TRUE)
      n_high <- sum(outcomes == 1, na.rm = TRUE)
      n_total <- n_low + n_high
      
      # Fix:/Update: (Replaced the existing probability calculation with the following calculation in order to handle base rates)
      if (n_total > 0) {
        prob_low_sample <- n_low / n_total
        prob_high_sample <- n_high / n_total
        
        # Get base rates
        if (config$questionnaire_type == "multi-construct") {
          # For multi-construct, ordered_items should be construct-specific
          # Find which construct we're working with
          construct_name <- NULL
          for (c_name in names(config$constructs)) {
            if (all(ordered_items %in% config$constructs[[c_name]])) {
              construct_name <- c_name
              break
            }
          }
          sample_rate <- config$sample_base_rates[[construct_name]]
          ref_rate <- config$reference_base_rates[[construct_name]]
        } else {
          sample_rate <- config$sample_base_rates$total
          ref_rate <- config$reference_base_rates$total
        }
        
        # Adjust probabilities if reference rate differs from sample rate
        if (!is.null(ref_rate) && !is.null(sample_rate) && 
            abs(ref_rate - sample_rate) > 0.01) {
          prob_low_adj <- adjust_probability_for_base_rate(prob_low_sample, 
                                                           sample_rate, ref_rate)
          prob_high_adj <- adjust_probability_for_base_rate(prob_high_sample, 
                                                            sample_rate, ref_rate)
          # Normalize to ensure they sum to 1
          total_prob <- prob_low_adj + prob_high_adj
          pattern_probs[[pattern]] <- c(
            prob_low = prob_low_adj / total_prob,
            prob_high = prob_high_adj / total_prob,
            prob_low_sample = prob_low_sample,
            prob_high_sample = prob_high_sample
          )
        } else {
          pattern_probs[[pattern]] <- c(
            prob_low = prob_low_sample,
            prob_high = prob_high_sample,
            prob_low_sample = prob_low_sample,
            prob_high_sample = prob_high_sample
          )
        }
      }
      
    }
    
    lookup_tables[[j]] <- pattern_probs
  }
  
  return(lookup_tables)
}

#' #' Build Ordinal Regression Models
#' #'
#' #' @param data Training data
#' #' @param ordered_items Ordered item names
#' #' @param config Configuration
#' #' @param cutoff Classification cutoff
#' #' @param type "simple" for sum score or "multiple" for individual items
#' #' @return List of models
#' build_ordinal_models <- function(data, ordered_items, config, cutoff, type = "simple") {
#'   n_items <- length(ordered_items)
#'   models <- vector("list", n_items)
#'   
#'   # Calculate full classifications based on all items and cutoff
#'   full_classifications <- calculate_classifications(data, ordered_items, config, cutoff)
#'   
#'   # Fix: (Replaced the warning and return(models) with a stopping error to prevent continuation with NULL models, which would produce meaningless results)
#'   if (length(unique(full_classifications[!is.na(full_classifications)])) < 2) {
#'     stop("Not enough variability in outcomes to fit regression models. All samples have the same classification.")
#'   }
#'   
#'   n_failed_models <- 0 # Fix (Added a counter for model convergence failures)
#'   for (j in seq_len(n_items)) {
#'     # Skip if too few items (adjust threshold for small constructs)
#'     min_items_for_model <- min(3, ceiling(n_items * 0.5))
#'     if (j < min_items_for_model) next
#'     
#'     # Get items up to position j
#'     items_so_far <- ordered_items[1:j]
#'     
#'     # Prepare data for modeling
#'     if (type == "simple") {
#'       # Use sum score as predictor
#'       predictor_data <- rowSums(data[, items_so_far, drop = FALSE], na.rm = TRUE)
#'       model_data <- data.frame(
#'         outcome = factor(full_classifications),
#'         sum_score = predictor_data
#'       )
#'       
#'       # Fit logistic regression
#'       tryCatch({
#'         # Check if there's enough variability in predictors
#'         if (length(unique(predictor_data)) < 2) {
#'           models[[j]] <- NULL
#'         } else {
#'           # Suppress warnings about perfect separation
#'           suppressWarnings({
#'             models[[j]] <- glm(outcome ~ sum_score, data = model_data, family = binomial())
#'           })
#'           # Check if model converged properly
#'           if (!models[[j]]$converged || any(is.na(coef(models[[j]])))) {
#'             models[[j]] <- NULL
#'             n_failed_models <- n_failed_models + 1 # Fix (Added a counter for model convergence failures)
#'           }
#'         }
#'       }, error = function(e) {
#'         models[[j]] <- NULL
#'       })
#'       
#'     } else {
#'       # Use individual items as predictors
#'       model_data <- data.frame(
#'         outcome = factor(full_classifications),
#'         data[, items_so_far, drop = FALSE]
#'       )
#'       
#'       # Fit logistic regression with all items
#'       formula_str <- paste("outcome ~", paste(items_so_far, collapse = " + "))
#'       tryCatch({
#'         # Check if we have enough observations relative to predictors
#'         n_obs <- sum(!is.na(model_data$outcome))
#'         n_predictors <- length(items_so_far)
#'         
#'         if (n_obs < n_predictors * 5) {  # Rule of thumb: 5-10 obs per predictor
#'           models[[j]] <- NULL
#'         } else {
#'           # Suppress warnings about perfect separation
#'           suppressWarnings({
#'             models[[j]] <- glm(as.formula(formula_str), data = model_data, family = binomial())
#'           })
#'           # Check if model converged properly
#'           if (!models[[j]]$converged || any(is.na(coef(models[[j]])))) {
#'             models[[j]] <- NULL
#'             n_failed_models <- n_failed_models + 1 # Fix (Added a counter for model convergence failures)
#'           }
#'         }
#'       }, error = function(e) {
#'         models[[j]] <- NULL
#'       })
#'     }
#'     
#'     # # Fix:/Update: (Inserted code to add base rate information to the model)
#'     # # Store base rate information with the model
#'     # if (!is.null(models[[j]])) {
#'     #   # Get appropriate base rates
#'     #   if (config$questionnaire_type == "multi-construct") {
#'     #     # Find construct name (similar logic as above)
#'     #     construct_name <- NULL
#'     #     for (c_name in names(config$constructs)) {
#'     #       if (all(ordered_items %in% config$constructs[[c_name]])) {
#'     #         construct_name <- c_name
#'     #         break
#'     #       }
#'     #     }
#'     #     models[[j]]$sample_base_rate <- config$sample_base_rates[[construct_name]]
#'     #     models[[j]]$reference_base_rate <- config$reference_base_rates[[construct_name]]
#'     #   } else {
#'     #     models[[j]]$sample_base_rate <- config$sample_base_rates$total
#'     #     models[[j]]$reference_base_rate <- config$reference_base_rates$total
#'     #   }
#'     # }
#'     
#'   }
#'   
#'   # Fix: (Added a warning for model convergence failures)
#'   if (n_failed_models > 0) {
#'     warning(sprintf("%d of %d models failed to converge", n_failed_models, n_items))
#'   }
#'   
#'   return(models)
#' }

# Fix: (New build_ordinal_models function to handle convergence failures)
#' Build Ordinal Regression Models
#'
#' @param data Training data
#' @param ordered_items Ordered item names
#' @param config Configuration
#' @param cutoff Classification cutoff
#' @param type "simple" for sum score or "multiple" for individual items
#' @return List with models and success indicator
build_ordinal_models <- function(data, ordered_items, config, cutoff, type = "simple") {
  n_items <- length(ordered_items)
  models <- vector("list", n_items)
  
  # Add tracking for model failures
  n_failed_models <- 0
  all_models_failed <- TRUE
  
  # Calculate full classifications based on all items and cutoff
  full_classifications <- calculate_classifications(data, ordered_items, config, cutoff)
  
  if (length(unique(full_classifications[!is.na(full_classifications)])) < 2) {
    # Return explicit failure indicator
    return(list(
      models = models,
      success = FALSE,
      error_message = "Not enough variability in outcomes to fit regression models. All samples have the same classification."
    ))
  }
  
  for (j in seq_len(n_items)) {
    # Skip if too few items (adjust threshold for small constructs)
    min_items_for_model <- min(3, ceiling(n_items * 0.5))
    if (j < min_items_for_model) next
    
    # Get items up to position j
    items_so_far <- ordered_items[1:j]
    
    # Prepare data for modeling
    if (type == "simple") {
      # Use sum score as predictor
      predictor_data <- rowSums(data[, items_so_far, drop = FALSE], na.rm = TRUE)
      model_data <- data.frame(
        outcome = factor(full_classifications),
        sum_score = predictor_data
      )
      
      # Fit logistic regression
      tryCatch({
        # Check if there's enough variability in predictors
        if (length(unique(predictor_data)) < 2) {
          models[[j]] <- NULL
          n_failed_models <- n_failed_models + 1
        } else {
          # Suppress warnings about perfect separation
          suppressWarnings({
            models[[j]] <- glm(outcome ~ sum_score, data = model_data, family = binomial())
          })
          # Check if model converged properly
          if (!models[[j]]$converged || any(is.na(coef(models[[j]])))) {
            models[[j]] <- NULL
            n_failed_models <- n_failed_models + 1
          } else {
            all_models_failed <- FALSE  # At least one model succeeded
          }
        }
      }, error = function(e) {
        models[[j]] <- NULL
        n_failed_models <- n_failed_models + 1
      })
      
    } else {
      # Use individual items as predictors
      model_data <- data.frame(
        outcome = factor(full_classifications),
        data[, items_so_far, drop = FALSE]
      )
      
      # Fit logistic regression with all items
      formula_str <- paste("outcome ~", paste(items_so_far, collapse = " + "))
      tryCatch({
        # Check if we have enough observations relative to predictors
        n_obs <- sum(!is.na(model_data$outcome))
        n_predictors <- length(items_so_far)
        
        if (n_obs < n_predictors * 5) {  # Rule of thumb: 5-10 obs per predictor
          models[[j]] <- NULL
          n_failed_models <- n_failed_models + 1
        } else {
          # Suppress warnings about perfect separation
          suppressWarnings({
            models[[j]] <- glm(as.formula(formula_str), data = model_data, family = binomial())
          })
          # Check if model converged properly
          if (!models[[j]]$converged || any(is.na(coef(models[[j]])))) {
            models[[j]] <- NULL
            n_failed_models <- n_failed_models + 1
          } else {
            all_models_failed <- FALSE  # At least one model succeeded
          }
        }
      }, error = function(e) {
        models[[j]] <- NULL
        n_failed_models <- n_failed_models + 1
      })
    }
    
    # # Store base rate information with the model if successful
    # if (!is.null(models[[j]])) {
    #   # Get appropriate base rates
    #   if (config$questionnaire_type == "multi-construct") {
    #     # Find construct name
    #     construct_name <- NULL
    #     for (c_name in names(config$constructs)) {
    #       if (all(ordered_items %in% config$constructs[[c_name]])) {
    #         construct_name <- c_name
    #         break
    #       }
    #     }
    #     models[[j]]$sample_base_rate <- config$sample_base_rates[[construct_name]]
    #     models[[j]]$reference_base_rate <- config$reference_base_rates[[construct_name]]
    #   } else {
    #     models[[j]]$sample_base_rate <- config$sample_base_rates$total
    #     models[[j]]$reference_base_rate <- config$reference_base_rates$total
    #   }
    # }
  }
  
  # Create informative error message
  error_msg <- NULL
  if (all_models_failed) {
    error_msg <- sprintf("All %d regression models failed to converge", n_failed_models)
  } else if (n_failed_models > 0) {
    warning(sprintf("%d of %d models failed to converge", n_failed_models, n_items))
  }
  
  # Return result with success indicator
  return(list(
    models = models,
    success = !all_models_failed,
    n_failed = n_failed_models,
    n_total = n_items,
    error_message = error_msg
  ))
}

#' Predict Probabilities from Simple Ordinal Model
#'
#' @param model Fitted model
#' @param sum_score Current sum score
#' @param n_items Number of items administered
#' @return Named vector of probabilities
# Fix:/Updated:
predict_ordinal_probs <- function(model, sum_score, n_items) {
  if (is.null(model)) {
    return(c(prob_low = 0.5, prob_high = 0.5))
  }
  
  new_data <- data.frame(sum_score = sum_score)
  
  tryCatch({
    pred_prob <- predict(model, newdata = new_data, type = "response")
    
    # Ensure we have a single numeric value
    if (length(pred_prob) > 0) {
      # Extract first element if vector, remove any names/attributes
      pred_prob_value <- as.numeric(pred_prob[1])
      
      if (is.na(pred_prob_value)) {
        return(c(prob_low = 0.5, prob_high = 0.5))
      }
      
      # Ensure probability is in valid range
      pred_prob_value <- max(0, min(1, pred_prob_value))
      
      # Fix:/Update: (Inserted code to adjust probability value for base rate)
      # Before returning, adjust for base rate if needed
      if (!is.null(model$reference_base_rate) && !is.null(model$sample_base_rate) &&
          abs(model$reference_base_rate - model$sample_base_rate) > 0.01) {
        prob_high_adj <- adjust_probability_for_base_rate(
          pred_prob_value,
          model$sample_base_rate,
          model$reference_base_rate
        )
        prob_high_adj <- max(0, min(1, prob_high_adj)) # Ensure adjusted probability is in valid range
        result <- c(prob_low = 1 - prob_high_adj, prob_high = prob_high_adj)
      } else {
        result <- c(prob_low = 1 - pred_prob_value, prob_high = pred_prob_value)
      }
      return(result)
      
    } else {
      return(c(prob_low = 0.5, prob_high = 0.5))
    }
  }, error = function(e) {
    return(c(prob_low = 0.5, prob_high = 0.5))
  })
}

#' Predict Probabilities from Multiple Ordinal Model
#'
#' @param model Fitted model
#' @param responses Named list of item responses
#' @param items_given Items administered
#' @return Named vector of probabilities
# Fix:/Updated:
predict_ordinal_probs_multiple <- function(model, responses, items_given) {
  if (is.null(model)) {
    return(c(prob_low = 0.5, prob_high = 0.5))
  }
  
  # Create data frame with appropriate columns - ensure correct types
  new_data <- data.frame(matrix(NA, nrow = 1, ncol = length(items_given)))
  names(new_data) <- items_given
  
  # Fill in the responses, ensuring numeric type
  for (item in names(responses)) {
    if (item %in% names(new_data)) {
      new_data[[item]] <- as.numeric(responses[[item]])
    }
  }
  
  # Check if we have valid predictor data
  if (all(is.na(new_data))) {
    return(c(prob_low = 0.5, prob_high = 0.5))
  }
  
  tryCatch({
    pred_prob <- predict(model, newdata = new_data, type = "response")
    
    # Ensure we have a single numeric value
    if (length(pred_prob) > 0) {
      # Extract first element if vector, remove any names/attributes
      pred_prob_value <- as.numeric(pred_prob[1])
      
      if (is.na(pred_prob_value)) {
        return(c(prob_low = 0.5, prob_high = 0.5))
      }
      
      # Ensure probability is in valid range
      pred_prob_value <- max(0, min(1, pred_prob_value))
      
      # Fix:/Update: (Inserted code to adjust probability value for base rate)
      # Before returning, adjust for base rate if needed
      if (!is.null(model$reference_base_rate) && !is.null(model$sample_base_rate) &&
          abs(model$reference_base_rate - model$sample_base_rate) > 0.01) {
        prob_high_adj <- adjust_probability_for_base_rate(
          pred_prob_value,
          model$sample_base_rate,
          model$reference_base_rate
        )
        prob_high_adj <- max(0, min(1, prob_high_adj)) # Ensure adjusted probability is in valid range
        result <- c(prob_low = 1 - prob_high_adj, prob_high = prob_high_adj)
      } else {
        result <- c(prob_low = 1 - pred_prob_value, prob_high = pred_prob_value)
      }
      return(result)
      
    } else {
      return(c(prob_low = 0.5, prob_high = 0.5))
    }
  }, error = function(e) {
    return(c(prob_low = 0.5, prob_high = 0.5))
  })
}

#' Fit IRT Model for CCT
#'
#' @param data Training data
#' @param ordered_items Ordered item names
#' @param config Configuration
#' @param cutoff Classification cutoff
#' @param irt_model_type Type of IRT model
#' @return List with model and theta cutoff
fit_irt_model <- function(data, ordered_items, config, cutoff, irt_model_type) {
  # Prepare data
  item_data <- data[, ordered_items]
  complete_cases <- complete.cases(item_data)
  
  # Fix: (Changed warning to error for insufficient data)
  if (sum(complete_cases) < 100) {
    stop("Insufficient data for IRT model fitting - need at least 100 complete cases")
  }
  
  item_data <- item_data[complete_cases, ]
  
  # Fit IRT model
  tryCatch({
    if (requireNamespace("mirt", quietly = TRUE)) {
      irt_model <- mirt::mirt(item_data, 1, itemtype = irt_model_type, verbose = FALSE)
      
      # Determine theta cutoff based on the raw score cutoff
      # Calculate theta values for all respondents
      theta_scores <- mirt::fscores(irt_model, method = "EAP")[, 1]
      
      # Calculate raw scores
      raw_scores <- rowSums(item_data)
      
      # Find the theta value that best separates based on the cutoff
      # Using a simple approach: find theta corresponding to the cutoff score
      cutoff_idx <- which.min(abs(raw_scores - cutoff))
      theta_cutoff <- theta_scores[cutoff_idx]
      
      # Alternative: fit a model to predict theta from raw score
      theta_model <- lm(theta_scores ~ raw_scores)
      theta_cutoff <- predict(theta_model, newdata = data.frame(raw_scores = cutoff))
      
      return(list(model = irt_model, theta_cutoff = as.numeric(theta_cutoff)))
    }
  }, error = function(e) {
    warning(paste("IRT model fitting failed:", e$message))
    return(list(model = NULL, theta_cutoff = 0))
  })
  
  return(list(model = NULL, theta_cutoff = 0))
}

#' Update Theta Estimate
#'
#' @param irt_model Fitted IRT model
#' @param items_given Items administered
#' @param responses Responses to items
#' @return List with theta estimate and standard error
update_theta_estimate <- function(irt_model, items_given, responses) {
  if (is.null(irt_model)) {
    return(list(theta = 0, se = Inf))
  }
  
  tryCatch({
    if (requireNamespace("mirt", quietly = TRUE)) {
      # Create response pattern
      pattern <- rep(NA, length(mirt::extract.mirt(irt_model, "itemnames")))
      item_names <- mirt::extract.mirt(irt_model, "itemnames")
      
      for (i in seq_along(items_given)) {
        idx <- which(item_names == items_given[i])
        if (length(idx) > 0) {
          pattern[idx] <- responses[i]
        }
      }
      
      # Estimate theta
      theta_est <- mirt::fscores(irt_model, response.pattern = pattern, method = "EAP")
      
      # Calculate standard error (simplified)
      se <- 1 / sqrt(sum(!is.na(pattern)))  # Rough approximation
      
      return(list(theta = theta_est[1], se = se))
    }
  }, error = function(e) {
    return(list(theta = 0, se = Inf))
  })
  
  return(list(theta = 0, se = Inf))
}

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Fix:/Update: (Added probability adjustment function for reference base rates)
#' Adjust Probability for Reference Base Rate
#'
#' @param prob_sample Probability calculated from sample
#' @param base_rate_sample Base rate in the training sample
#' @param base_rate_reference Reference base rate
#' @return Adjusted probability
adjust_probability_for_base_rate <- function(prob_sample, base_rate_sample, base_rate_reference) {
  # Avoid division by zero
  if (base_rate_sample == 0 || base_rate_sample == 1 || 
      base_rate_reference == 0 || base_rate_reference == 1) {
    return(prob_sample)
  }
  
  # Convert to odds
  odds_sample <- prob_sample / (1 - prob_sample)
  
  # Calculate base rate adjustment factor
  base_rate_ratio <- (base_rate_reference / (1 - base_rate_reference)) / 
    (base_rate_sample / (1 - base_rate_sample))
  
  # Adjust odds
  odds_adjusted <- odds_sample * base_rate_ratio
  
  # Convert back to probability
  prob_adjusted <- odds_adjusted / (1 + odds_adjusted)
  
  # Ensure valid range before returning
  prob_adjusted <- max(0, min(1, prob_adjusted))
  
  return(prob_adjusted)
}

# ============================================================================
# Wrapper Functions for Common Use Cases
# ============================================================================

#' Train Reduction Method
#'
#' @param method Reduction method name
#' @param ordered_items Ordered items from Module 3
#' @param train_data Training data
#' @param config Configuration
#' @param method_params Method-specific parameters
#' @return Training parameters for the method
#' @export
train_reduction_method <- function(method, ordered_items, train_data, config, 
                                   method_params = list()) {
  
  if (method == "none") {
    return(NULL)  # No training needed
  }
  
  # Run reduction on training data to extract parameters
  result <- reduce_items(
    ordered_items = ordered_items,
    data = train_data,
    config = config,
    method = method,
    method_params = method_params,
    training_params = NULL  # Force training mode
  )
  
  return(result$training_params)
}

#' Apply Trained Reduction Method
#'
#' @param method Reduction method name
#' @param ordered_items Ordered items from Module 3
#' @param test_data Test data
#' @param config Configuration
#' @param method_params Method-specific parameters
#' @param training_params Parameters from training phase
#' @return Reduction results on test data
#' @export
apply_reduction_method <- function(method, ordered_items, test_data, config,
                                   method_params = list(), training_params) {
  
  result <- reduce_items(
    ordered_items = ordered_items,
    data = test_data,
    config = config,
    method = method,
    method_params = method_params,
    training_params = training_params
  )
  
  return(result)
}