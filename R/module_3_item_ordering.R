# ============================================================================
# Module 3: Item Ordering Module
# ============================================================================
# Purpose: Implement item ordering strategies that adapt based on questionnaire
#          type (unidimensional vs multi-construct)
# ============================================================================

# Note on required packages:
# - pROC
# - mirt
# - ltm

# Required packages
required_packages <- c("pROC", "ltm", "mirt")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Note: Package", pkg, "is recommended for full functionality"))
  }
}

#' Order Items Using Specified Method
#'
#' @param data Training data with item responses and outcomes
#' @param config Data configuration from Module 1
#' @param method Ordering method name
#' @param method_params Additional parameters for the ordering method
#' @param interleave Apply interleaving
#' @return A list containing ordered item names and method-specific information
#' @export
order_items <- function(data, config, method = "original", method_params = list(), interleave = TRUE) { # Added interleave parameter
  
  # Validate inputs
  if (!method %in% c("original", "auc", "incremental_auc", "correlation", 
                     "forward_stepwise", "irt", "domain_blocked")) {
    stop(paste("Unknown ordering method:", method))
  }
  
  # Route to appropriate method
  result <- switch(method,
                   original = order_original(data, config, method_params),
                   auc = order_auc(data, config, method_params),
                   incremental_auc = order_incremental_auc(data, config, method_params),
                   correlation = order_correlation(data, config, method_params),
                   forward_stepwise = order_forward_stepwise(data, config, method_params),
                   irt = order_irt(data, config, method_params),
                   domain_blocked = order_domain_blocked(data, config, method_params)
  )
  
  # Add metadata
  result$method <- method
  result$questionnaire_type <- config$questionnaire_type
  result$timestamp <- Sys.time()
  
  # Fix:/Update: (Added to enable interleaving of reordered questions/items based on their original order)
  # Apply interleaving if requested and applicable
  if (interleave && config$questionnaire_type == "multi-construct") {
    interleaved_result <- interleave_ordered_items(result$ordered_items, config, result$method_info)
    result$ordered_items <- interleaved_result$ordered_items
    result$interleaved <- interleaved_result$interleaved
    result$interleaving_info <- interleaved_result$interleaving_info
    if (!is.null(interleaved_result$method_info)) {
      result$method_info <- interleaved_result$method_info
    }
  } else {
    result$interleaved <- FALSE
  }
  
  return(result)
}

#' Original Order - Preserve Native Sequence
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters (not used)
#' @return List with ordered items
order_original <- function(data, config, method_params = list()) {
  
  # Get all items in their original order
  all_items <- unlist(config$constructs)
  
  # Validate items exist in data
  missing_items <- setdiff(all_items, names(data))
  if (length(missing_items) > 0) {
    warning(paste("Missing items in data:", paste(missing_items, collapse = ", ")))
    all_items <- intersect(all_items, names(data))
  }
  
  result <- list(
    ordered_items = all_items,
    construct_order = names(config$constructs),
    method_info = list(
      description = "Original questionnaire order preserved"
    )
  )
  
  return(result)
}

#' AUC-based Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters
#' @return List with ordered items
order_auc <- function(data, config, method_params = list()) {
  
  if (config$questionnaire_type == "unidimensional") {
    # Order all items globally by AUC
    ordered_items <- order_auc_unidimensional(data, config, method_params)
  } else {
    # Order items within each construct
    ordered_items <- order_auc_multi_construct(data, config, method_params)
  }
  
  return(ordered_items)
}

#' AUC Ordering for Unidimensional Questionnaires
order_auc_unidimensional <- function(data, config, method_params) {
  
  # Get outcome column
  outcome_col <- ifelse(config$questionnaire_type == "unidimensional",
                        config$outcome_column,
                        paste0(names(config$constructs)[1], "_outcome"))
  
  # Calculate AUC for each item
  all_items <- unlist(config$constructs)
  item_aucs <- numeric(length(all_items))
  names(item_aucs) <- all_items
  
  for (item in all_items) {
    if (item %in% names(data) && outcome_col %in% names(data)) {
      # Handle missing data
      complete_cases <- complete.cases(data[, c(item, outcome_col)])
      if (sum(complete_cases) > 10) {  # Minimum cases for AUC calculation
        if (requireNamespace("pROC", quietly = TRUE)) {
          auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                               data[[item]][complete_cases], 
                               quiet = TRUE)
          item_aucs[item] <- as.numeric(auc_obj)
        } else {
          # Fallback to simple discrimination
          if (!exists("auc_warning_shown")) {
            warning("pROC package not available. Using simplified AUC calculation.")
            auc_warning_shown <<- TRUE  # Only warn once
          }
          item_aucs[item] <- calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                                  data[[item]][complete_cases])
        }
      } else {
        item_aucs[item] <- 0.5  # No discrimination if insufficient data
      }
    } else {
      item_aucs[item] <- NA
    }
  }
  
  # Remove items with NA AUC
  valid_items <- names(item_aucs)[!is.na(item_aucs)]
  item_aucs <- item_aucs[valid_items]
  
  # Order by descending AUC
  ordered_items <- names(sort(item_aucs, decreasing = TRUE))
  
  result <- list(
    ordered_items = ordered_items,
    method_info = list(
      item_aucs = sort(item_aucs, decreasing = TRUE),
      description = "Items ordered by individual AUC (global)"
    )
  )
  
  return(result)
}

#' AUC Ordering for Multi-construct Questionnaires
order_auc_multi_construct <- function(data, config, method_params) {
  
  ordered_items <- character()
  construct_info <- list()
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    # Calculate AUC for items within this construct
    item_aucs <- numeric(length(construct_items))
    names(item_aucs) <- construct_items
    
    for (item in construct_items) {
      if (item %in% names(data) && outcome_col %in% names(data)) {
        complete_cases <- complete.cases(data[, c(item, outcome_col)])
        if (sum(complete_cases) > 10) {
          if (requireNamespace("pROC", quietly = TRUE)) {
            auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                                 data[[item]][complete_cases], 
                                 quiet = TRUE)
            item_aucs[item] <- as.numeric(auc_obj)
          } else {
            if (!exists("auc_warning_shown")) {
              warning("pROC package not available. Using simplified AUC calculation.")
              auc_warning_shown <<- TRUE  # Only warn once
            }
            item_aucs[item] <- calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                                    data[[item]][complete_cases])
          }
        } else {
          item_aucs[item] <- 0.5
        }
      } else {
        item_aucs[item] <- NA
      }
    }
    
    # Order items within construct by AUC
    valid_items <- names(item_aucs)[!is.na(item_aucs)]
    item_aucs <- item_aucs[valid_items]
    construct_ordered <- names(sort(item_aucs, decreasing = TRUE))
    
    ordered_items <- c(ordered_items, construct_ordered)
    construct_info[[construct_name]] <- list(
      ordered_items = construct_ordered,
      aucs = sort(item_aucs, decreasing = TRUE)
    )
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = names(config$constructs),
    method_info = list(
      construct_info = construct_info,
      description = "Items ordered by AUC within each construct"
    )
  )
  
  return(result)
}

#' Incremental AUC Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters
#' @return List with ordered items
order_incremental_auc <- function(data, config, method_params = list()) {
  
  if (config$questionnaire_type == "unidimensional") {
    ordered_items <- order_incremental_auc_unidimensional(data, config, method_params)
  } else {
    ordered_items <- order_incremental_auc_multi_construct(data, config, method_params)
  }
  
  return(ordered_items)
}

#' Incremental AUC for Unidimensional
order_incremental_auc_unidimensional <- function(data, config, method_params) {
  
  outcome_col <- ifelse(config$questionnaire_type == "unidimensional",
                        config$outcome_column,
                        paste0(names(config$constructs)[1], "_outcome"))
  
  all_items <- unlist(config$constructs)
  ordered_items <- character()
  remaining_items <- all_items
  incremental_aucs <- numeric()
  
  # Iteratively select items with highest incremental AUC
  while (length(remaining_items) > 0) {
    best_item <- NULL
    best_auc <- -Inf
    
    for (item in remaining_items) {
      # Calculate AUC with current items plus candidate
      test_items <- c(ordered_items, item)
      
      if (length(test_items) == 1) {
        # First item - use simple AUC
        complete_cases <- complete.cases(data[, c(item, outcome_col)])
        if (sum(complete_cases) > 10) {
          if (requireNamespace("pROC", quietly = TRUE)) {
            auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                                 data[[item]][complete_cases], 
                                 quiet = TRUE)
            current_auc <- as.numeric(auc_obj)
          } else {
            if (!exists("auc_warning_shown")) {
              warning("pROC package not available. Using simplified AUC calculation.")
              auc_warning_shown <<- TRUE  # Only warn once
            }
            current_auc <- calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                                data[[item]][complete_cases])
          }
        } else {
          current_auc <- 0.5
        }
      } else {
        # Multiple items - use sum score
        current_auc <- calculate_combined_auc(data, test_items, outcome_col)
      }
      
      if (current_auc > best_auc) {
        best_auc <- current_auc
        best_item <- item
      }
    }
    
    # Add best item to ordered list
    if (!is.null(best_item)) {
      ordered_items <- c(ordered_items, best_item)
      remaining_items <- setdiff(remaining_items, best_item)
      incremental_aucs <- c(incremental_aucs, best_auc)
    } else {
      # No improvement found, add remaining items in original order
      ordered_items <- c(ordered_items, remaining_items)
      break
    }
  }
  
  result <- list(
    ordered_items = ordered_items,
    method_info = list(
      incremental_aucs = incremental_aucs,
      description = "Items ordered by incremental AUC contribution (global)"
    )
  )
  
  return(result)
}

#' Incremental AUC for Multi-construct
order_incremental_auc_multi_construct <- function(data, config, method_params) {
  
  ordered_items <- character()
  construct_info <- list()
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    # Apply incremental AUC within construct
    construct_ordered <- character()
    remaining_items <- construct_items
    incremental_aucs <- numeric()
    
    while (length(remaining_items) > 0) {
      best_item <- NULL
      best_auc <- -Inf
      
      for (item in remaining_items) {
        test_items <- c(construct_ordered, item)
        
        if (length(test_items) == 1) {
          complete_cases <- complete.cases(data[, c(item, outcome_col)])
          if (sum(complete_cases) > 10) {
            if (requireNamespace("pROC", quietly = TRUE)) {
              auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                                   data[[item]][complete_cases], 
                                   quiet = TRUE)
              current_auc <- as.numeric(auc_obj)
            } else {
              if (!exists("auc_warning_shown")) {
                warning("pROC package not available. Using simplified AUC calculation.")
                auc_warning_shown <<- TRUE  # Only warn once
              }
              current_auc <- calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                                  data[[item]][complete_cases])
            }
          } else {
            current_auc <- 0.5
          }
        } else {
          current_auc <- calculate_combined_auc(data, test_items, outcome_col)
        }
        
        if (current_auc > best_auc) {
          best_auc <- current_auc
          best_item <- item
        }
      }
      
      if (!is.null(best_item)) {
        construct_ordered <- c(construct_ordered, best_item)
        remaining_items <- setdiff(remaining_items, best_item)
        incremental_aucs <- c(incremental_aucs, best_auc)
      } else {
        construct_ordered <- c(construct_ordered, remaining_items)
        break
      }
    }
    
    ordered_items <- c(ordered_items, construct_ordered)
    construct_info[[construct_name]] <- list(
      ordered_items = construct_ordered,
      incremental_aucs = incremental_aucs
    )
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = names(config$constructs),
    method_info = list(
      construct_info = construct_info,
      description = "Items ordered by incremental AUC within each construct"
    )
  )
  
  return(result)
}

#' Correlation-based Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters
#' @return List with ordered items
order_correlation <- function(data, config, method_params = list()) {
  
  if (config$questionnaire_type == "unidimensional") {
    ordered_items <- order_correlation_unidimensional(data, config, method_params)
  } else {
    ordered_items <- order_correlation_multi_construct(data, config, method_params)
  }
  
  return(ordered_items)
}

#' Correlation Ordering for Unidimensional
order_correlation_unidimensional <- function(data, config, method_params) {
  
  outcome_col <- ifelse(config$questionnaire_type == "unidimensional",
                        config$outcome_column,
                        paste0(names(config$constructs)[1], "_outcome"))
  
  all_items <- unlist(config$constructs)
  item_cors <- numeric(length(all_items))
  names(item_cors) <- all_items
  
  # Calculate correlation for each item
  for (item in all_items) {
    if (item %in% names(data) && outcome_col %in% names(data)) {
      complete_cases <- complete.cases(data[, c(item, outcome_col)])
      if (sum(complete_cases) > 10) {
        item_cors[item] <- abs(cor(data[[item]][complete_cases], 
                                   data[[outcome_col]][complete_cases]))
      } else {
        item_cors[item] <- 0
      }
    } else {
      item_cors[item] <- NA
    }
  }
  
  # Remove items with NA correlation
  valid_items <- names(item_cors)[!is.na(item_cors)]
  item_cors <- item_cors[valid_items]
  
  # Order by descending absolute correlation
  ordered_items <- names(sort(item_cors, decreasing = TRUE))
  
  result <- list(
    ordered_items = ordered_items,
    method_info = list(
      item_correlations = sort(item_cors, decreasing = TRUE),
      description = "Items ordered by absolute correlation with outcome (global)"
    )
  )
  
  return(result)
}

#' Correlation Ordering for Multi-construct
order_correlation_multi_construct <- function(data, config, method_params) {
  
  ordered_items <- character()
  construct_info <- list()
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    # Calculate correlations within construct
    item_cors <- numeric(length(construct_items))
    names(item_cors) <- construct_items
    
    for (item in construct_items) {
      if (item %in% names(data) && outcome_col %in% names(data)) {
        complete_cases <- complete.cases(data[, c(item, outcome_col)])
        if (sum(complete_cases) > 10) {
          item_cors[item] <- abs(cor(data[[item]][complete_cases], 
                                     data[[outcome_col]][complete_cases]))
        } else {
          item_cors[item] <- 0
        }
      } else {
        item_cors[item] <- NA
      }
    }
    
    # Order items within construct
    valid_items <- names(item_cors)[!is.na(item_cors)]
    item_cors <- item_cors[valid_items]
    construct_ordered <- names(sort(item_cors, decreasing = TRUE))
    
    ordered_items <- c(ordered_items, construct_ordered)
    construct_info[[construct_name]] <- list(
      ordered_items = construct_ordered,
      correlations = sort(item_cors, decreasing = TRUE)
    )
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = names(config$constructs),
    method_info = list(
      construct_info = construct_info,
      description = "Items ordered by correlation within each construct"
    )
  )
  
  return(result)
}

#' Forward Stepwise Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters
#' @return List with ordered items
order_forward_stepwise <- function(data, config, method_params = list()) {
  
  if (config$questionnaire_type == "unidimensional") {
    ordered_items <- order_forward_stepwise_unidimensional(data, config, method_params)
  } else {
    ordered_items <- order_forward_stepwise_multi_construct(data, config, method_params)
  }
  
  return(ordered_items)
}

#' Forward Stepwise for Unidimensional
order_forward_stepwise_unidimensional <- function(data, config, method_params) {
  
  outcome_col <- ifelse(config$questionnaire_type == "unidimensional",
                        config$outcome_column,
                        paste0(names(config$constructs)[1], "_outcome"))
  
  all_items <- unlist(config$constructs)
  ordered_items <- character()
  remaining_items <- all_items
  partial_cors <- numeric()
  
  # Iteratively select items with highest partial correlation
  while (length(remaining_items) > 0) {
    best_item <- NULL
    best_cor <- -Inf
    
    for (item in remaining_items) {
      if (length(ordered_items) == 0) {
        # First item - use simple correlation
        complete_cases <- complete.cases(data[, c(item, outcome_col)])
        if (sum(complete_cases) > 10) {
          current_cor <- abs(cor(data[[item]][complete_cases], 
                                 data[[outcome_col]][complete_cases]))
        } else {
          current_cor <- 0
        }
      } else {
        # Calculate partial correlation controlling for selected items
        current_cor <- calculate_partial_correlation(data, item, outcome_col, ordered_items)
      }
      
      if (current_cor > best_cor) {
        best_cor <- current_cor
        best_item <- item
      }
    }
    
    # Add best item
    if (!is.null(best_item) && best_cor > 0.01) {  # Minimum correlation threshold
      ordered_items <- c(ordered_items, best_item)
      remaining_items <- setdiff(remaining_items, best_item)
      partial_cors <- c(partial_cors, best_cor)
    } else {
      # No meaningful improvement, add remaining items
      ordered_items <- c(ordered_items, remaining_items)
      break
    }
  }
  
  result <- list(
    ordered_items = ordered_items,
    method_info = list(
      partial_correlations = partial_cors,
      description = "Items selected by forward stepwise regression (global)"
    )
  )
  
  return(result)
}

#' Forward Stepwise for Multi-construct
order_forward_stepwise_multi_construct <- function(data, config, method_params) {
  
  ordered_items <- character()
  construct_info <- list()
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    # Apply forward stepwise within construct
    construct_ordered <- character()
    remaining_items <- construct_items
    partial_cors <- numeric()
    
    while (length(remaining_items) > 0) {
      best_item <- NULL
      best_cor <- -Inf
      
      for (item in remaining_items) {
        if (length(construct_ordered) == 0) {
          complete_cases <- complete.cases(data[, c(item, outcome_col)])
          if (sum(complete_cases) > 10) {
            current_cor <- abs(cor(data[[item]][complete_cases], 
                                   data[[outcome_col]][complete_cases]))
          } else {
            current_cor <- 0
          }
        } else {
          current_cor <- calculate_partial_correlation(data, item, outcome_col, construct_ordered)
        }
        
        if (current_cor > best_cor) {
          best_cor <- current_cor
          best_item <- item
        }
      }
      
      if (!is.null(best_item) && best_cor > 0.01) {
        construct_ordered <- c(construct_ordered, best_item)
        remaining_items <- setdiff(remaining_items, best_item)
        partial_cors <- c(partial_cors, best_cor)
      } else {
        construct_ordered <- c(construct_ordered, remaining_items)
        break
      }
    }
    
    ordered_items <- c(ordered_items, construct_ordered)
    construct_info[[construct_name]] <- list(
      ordered_items = construct_ordered,
      partial_correlations = partial_cors
    )
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = names(config$constructs),
    method_info = list(
      construct_info = construct_info,
      description = "Items selected by forward stepwise within each construct"
    )
  )
  
  return(result)
}

#' IRT-based Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters (irt_model)
#' @return List with ordered items
order_irt <- function(data, config, method_params = list()) {
  
  # # Check if IRT package is available
  # if (!requireNamespace("mirt", quietly = TRUE) && 
  #     !requireNamespace("ltm", quietly = TRUE)) {
  #   warning("IRT packages (mirt or ltm) not available. Falling back to correlation ordering.")
  #   return(order_correlation(data, config, method_params))
  # }
  # Fix: (Removed correlation fallback)
  # Check if IRT package is available
  if (!requireNamespace("mirt", quietly = TRUE) && 
      !requireNamespace("ltm", quietly = TRUE)) {
    stop("IRT packages (mirt or ltm) not available. Cannot perform IRT-based ordering.")
  }
  
  if (config$questionnaire_type == "unidimensional") {
    ordered_items <- order_irt_unidimensional(data, config, method_params)
  } else {
    ordered_items <- order_irt_multi_construct(data, config, method_params)
  }
  
  return(ordered_items)
}

#' IRT Ordering for Unidimensional
order_irt_unidimensional <- function(data, config, method_params) {
  
  outcome_col <- ifelse(config$questionnaire_type == "unidimensional",
                        config$outcome_column,
                        paste0(names(config$constructs)[1], "_outcome"))
  
  all_items <- unlist(config$constructs)
  
  # Prepare data for IRT
  item_data <- data[, all_items]
  complete_cases <- complete.cases(item_data)
  
  if (sum(complete_cases) < 100) {
    warning("Insufficient complete cases for IRT. Falling back to correlation ordering.")
    return(order_correlation_unidimensional(data, config, method_params))
  }
  
  item_data <- item_data[complete_cases, ]
  
  # Fit IRT model and calculate information
  tryCatch({
    if (requireNamespace("mirt", quietly = TRUE)) {
      # Use mirt package
      irt_model <- mirt::mirt(item_data, 1, itemtype = "graded", verbose = FALSE)
      
      # Calculate cutoff based on outcome
      outcome_data <- data[[outcome_col]][complete_cases]
      theta_est <- mirt::fscores(irt_model, method = "EAP")[, 1]
      cutoff_theta <- mean(theta_est[outcome_data == 1])
      
      # Calculate information at cutoff
      item_info <- numeric(length(all_items))
      for (i in seq_along(all_items)) {
        item_info[i] <- mirt::iteminfo(mirt::extract.item(irt_model, i), cutoff_theta)
      }
    } else if (requireNamespace("ltm", quietly = TRUE)) {
      # Use ltm package as fallback
      # Convert to binary if needed
      item_data_binary <- apply(item_data, 2, function(x) as.numeric(x > median(x)))
      irt_model <- ltm::rasch(item_data_binary)
      
      # Simple information calculation
      item_params <- coef(irt_model)
      item_info <- abs(item_params[, "Dffclt"])  # Use difficulty as proxy for information
    }
    
    names(item_info) <- all_items
    ordered_items <- names(sort(item_info, decreasing = TRUE))
    
    result <- list(
      ordered_items = ordered_items,
      method_info = list(
        item_information = sort(item_info, decreasing = TRUE),
        cutoff_theta = ifelse(exists("cutoff_theta"), cutoff_theta, NA),
        description = "Items ordered by IRT information function (global)"
      )
    )
    
    # }, error = function(e) {
    #   warning(paste("IRT fitting failed:", e$message, "\nFalling back to correlation ordering."))
    #   return(order_correlation_unidimensional(data, config, method_params))
    # })
    # Fix: (Removed correlation fallback in error handler)
  }, error = function(e) {
    stop(paste("IRT model fitting failed:", e$message))
  })
  
  return(result)
}

#' IRT Ordering for Multi-construct
order_irt_multi_construct <- function(data, config, method_params) {
  
  ordered_items <- character()
  construct_info <- list()
  
  # Process each construct separately
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    # Prepare construct data
    item_data <- data[, construct_items]
    complete_cases <- complete.cases(item_data)
    
    # if (sum(complete_cases) < 50 || length(construct_items) < 3) {
    #   # Not enough data for IRT, use correlation
    #   warning(paste("Insufficient data for IRT in construct", construct_name, 
    #                 "- using correlation ordering"))
    #   cor_result <- order_correlation_multi_construct(
    #     data[, c(construct_items, outcome_col)], 
    #     list(questionnaire_type = "unidimensional", 
    #          constructs = list(temp = construct_items)), 
    #     method_params
    #   )
    #   construct_ordered <- cor_result$ordered_items
    #   item_info <- cor_result$method_info$item_correlations
    # Fix: (Removed correlation fallback for insufficient data)
    if (sum(complete_cases) < 50 || length(construct_items) < 3) {
      # Not enough data for IRT
      stop(paste("Insufficient data for IRT in construct", construct_name,
                 "- need at least 50 complete cases and 3 items"))
    } else {
      item_data <- item_data[complete_cases, ]
      
      tryCatch({
        if (requireNamespace("mirt", quietly = TRUE)) {
          # Fit IRT model for construct
          irt_model <- mirt::mirt(item_data, 1, itemtype = "graded", verbose = FALSE)
          
          # Calculate cutoff
          outcome_data <- data[[outcome_col]][complete_cases]
          theta_est <- mirt::fscores(irt_model, method = "EAP")[, 1]
          cutoff_theta <- mean(theta_est[outcome_data == 1])
          
          # Calculate information
          item_info <- numeric(length(construct_items))
          for (i in seq_along(construct_items)) {
            item_info[i] <- mirt::iteminfo(mirt::extract.item(irt_model, i), cutoff_theta)
          }
          names(item_info) <- construct_items
        } else {
          # Fallback to simple method
          item_data_binary <- apply(item_data, 2, function(x) as.numeric(x > median(x)))
          item_info <- apply(item_data_binary, 2, var)  # Use variance as proxy
        }
        
        construct_ordered <- names(sort(item_info, decreasing = TRUE))
        
        # }, error = function(e) {
        #   warning(paste("IRT failed for construct", construct_name, "- using correlation"))
        #   cor_result <- order_correlation_multi_construct(
        #     data[, c(construct_items, outcome_col)], 
        #     list(questionnaire_type = "unidimensional", 
        #          constructs = list(temp = construct_items)), 
        #     method_params
        #   )
        #   construct_ordered <- cor_result$ordered_items
        #   item_info <- cor_result$method_info$item_correlations
        # })
        # Fix: (Removed correlation fallback in catch block)
      }, error = function(e) {
        stop(paste("IRT failed for construct", construct_name, ":", e$message))
      })
    }
    
    ordered_items <- c(ordered_items, construct_ordered)
    construct_info[[construct_name]] <- list(
      ordered_items = construct_ordered,
      item_information = item_info[construct_ordered]
    )
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = names(config$constructs),
    method_info = list(
      construct_info = construct_info,
      description = "Items ordered by IRT information within each construct"
    )
  )
  
  return(result)
}

#' Domain-blocked Ordering
#'
#' @param data Training data
#' @param config Data configuration
#' @param method_params Additional parameters (within_block_method, construct_order_method)
#' @return List with ordered items
order_domain_blocked <- function(data, config, method_params = list()) {
  
  # # Domain-blocked only makes sense for multi-construct
  # if (config$questionnaire_type == "unidimensional") {
  #   warning("Domain-blocked ordering is designed for multi-construct questionnaires. Using AUC ordering instead.")
  #   return(order_auc(data, config, method_params))
  # }
  # Fix: (Replaced the warning and return(order_auc(...)) with a stopping error to prevent fallback to AUC ordering for questionnaire_type = "unidimensional")
  # Domain-blocked only makes sense for multi-construct
  if (config$questionnaire_type == "unidimensional") {
    stop("Domain-blocked ordering is only applicable to multi-construct questionnaires.")
  }
  
  # Set default parameters
  within_method <- method_params$within_block_method %||% "auc"
  construct_order_method <- method_params$construct_order_method %||% "max_auc"
  
  # First, determine optimal construct order
  construct_scores <- numeric(length(config$constructs))
  names(construct_scores) <- names(config$constructs)
  
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    outcome_col <- paste0(construct_name, "_outcome")
    
    if (construct_order_method == "max_auc") {
      # Use maximum item AUC within construct
      max_auc <- 0.5
      for (item in construct_items) {
        if (item %in% names(data) && outcome_col %in% names(data)) {
          complete_cases <- complete.cases(data[, c(item, outcome_col)])
          if (sum(complete_cases) > 10) {
            if (requireNamespace("pROC", quietly = TRUE)) {
              auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                                   data[[item]][complete_cases], 
                                   quiet = TRUE)
              item_auc <- as.numeric(auc_obj)
            } else {
              if (!exists("auc_warning_shown")) {
                warning("pROC package not available. Using simplified AUC calculation.")
                auc_warning_shown <<- TRUE  # Only warn once
              }
              item_auc <- calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                               data[[item]][complete_cases])
            }
            max_auc <- max(max_auc, item_auc)
          }
        }
      }
      construct_scores[construct_name] <- max_auc
      
    } else if (construct_order_method == "mean_auc") {
      # Use mean AUC of items
      aucs <- numeric()
      for (item in construct_items) {
        if (item %in% names(data) && outcome_col %in% names(data)) {
          complete_cases <- complete.cases(data[, c(item, outcome_col)])
          if (sum(complete_cases) > 10) {
            if (requireNamespace("pROC", quietly = TRUE)) {
              auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], 
                                   data[[item]][complete_cases], 
                                   quiet = TRUE)
              aucs <- c(aucs, as.numeric(auc_obj))
            } else {
              if (!exists("auc_warning_shown")) {
                warning("pROC package not available. Using simplified AUC calculation.")
                auc_warning_shown <<- TRUE  # Only warn once
              }
              aucs <- c(aucs, calculate_simple_auc(data[[outcome_col]][complete_cases], 
                                                   data[[item]][complete_cases]))
            }
          }
        }
      }
      construct_scores[construct_name] <- ifelse(length(aucs) > 0, mean(aucs), 0.5)
      
    } else if (construct_order_method == "outcome_prevalence") {
      # Order by outcome prevalence
      if (outcome_col %in% names(data)) {
        construct_scores[construct_name] <- mean(data[[outcome_col]], na.rm = TRUE)
      } else {
        construct_scores[construct_name] <- 0
      }
    }
  }
  
  # Order constructs
  ordered_constructs <- names(sort(construct_scores, decreasing = TRUE))
  
  # Now order items within each construct using specified method
  ordered_items <- character()
  within_construct_info <- list()
  
  for (construct_name in ordered_constructs) {
    # Create temporary config for within-construct ordering
    temp_config <- list(
      questionnaire_type = "unidimensional",
      constructs = list(temp = config$constructs[[construct_name]]),
      outcome_column = paste0(construct_name, "_outcome")
    )
    
    # Order items within this construct
    within_result <- order_items(data, temp_config, within_method, method_params)
    
    ordered_items <- c(ordered_items, within_result$ordered_items)
    within_construct_info[[construct_name]] <- within_result$method_info
  }
  
  result <- list(
    ordered_items = ordered_items,
    construct_order = ordered_constructs,
    method_info = list(
      construct_scores = construct_scores[ordered_constructs],
      within_construct_method = within_method,
      construct_order_method = construct_order_method,
      within_construct_info = within_construct_info,
      description = paste("Domain-blocked: constructs ordered by", construct_order_method,
                          "with", within_method, "ordering within blocks")
    )
  )
  
  return(result)
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Calculate Simple AUC (fallback when pROC not available)
calculate_simple_auc <- function(outcome, predictor) {
  # Simple trapezoidal rule AUC calculation
  o <- order(predictor)
  outcome <- outcome[o]
  predictor <- predictor[o]
  
  n_pos <- sum(outcome == 1)
  n_neg <- sum(outcome == 0)
  
  if (n_pos == 0 || n_neg == 0) return(0.5)
  
  # Calculate ranks
  ranks <- rank(predictor, ties.method = "average")
  auc <- (sum(ranks[outcome == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  
  return(auc)
}

#' Calculate Combined AUC for Multiple Items
calculate_combined_auc <- function(data, items, outcome_col) {
  # Calculate sum score and AUC
  complete_cases <- complete.cases(data[, c(items, outcome_col)])
  
  if (sum(complete_cases) < 10) return(0.5)
  
  sum_score <- rowSums(data[complete_cases, items])
  
  if (requireNamespace("pROC", quietly = TRUE)) {
    auc_obj <- pROC::auc(data[[outcome_col]][complete_cases], sum_score, quiet = TRUE)
    return(as.numeric(auc_obj))
  } else {
    if (!exists("auc_warning_shown")) {
      warning("pROC package not available. Using simplified AUC calculation.")
      auc_warning_shown <<- TRUE  # Only warn once
    }
    return(calculate_simple_auc(data[[outcome_col]][complete_cases], sum_score))
  }
}

#' Calculate Partial Correlation
calculate_partial_correlation <- function(data, item, outcome, control_items) {
  # Simple partial correlation calculation
  complete_cases <- complete.cases(data[, c(item, outcome, control_items)])
  
  if (sum(complete_cases) < 20) return(0)
  
  # Residualize item and outcome on control items
  item_resid <- resid(lm(as.formula(paste(item, "~", paste(control_items, collapse = "+"))), 
                         data = data[complete_cases, ]))
  outcome_resid <- resid(lm(as.formula(paste(outcome, "~", paste(control_items, collapse = "+"))), 
                            data = data[complete_cases, ]))
  
  return(abs(cor(item_resid, outcome_resid)))
}

# NULL-coalescing operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Fix:/Update: (Added to enable interleaving of reordered questions/items based on their original order)
#' Interleave Ordered Items Based on Original Positions
#'
#' @param ordered_items Vector of ordered item names from order_items()
#' @param config Data configuration
#' @param method_info Method information from ordering
#' @return List with interleaved items maintaining original interspersed pattern
interleave_ordered_items <- function(ordered_items, config, method_info = NULL) {
  
  # Only apply to multi-construct questionnaires
  if (config$questionnaire_type != "multi-construct") {
    return(list(
      ordered_items = ordered_items,
      interleaved = FALSE,
      method_info = method_info
    ))
  }
  
  # Extract numeric positions from item names (assuming format "qN" like "q1")
  get_item_position <- function(item_name) {
    # Extract number from item name
    pos <- as.numeric(gsub("^q", "", item_name))
    if (is.na(pos)) {
      warning(paste("Could not extract position from item:", item_name))
      return(Inf)  # Place items without positions at the end
    }
    return(pos)
  }
  
  # Create a mapping of items to their original positions
  item_positions <- sapply(ordered_items, get_item_position)
  names(item_positions) <- ordered_items
  
  # Create construct-wise ordered lists
  construct_ordered_lists <- list()
  for (construct_name in names(config$constructs)) {
    construct_items <- config$constructs[[construct_name]]
    # Get the ordered items for this construct (maintaining the order from the ordering method)
    construct_ordered <- ordered_items[ordered_items %in% construct_items]
    construct_ordered_lists[[construct_name]] <- construct_ordered
  }
  
  # Now interleave based on original positions
  interleaved_items <- character()
  
  # Create a list to track which items have been used from each construct
  construct_indices <- lapply(construct_ordered_lists, function(x) 1)
  
  # Get all unique positions and sort them
  all_positions <- sort(unique(item_positions))
  
  # For each position in ascending order
  for (pos in all_positions) {
    # Find which item has this position
    item_at_pos <- names(item_positions)[item_positions == pos]
    
    if (length(item_at_pos) > 0) {
      item <- item_at_pos[1]  # In case of ties (shouldn't happen)
      
      # Find which construct this item belongs to
      item_construct <- NULL
      for (construct_name in names(config$constructs)) {
        if (item %in% config$constructs[[construct_name]]) {
          item_construct <- construct_name
          break
        }
      }
      
      if (!is.null(item_construct)) {
        # Add the next item from this construct's ordered list
        construct_list <- construct_ordered_lists[[item_construct]]
        idx <- construct_indices[[item_construct]]
        
        if (idx <= length(construct_list)) {
          # Add the next ordered item from this construct
          next_item <- construct_list[idx]
          interleaved_items <- c(interleaved_items, next_item)
          construct_indices[[item_construct]] <- idx + 1
        }
      }
    }
  }
  
  # Add any remaining items that couldn't be interleaved (shouldn't happen with proper data)
  for (construct_name in names(construct_ordered_lists)) {
    construct_list <- construct_ordered_lists[[construct_name]]
    idx <- construct_indices[[construct_name]]
    if (idx <= length(construct_list)) {
      remaining <- construct_list[idx:length(construct_list)]
      interleaved_items <- c(interleaved_items, remaining)
    }
  }
  
  # Create detailed interleaving information
  interleaving_info <- list(
    initial_order = ordered_items,
    interleaved_order = interleaved_items,
    construct_ordered_lists = construct_ordered_lists,
    administration_sequence = data.frame(
      position = seq_along(interleaved_items),
      item = interleaved_items,
      original_position = sapply(interleaved_items, get_item_position),
      construct = sapply(interleaved_items, function(item) {
        for (c in names(config$constructs)) {
          if (item %in% config$constructs[[c]]) return(c)
        }
        return(NA)
      }),
      stringsAsFactors = FALSE
    )
  )
  
  # Update method_info if provided
  if (!is.null(method_info)) {
    method_info$interleaving_applied = TRUE
    method_info$interleaving_info = interleaving_info
  }
  
  return(list(
    ordered_items = interleaved_items,
    interleaved = TRUE,
    method_info = method_info,
    interleaving_info = interleaving_info
  ))
}

# ============================================================================
# Example Usage
# ============================================================================

# # Example 1: Order items using AUC method
# ordered_result <- order_items(
#   data = prepared_data$splits$train,
#   config = prepared_data$config,
#   method = "auc"
# )
# 
# print(paste("First 10 ordered items:", paste(ordered_result$ordered_items[1:10], collapse = ", ")))
# print(paste("Method:", ordered_result$method_info$description))
# 
# # Example 2: Order items using domain-blocked method
# ordered_result_blocked <- order_items(
#   data = prepared_data$splits$train,
#   config = prepared_data$config,
#   method = "domain_blocked",
#   method_params = list(
#     within_block_method = "incremental_auc",
#     construct_order_method = "max_auc"
#   )
# )
# 
# print(paste("Construct order:", paste(ordered_result_blocked$construct_order, collapse = ", ")))
# print(paste("First construct items:", 
#             paste(ordered_result_blocked$ordered_items[1:4], collapse = ", ")))
# 
# # Example 3: Compare multiple ordering methods
# methods_to_compare <- c("original", "auc", "correlation", "forward_stepwise")
# 
# comparison_results <- list()
# for (method in methods_to_compare) {
#   cat(paste("\nOrdering with method:", method, "\n"))
#   result <- order_items(
#     data = prepared_data$splits$train,
#     config = prepared_data$config,
#     method = method
#   )
#   comparison_results[[method]] <- result
#   cat(paste("First 5 items:", paste(result$ordered_items[1:5], collapse = ", "), "\n"))
# }