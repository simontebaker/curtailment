# Create Binary Outcome Columns for Each Scale -----

#' Create Binary Outcome Columns for Each Scale
#'
#' @param data Data frame containing the percentile columns (modified in place)
#' @param scale_names Character vector of scale/facet names
#' @param threshold Numeric threshold for binary classification (default: 93.32)
#' @param suffix Character suffix for percentile columns (default: "normative")
#' @param use_average Logical, whether to use average percentile columns (default: TRUE)
#' @param outcome_suffix Character suffix for new outcome columns (default: "_outcome")
#' @return The modified data frame with added binary outcome columns
#' @export
create_binary_outcome_columns <- function(data, 
                                          scale_names, 
                                          threshold = 93.32, 
                                          suffix = "normative", 
                                          use_average = TRUE,
                                          outcome_suffix = "_outcome") {
  
  # Process each scale
  for (scale_name in scale_names) {
    
    # Determine source column name based on use_average
    if (use_average) {
      col_name <- paste0("score_", scale_name, "_average_percentile_", suffix)
    } else {
      col_name <- paste0("score_", scale_name, "_percentile_", suffix)
    }
    
    # Create outcome column name
    outcome_col_name <- paste0(scale_name, outcome_suffix)
    
    # Check if source column exists
    if (!col_name %in% names(data)) {
      warning(paste("Column", col_name, "not found for scale", scale_name, "- skipping"))
      next
    }
    
    # Create binary outcome column directly in the data frame
    # 1 if value >= threshold and not NA, 0 otherwise
    data[[outcome_col_name]] <- ifelse(
      !is.na(data[[col_name]]) & data[[col_name]] >= threshold,
      1,
      0
    )
    
    # Optional: Report creation
    n_positive <- sum(data[[outcome_col_name]] == 1, na.rm = TRUE)
    n_total <- sum(!is.na(data[[outcome_col_name]]))
    message(sprintf("Created %s: %d/%d (%.1f%%) above threshold", 
                    outcome_col_name, n_positive, n_total, 
                    100 * n_positive / n_total))
  }
  
  # Return the modified data frame
  return(data)
}

# ============================================================================
# Usage Example
# ============================================================================

# Define PID-5-SF facet scales
facet_scales <- c(
  "anhedonia", "anxiousness", "attention_seeking", "callousness", "deceitfulness",
  "depressivity", "distractability", "eccentricity", "emotional_lability", "grandiosity",
  "hostility", "impulsivity", "intimacy_avoidance", "irresponsibility", "manipulativeness",
  "perceptual_dysregulation", "perseveration", "restricted_affectivity", "rigid_perfectionism", 
  "risk_taking", "separation_insecurity", "submissiveness", "suspiciousness", 
  "unusual_beliefs_and_experiences", "withdrawal"
)

# Create binary outcomes directly in the original data frame
data <- create_binary_outcome_columns(
  data = data,
  scale_names = facet_scales,
  threshold = 93.32,
  suffix = "normative",
  use_average = TRUE
)

# The original 'data' object now contains the new outcome columns
# Check the results
# table(data$anhedonia_outcome)
# mean(data$anhedonia_outcome)  # Proportion above threshold





# Create a subset of your data frame with only the variables required for the curtailment analysis
# Using dplyr
data_for_curtailment <- data %>%
  select(
    client_id,
    q1:q100,  # All columns from q1 to q100
    ends_with("_outcome")  # All columns ending with "_outcome"
  )

# Verify the structure
str(data_for_curtailment)
dim(data_for_curtailment)  # Should show 126 columns (1 ID + 100 items + 25 outcomes)

# Check that all expected columns are present
all(c("client_id", paste0("q", 1:100), paste0(facet_scales, "_outcome")) %in% names(data_for_curtailment))

# Save the data
write.csv(data_for_curtailment, paste0(prefix, "_data_for_curtailment.csv"), row.names=FALSE)
data_for_curtailment <- fread(paste0(prefix, "_data_for_curtailment.csv"))