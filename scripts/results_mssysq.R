setwd(run_dir)

schema_scales <- c(
  "Abandonment_Anxious_Attachment", "Emotional_Deprivation",
  "Mistrust_of_Others", "Social_Isolation_Outsider",
  "Defectiveness_Shame", "Vulnerability_to_Dangerous_World",
  "Dependence", "Failure_Achievement_Inferiority",
  "Low_Self_Efficacy_Weakness", "Enmeshment_Diffuse_Boundaries",
  "Subjugation_Submission_to_Others", "Self_Sacrifice",
  "Approval_Seeking_Excessive_Need_to_be_Liked", "Emotional_Inhibition",
  "Pessimism_Negativity", "Unrelenting_Standards",
  "Punitiveness_Unforgiving_of_Self", "Punitiveness_Unforgiving_of_Others",
  "Entitlement_Specialness"
)

##### Base Rates, Optimised Gamma Values, Achieved False Negative Rates, and Classification Rates by Schema

# Create an empty data frame
results_table <- data.frame(
  scale_name = character(length(schema_scales)),
  base_rate_pct = numeric(length(schema_scales)),
  optimised_gamma_0 = numeric(length(schema_scales)),
  achieved_fnr_pct = numeric(length(schema_scales)),
  testing_class_rate_pct = numeric(length(schema_scales)),
  optimised_class_rate_pct = numeric(length(schema_scales)),
  class_rate_diff_pct = numeric(length(schema_scales)),
  stringsAsFactors = FALSE
)

# Calculate classification rates from testing data
calculated_base_rates_test <- calculate_base_rates(prepared_data$splits$test, config_module_1)

# Loop through each scale
for (i in seq_along(schema_scales)) {
  scale_name <- schema_scales[i]
  base_rate <- prepared_data$config$base_rates[[scale_name]]
  optimised_gamma_0 <- optimization_results$optimization_analysis$optimized_gammas[[scale_name]]$gamma_0
  achieved_fnr <- optimization_results$optimization_analysis$optimized_performance$construct_metrics[[scale_name]]$fnr
  testing_class_rate <- calculated_base_rates_test[[scale_name]]
  optimised_class_rate <- sum(optimization_results$optimized_method$reduction_result$construct_classifications[[scale_name]] == 1) / 
    length(optimization_results$optimized_method$reduction_result$construct_classifications[[scale_name]])
  # class_rate_diff <- optimised_class_rate - testing_class_rate
  
  # Fill in the table
  results_table$scale_name[i] <- scale_name
  results_table$base_rate_pct[i] <- round(base_rate * 100, 0)
  results_table$optimised_gamma_0[i] <- round(optimised_gamma_0, 2)
  results_table$achieved_fnr_pct[i] <- round(achieved_fnr * 100, 2)
  results_table$testing_class_rate_pct[i] <- round(testing_class_rate * 100, 2)
  results_table$optimised_class_rate_pct[i] <- round(optimised_class_rate * 100, 2)
  # results_table$class_rate_diff_pct[i] <- round(class_rate_diff * 100, 2)
  results_table$class_rate_diff_pct[i] <- round(results_table$optimised_class_rate_pct[i] - results_table$testing_class_rate_pct[i], 2)
}

# Write to CSV
write.csv(results_table, "mss-ysq-dynamic_results_table_base_rates_optimised_gamma_values_achieved_fnrs_class_rates.csv", row.names = FALSE)

# Clean up environment
rm(i, scale_name, base_rate, optimised_gamma_0, achieved_fnr, results_table)

##### Number of Items Administered by Schema

# Create an empty data frame
results_table <- data.frame(
  scale_name = character(length(schema_scales)),
  mean = numeric(length(schema_scales)),
  sd = numeric(length(schema_scales)),
  median = numeric(length(schema_scales)),
  reduction_pct = numeric(length(schema_scales)),
  stringsAsFactors = FALSE
)

# Loop through each scale
for (i in seq_along(schema_scales)) {
  scale_name <- schema_scales[i]
  mean <- optimization_results$optimization_analysis$optimized_performance$construct_reduction_info[[scale_name]]$mean_items_used
  sd <- optimization_results$optimization_analysis$optimized_performance$construct_reduction_info[[scale_name]]$sd_items_used
  median <- optimization_results$optimization_analysis$optimized_performance$construct_reduction_info[[scale_name]]$median_items_used
  reduction_pct <- optimization_results$optimization_analysis$optimized_performance$construct_reduction_info[[scale_name]]$reduction_pct
  
  # Fill in the table
  results_table$scale_name[i] <- scale_name
  results_table$mean[i] <- round(mean, 2)
  results_table$sd[i] <- round(sd, 2)
  results_table$median[i] <- round(mean, 0)
  results_table$reduction_pct[i] <- round(reduction_pct, 2)
}

# Write to CSV
write.csv(results_table, "mss-ysq-dynamic_results_table_number_of_items_administered.csv", row.names = FALSE)

# Clean up environment
rm(i, scale_name, mean, sd, median, reduction_pct, results_table)

##### Total Number of Items Administered

### Selected method results



### Optimisation results

# min(optimization_results$optimized_method$reduction_result$n_items_used)
# max(optimization_results$optimized_method$reduction_result$n_items_used)
# min(optimization_results$optimized_method$reduction_result$stopped_at)
# max(optimization_results$optimized_method$reduction_result$stopped_at)

optimization_results$optimization_analysis$optimized_performance$mean_items_used
optimization_results$optimization_analysis$optimized_performance$sd_items_used
optimization_results$optimization_analysis$optimized_performance$median_items_used
min(optimization_results$optimization_analysis$optimized_reduction_result$n_items_used)
max(optimization_results$optimization_analysis$optimized_reduction_result$n_items_used)
# min(optimization_results$optimization_analysis$optimized_reduction_result$stopped_at)
# max(optimization_results$optimization_analysis$optimized_reduction_result$stopped_at)
optimization_results$optimization_analysis$optimized_performance$reduction_pct

# optimization_results$optimized_performance$mean_items_used
# optimization_results$optimized_performance$sd_items_used
# optimization_results$optimized_performance$median_items_used
# optimization_results$optimized_performance$reduction_pct

### Deployment validation results

validation_results$deployment_performance$mean_items_used
validation_results$deployment_performance$sd_items_used
validation_results$deployment_performance$median_items_used
min(validation_results$simulation_results$n_items_used)
max(validation_results$simulation_results$n_items_used)
# min(validation_results$simulation_results$stopped_at)
# max(validation_results$simulation_results$stopped_at)
validation_results$deployment_performance$reduction_pct

# validation_results$performance_comparison$deployment_performance$mean_items_used
# validation_results$performance_comparison$deployment_performance$sd_items_used
# validation_results$performance_comparison$deployment_performance$median_items_used
# validation_results$performance_comparison$deployment_performance$reduction_pct
