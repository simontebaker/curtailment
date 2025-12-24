setwd(run_dir)

schema_scales <- c(
  "Abandonment_Anxious_Attachment", "Excessive_Self_Reliance_Avoidant_Attachment",
  "Emotional_Deprivation", "Mistrust_of_Others", "Others_are_Dangerous_Malevolent",
  "Social_Isolation_Outsider", "Defectiveness_Shame", "Vulnerability_to_Dangerous_World",
  "Dependence", "Failure_Achievement_Inferiority", "Low_Self_Efficacy_Weakness",
  "Fatalistic_External_Locus_of_Control", "Enmeshment_Diffuse_Boundaries", "Subjugation",
  "Self_Sacrifice", "Approval_Seeking", "Emotional_Inhibition", "Pessimism_Negativity",
  "Unrelenting_Standards", "Punitiveness_Unforgiving_of_Self", "Punitiveness_Unforgiving_of_Others",
  "Entitlement_Specialness", "Full_Control", "Over_Reliance_on_Emotions", "Unfairness",
  "Meaningless_World", "Lack_of_Coherent_Identity"
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
write.csv(results_table, "mss-dynamic_results_table_base_rates_optimised_gamma_values_achieved_fnrs_class_rates.csv", row.names = FALSE)

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
write.csv(results_table, "mss-dynamic_results_table_number_of_items_administered.csv", row.names = FALSE)

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


##### Real-World Deployment

# ============================================================================
# Load packages
# ============================================================================

# source("/Users/simonbaker/GitHub/generic/R/load_packages.R")

# ============================================================================
# Load functions
# ============================================================================

source("/Users/simonbaker/GitHub/generic/R/data_cleaning_with_inferred_assessment_type.R")

# ============================================================================
# Set Directory Paths
# ============================================================================

# Set code directory
code_dir <- "/Users/simonbaker/GitHub/sandbox/scripts"

# Set data directory
data_dir <- "/Users/simonbaker/GitHub/sandbox/data"

# Set output directory
output_dir <- "/Users/simonbaker/GitHub/sandbox/output"

# ============================================================================
# Set prefix
# ============================================================================

# Set prefix to use when writing files to output directory
prefix <- "mss"

# ============================================================================
# Load the data
# ============================================================================

cat("\n=== LOADING DATA ===\n")

# Set data file name
data_file <- "mss-dynamic_20251223_1100.csv"

# Set data file path
data_path <- file.path(data_dir, data_file)

# Read in the data
df <- fread(data_path, fill = TRUE)

# Convert data.frame to data.table
setDT(df)

# ============================================================================
# Clean the data
# ============================================================================

cat("\n=== CLEANING DATA ===\n")

data <- clean_assessment_data(df, remove_direct_assessments = FALSE, remove_sensitive_columns = FALSE)

# ============================================================================
# Keep Website Administrations
# ============================================================================

cat("\n=== KEEPING WEBSITE ADMINISTRATIONS ===\n")

# Keep only rows where assessor first name is "Website" AND assessor last name is "Administration"
data <- data[data$assessor_first_name == "Website" & 
               data$assessor_last_name == "Administration", ]

# ============================================================================
# Define subscales
# ============================================================================

cat("\n=== DEFINING SUBSCALES ===\n")

subscales <- c(
  "abandonment_/_anxious_attachment", "excessive_self-reliance_/_avoidant_attachment",
  "emotional_deprivation", "mistrust_of_others", "others_are_dangerous_/_malevolent",
  "social_isolation_/_outsider", "defectiveness_/_shame", "vulnerability_to_dangerous_world",
  "dependence", "failure_/_achievement_inferiority", "low_self-efficacy_/_weakness",
  "fatalistic_/_external_locus_of_control", "enmeshment_/_diffuse_boundaries", "subjugation_/_submission_to_others",
  "self-sacrifice", "approval-seeking_/_excessive_need_to_be_liked", "emotional_inhibition", "pessimism_/_negativity",
  "unrelenting_standards", "punitiveness_/_unforgiving_of_self", "punitiveness_/_unforgiving_of_others",
  "entitlement_/_specialness", "full_control", "over-reliance_on_emotions", "unfairness",
  "meaningless_world", "lack_of_coherent_identity"
)

print(subscales)

# ============================================================================
# Calculate metrics
# ============================================================================

cat("\n=== CALCULATING METRICS ===\n")

# Overall metrics based on score_total_answered_count
overall_metrics <- data.frame(
  Scale = "Total",
  Mean = round(mean(data$score_total_answered_count, na.rm = TRUE), 2),
  SD = round(sd(data$score_total_answered_count, na.rm = TRUE), 2),
  Median = round(median(data$score_total_answered_count, na.rm = TRUE), 2),
  Minimum = min(data$score_total_answered_count, na.rm = TRUE),
  Maximum = max(data$score_total_answered_count, na.rm = TRUE),
  Percent_Reduction = round(mean((108 - data$score_total_answered_count) / 108 * 100, na.rm = TRUE), 2)
)

# Calculate IQR (25th and 75th percentiles)
q25 <- quantile(data$score_total_answered_count, 0.25, na.rm = TRUE)
q75 <- quantile(data$score_total_answered_count, 0.75, na.rm = TRUE)

cat("Overall metrics calculated\n")
print(overall_metrics)

cat("Q1 (25th percentile):", q25, "\n")
cat("Q3 (75th percentile):", q75, "\n")

# Subscale metrics
subscale_metrics_list <- list()

for (subscale in subscales) {
  col_name <- paste0("score_", subscale, "_answered_count")
  
  if (col_name %in% names(data)) {
    subscale_data <- data[[col_name]]
    
    subscale_metrics_list[[subscale]] <- data.frame(
      Subscale = subscale,
      Mean = round(mean(subscale_data, na.rm = TRUE), 2),
      SD = round(sd(subscale_data, na.rm = TRUE), 2),
      Median = round(median(subscale_data, na.rm = TRUE), 2),
      Percent_Reduction = round(mean((4 - subscale_data) / 4 * 100, na.rm = TRUE), 2)
    )
  }
}

# Combine all subscale metrics into one data frame
subscale_metrics_df <- do.call(rbind, subscale_metrics_list)
rownames(subscale_metrics_df) <- NULL

cat("Subscale metrics calculated\n")
print(head(subscale_metrics_df))

# ============================================================================
# Create visualizations and HTML output
# ============================================================================

cat("\n=== CREATING HTML OUTPUT ===\n")

# Load required packages for visualization and HTML output
library(ggplot2)
library(knitr)
library(htmltools)

# Create frequency histogram
histogram_plot <- ggplot(data, aes(x = score_total_answered_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.7) +
  theme_minimal() +
  labs(
    # title = "Distribution of Total Items Administered",
    # subtitle = paste0("N = ", nrow(data), " administrations"),
    x = "Number of Items Administered",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(0, 108, by = 10)) # +
# geom_vline(aes(xintercept = mean(score_total_answered_count, na.rm = TRUE)),
#            color = "red", linetype = "dashed", linewidth = 1) +
# geom_vline(aes(xintercept = median(score_total_answered_count, na.rm = TRUE)),
#            color = "darkgreen", linetype = "dashed", linewidth = 1) +
# annotate("text", x = mean(data$score_total_answered_count, na.rm = TRUE) + 5, 
#          y = Inf, label = paste0("Mean = ", round(mean(data$score_total_answered_count, na.rm = TRUE), 1)),
#          vjust = 2, hjust = 0, color = "red") +
# annotate("text", x = median(data$score_total_answered_count, na.rm = TRUE) - 5, 
#          y = Inf, label = paste0("Median = ", round(median(data$score_total_answered_count, na.rm = TRUE), 1)),
#          vjust = 3.5, hjust = 1, color = "darkgreen")

# Save histogram as temporary image
temp_hist <- tempfile(fileext = ".png")
ggsave(temp_hist, histogram_plot, width = 10, height = 6, dpi = 150)

# Create density plot for total items administered
temp_density_items <- tempfile(fileext = ".png")
png(temp_density_items, width = 800, height = 600)
par(mar = c(5, 4, 4, 2) + 0.1)

# Set color for density plot
color_items <- "mediumseagreen"
transparency <- 0.5

# Function to add transparency
add_transparency <- function(color, alpha = transparency) {
  rgb_col <- col2rgb(color)
  return(rgb(rgb_col[1], rgb_col[2], rgb_col[3], alpha * 255, maxColorValue = 255))
}

fill_color_items <- add_transparency(color_items)

# Calculate density
items_data <- data$score_total_answered_count[!is.na(data$score_total_answered_count)]
density_items <- density(items_data)

# Set plot limits
x_min_items <- floor(min(items_data))
x_max_items <- ceiling(max(items_data))
y_max_items <- max(density_items$y) * 1.05

# Create empty plot
plot(0, 0, type = "n",
     xlim = c(x_min_items, x_max_items),
     ylim = c(0, y_max_items),
     xlab = "Number of Items Administered",
     ylab = "Density",
     xaxs = "i",
     yaxs = "i",
     xaxt = "n")

# Add custom x-axis with 10-item intervals
axis(1, at = seq(0, ceiling(x_max_items/10)*10, by = 10))

# Add density
lines(density_items, col = color_items, lwd = 2)
polygon(c(density_items$x, rev(density_items$x)), c(density_items$y, rep(0, length(density_items$y))),
        col = fill_color_items, border = NA)

# Add grid
grid(lty = "dotted", col = "gray90")

dev.off()

cat("Density plot for total items administered created\n")

# Create HTML content
html_content <- tags$html(
  tags$head(
    tags$title("MSS-Dynamic Items Administered Report"),
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
        background-color: #f5f5f5;
      }
      h1 {
        color: #2c3e50;
        border-bottom: 3px solid #3498db;
        padding-bottom: 10px;
      }
      h2 {
        color: #34495e;
        margin-top: 30px;
      }
      .metric-box {
        background-color: white;
        border-radius: 8px;
        padding: 20px;
        margin: 20px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      table {
        border-collapse: collapse;
        width: 100%;
        margin-top: 20px;
        font-family: Arial, sans-serif;
        border: 1px solid #ddd;
      }
      th, td {
        border: 1px solid #ddd;
        padding: 8px;
        text-align: left;
      }
      th {
        background-color: #f2f2f2;
        font-weight: bold;
      }
    "))
  ),
  tags$body(
    tags$h1("MSS-Dynamic Items Administered Report"),
    tags$p(paste("Report generated on:", Sys.Date())),
    tags$p(paste("Total administrations analyzed:", nrow(data))),
    
    tags$div(class = "metric-box",
             tags$h2("Overall"),
             tags$p("The total scale originally contains 108 items. The table below shows the overall statistics."),
             HTML(paste0(
               "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>",
               "<thead>",
               "<tr>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'></th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Mean</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>SD</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Median</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Minimum</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Maximum</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>% Reduction</th>",
               "</tr>",
               "</thead>",
               "<tbody>",
               "<tr>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center; font-weight: bold;'>Total</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", overall_metrics$Mean), "</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", overall_metrics$SD), "</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.0f", overall_metrics$Median), "</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", overall_metrics$Minimum, "</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", overall_metrics$Maximum, "</td>",
               "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", overall_metrics$Percent_Reduction), "</td>",
               "</tr>",
               "</tbody>",
               "</table>"
             ))
    ),
    
    tags$div(class = "metric-box",
             tags$h2("Distribution of Total Items Administered"),
             tags$img(src = paste0("data:image/png;base64,", base64enc::base64encode(temp_hist)),
                      style = "width: 100%; max-width: 900px; display: block; margin: 0 auto;"),
             tags$img(src = paste0("data:image/png;base64,", base64enc::base64encode(temp_density_items)),
                      style = "width: 100%; max-width: 900px; display: block; margin: 0 auto;")
    ),
    
    tags$div(class = "metric-box",
             tags$h2("Subscale-Specific"),
             tags$p("Each subscale originally contains 4 items. The table below shows the statistics for each subscale."),
             HTML(paste0(
               "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>",
               "<thead>",
               "<tr>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Subscale</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Mean</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>SD</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>Median</th>",
               "<th style='border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2; font-weight: bold;'>% Reduction</th>",
               "</tr>",
               "</thead>",
               "<tbody>",
               paste0(lapply(1:nrow(subscale_metrics_df), function(i) {
                 paste0(
                   "<tr>",
                   "<td style='border: 1px solid #ddd; padding: 8px; text-align: center; font-weight: bold;'>", subscale_metrics_df$Subscale[i], "</td>",
                   "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", subscale_metrics_df$Mean[i]), "</td>",
                   "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", subscale_metrics_df$SD[i]), "</td>",
                   "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.0f", subscale_metrics_df$Median[i]), "</td>",
                   "<td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>", sprintf("%.2f", subscale_metrics_df$Percent_Reduction[i]), "</td>",
                   "</tr>"
                 )
               }), collapse = ""),
               "</tbody>",
               "</table>"
             ))
    )
  )
)

# Write HTML file
output_file <- file.path(output_dir, paste0(prefix, "_items_administered_", 
                                            format(Sys.Date(), "%Y%m%d"), ".html"))

# Save the HTML content
html_string <- as.character(html_content)
writeLines(html_string, output_file)

# Clean up temporary file
unlink(temp_hist)
unlink(temp_density_items)

# cat(paste("\n=== HTML REPORT SAVED ===\n"))
cat(paste("Output file:", output_file, "\n"))

# Also save the metrics as CSV files for reference
write.csv(overall_metrics, 
          file.path(output_dir, paste0(prefix, "_items_administered_stats_overall_", 
                                       format(Sys.Date(), "%Y%m%d"), ".csv")), 
          row.names = FALSE)

write.csv(subscale_metrics_df, 
          file.path(output_dir, paste0(prefix, "_items_administered_stats_subscale_specific_", 
                                       format(Sys.Date(), "%Y%m%d"), ".csv")), 
          row.names = FALSE)

cat("CSV files also saved for reference\n")
