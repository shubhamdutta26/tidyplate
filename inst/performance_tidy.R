# Load required packages
library(microbenchmark)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyplate)
library(plater)

# Function to perform benchmarking for a single plate format
benchmark_plate_format <- function(file_path, plate_format, times = 50) {
  tryCatch({
    benchmark_results <- microbenchmark(
      tidyplate = tidyplate::tidy_plate(file_path),
      plater = plater::read_plate(file_path),
      times = times,
      unit = "ms"
    )
    
    # Convert to data frame and add plate format information
    results_df <- as.data.frame(benchmark_results) %>%
      mutate(
        plate_format = plate_format,
        time_ms = time / 1e6  # Convert nanoseconds to milliseconds
      )
    
    return(results_df)
  }, error = function(e) {
    warning(sprintf("Error processing %s plate format: %s", plate_format, e$message))
    return(NULL)
  })
}

# Function to run benchmarks for all plate formats
benchmark_all_formats <- function(base_path = "extdata", times = 50) {
  # Define plate formats to test
  plate_formats <- c("6", "12", "24", "48", "96", "384", "1536")
  
  # Initialize list to store results
  all_results <- list()
  
  # Run benchmarks for each format
  for (format in plate_formats) {
    file_path <- file.path(base_path, sprintf("example_%s_well.csv", format))
    if (file.exists(file_path)) {
      results <- benchmark_plate_format(file_path, format, times)
      if (!is.null(results)) {
        all_results[[format]] <- results
      }
    } else {
      warning(sprintf("File not found for %s-well plate format: %s", format, file_path))
    }
  }
  
  # Combine all results
  combined_results <- bind_rows(all_results)
  
  # Calculate summary statistics
  summary_stats <- combined_results %>%
    group_by(plate_format, expr) %>%
    summarise(
      median_time = median(time_ms),
      mean_time = mean(time_ms),
      sd_time = sd(time_ms),
      min_time = min(time_ms),
      max_time = max(time_ms),
      .groups = "drop"
    )
  
  # Create performance ratio statistics
  performance_ratio <- summary_stats %>%
    select(plate_format, expr, median_time) %>%
    pivot_wider(names_from = expr, values_from = median_time) %>%
    mutate(ratio = plater / tidyplate)
  
  combined_results$plate_format <- factor(
    combined_results$plate_format,
    levels = c("6", "12", "24", "48", "96", "384", "1536")
  )
  
  # Create visualizations
  box_plot <- ggplot(combined_results, aes(x = plate_format, y = time_ms, fill = expr)) +
    geom_boxplot(alpha = 0.7) +
    scale_y_log10() +  # Log scale for better visualization of differences
    theme_bw() +
    labs(title = "Performance comparison",
         x = "Plate format",
         y = "Time (milliseconds, log scale)",
         fill = "Implementation") +
    scale_fill_discrete(labels = c("tidyplate::tidy_plate()", "plater::read_plate()")) + 
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.2, 0.7),
      text = element_text(size = 12)
    )
  
  # Create bar plot with error bars
  # First calculate mean and standard deviation for each group
  summary_for_plot <- combined_results %>%
    group_by(plate_format, expr) %>%
    summarise(
      mean_time = mean(time_ms),
      sd_time = sd(time_ms),
      .groups = "drop"
    )
  
  bar_plot <- ggplot(summary_for_plot, aes(x = plate_format, y = mean_time, fill = expr)) +
    # Add bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
    # Add error bars
    geom_errorbar(aes(ymin = mean_time - sd_time, 
                      ymax = mean_time + sd_time),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    # Use log scale for better visualization of differences
    scale_y_log10() +
    # Customize appearance
    theme_minimal() +
    labs(title = "Mean Performance Comparison Across Plate Formats",
         x = "Plate Format (wells)",
         y = "Time (milliseconds, log scale)",
         fill = "Implementation") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank()) +
    # Use a color-blind friendly palette
    scale_fill_manual(values = c("#E69F00", "#56B4E9"))
  
  # Create scaling plot
  scaling_plot <- ggplot(
    summary_stats, 
    aes(
      x = as.numeric(plate_format), 
      y = median_time, 
      color = expr)) +
    geom_line() +
    geom_point() +
    scale_x_log10(breaks = as.numeric(plate_formats)) +
    scale_y_log10() +
    theme_minimal() +
    labs(title = "Performance Scaling with Plate Size",
         x = "Number of Wells (log scale)",
         y = "Median Time (milliseconds, log scale)",
         color = "Implementation") +
    theme(text = element_text(size = 12))
  
  return(list(
    summary_stats = summary_stats,
    performance_ratio = performance_ratio,
    box_plot = box_plot,
    bar_plot=bar_plot,
    scaling_plot = scaling_plot,
    raw_data = combined_results
  ))
}

# Run the comprehensive benchmark
results <- benchmark_all_formats()

results$summary_stats$plate_format <- as.integer(results$summary_stats$plate_format)
results$performance_ratio$plate_format <- as.integer(results$performance_ratio$plate_format)

# Print summary statistics
results$summary_stats %>% 
  arrange(plate_format, expr) %>%
  mutate(across(where(is.numeric), round, 3))

results$performance_ratio %>%
  arrange(plate_format) %>%
  mutate(across(where(is.numeric), round, 3))

# Display the visualizations
p<-results$box_plot +
  coord_cartesian(ylim = c(NA, 150))
ggsave("comparison_plot.png", p, dpi = 300, height = 4, width = 7)

results$bar_plot
results$scaling_plot +
  coord_cartesian(ylim = c(NA, 120))

# Generate a textual analysis of the results
results$performance_ratio %>%
  arrange(plate_format) %>%
  group_by(plate_format) %>%
  summarise(
    ratio = round(ratio, 2),
    interpretation = ifelse(
      ratio > 1,
      sprintf("plater is %.2fx slower than tidyplate", ratio),
      sprintf("plater is %.2fx faster than tidyplate", 1/ratio))
  ) %>%
  {cat(sprintf("%s-well plate: %s\n", .$plate_format, .$interpretation))}
