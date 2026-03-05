#!/usr/bin/env Rscript

#' script: example_analysis.R
#' Description: Example script showing how to analyze and visualize
#'              the output from the T2D TSV model
#' 
#' Prerequisites: You must have already run run_T2D_model.R successfully,
#'                which generates:
#'                - output_status_quo.csv
#'                - output_scenario_total_control.csv
#'                - output_scenario_total_control_MF.csv
#'                - output_scenario_GLP1.csv
#'                - output_scenario_GLP1_MF.csv

## Setup ########################################################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Check that output files exist
required_files <- c(
  "outputs/output_status_quo.csv",
  "outputs/output_scenario_total_control.csv",
  "outputs/output_scenario_total_control_MF.csv",
  "outputs/output_scenario_GLP1.csv",
  "outputs/output_scenario_GLP1_MF.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("ERROR: Output files not found!\n")
  cat("Missing files:\n")
  for (f in missing_files) {
    cat(sprintf("  - %s\n", f))
  }
  cat("\nPlease run 'Rscript run_T2D_model.R' first to generate output files.\n")
  quit(status = 1)
}

cat("=== T2D TSV Model - Example Analysis ===\n\n")

## 1. Load Data #################################################################

cat("Loading output data...\n")
status_quo <- read.csv("outputs/output_status_quo.csv")
scenario_a <- read.csv("outputs/output_scenario_total_control.csv")
scenario_a1 <- read.csv("outputs/output_scenario_total_control_MF.csv")
scenario_b <- read.csv("outputs/output_scenario_GLP1.csv")
scenario_b1 <- read.csv("outputs/output_scenario_GLP1_MF.csv")

cat(sprintf("  Base case (Status Quo): %d years of data (%.0f to %.0f)\n",
            nrow(status_quo), min(status_quo$time), max(status_quo$time)))
cat(sprintf("  Scenario A (Total Control): %d years of data (%.0f to %.0f)\n",
            nrow(scenario_a), min(scenario_a$time), max(scenario_a$time)))
cat(sprintf("  Scenario A1 (Total Control & MF): %d years of data (%.0f to %.0f)\n",
            nrow(scenario_a1), min(scenario_a1$time), max(scenario_a1$time)))
cat(sprintf("  Scenario B (GLP1): %d years of data (%.0f to %.0f)\n",
            nrow(scenario_b), min(scenario_b$time), max(scenario_b$time)))
cat(sprintf("  Scenario B1 (GLP1 & MF): %d years of data (%.0f to %.0f)\n\n",
            nrow(scenario_b1), min(scenario_b1$time), max(scenario_b1$time)))

## 2. Calculate Key Outcomes ###################################################

cat("=== KEY OUTCOMES COMPARISON ===\n\n")

# Function to calculate summary statistics
calculate_summary <- function(data, scenario_name) {
  # Remove last row (contains NAs)
  data <- head(data, -1)
  
  # Total complications over entire period
  total_macro <- sum(data$fC_Ma, na.rm = TRUE)
  total_micro <- sum(data$fC_Mi, na.rm = TRUE)
  total_other <- sum(data$fC_o, na.rm = TRUE)
  total_complications <- total_macro + total_micro + total_other
  
  # Final year population (last row before NAs)
  final_idx <- nrow(data)
  pop_no_diabetes <- data$f_D0_aX[final_idx]
  pop_uncontrolled <- data$f_D1_aX[final_idx]
  pop_controlled <- data$f_D2_aX[final_idx]
  total_pop <- data$f_P[final_idx]
  
  # Total deaths
  total_deaths <- sum(
    data$D_acm_D0_a1, data$D_acm_D1_a1, data$D_acm_D2_a1,
    data$D_acm_D0_a2, data$D_acm_D1_a2, data$D_acm_D2_a2,
    data$D_acm_D0_a3, data$D_acm_D1_a3, data$D_acm_D2_a3,
    na.rm = TRUE
  )
  
  # Health economics (cumulative)
  if ("direct_costs" %in% names(data)) {
    total_direct_costs <- sum(data$direct_costs, na.rm = TRUE)
    total_indirect_costs <- sum(data$indirect_costs, na.rm = TRUE)
    total_qaly_loss <- sum(data$disutilities, na.rm = TRUE)
    total_lyl <- sum(data$LYL, na.rm = TRUE)
  } else {
    total_direct_costs <- NA
    total_indirect_costs <- NA
    total_qaly_loss <- NA
    total_lyl <- NA
  }
  
  return(list(
    scenario = scenario_name,
    total_complications = total_complications,
    macro_complications = total_macro,
    micro_complications = total_micro,
    other_complications = total_other,
    final_population = total_pop,
    final_no_diabetes = pop_no_diabetes,
    final_uncontrolled = pop_uncontrolled,
    final_controlled = pop_controlled,
    total_deaths = total_deaths,
    direct_costs = total_direct_costs,
    indirect_costs = total_indirect_costs,
    qaly_loss = total_qaly_loss,
    life_years_lost = total_lyl
  ))
}

sq_summary <- calculate_summary(status_quo, "Base case (Status Quo)")
a_summary <- calculate_summary(scenario_a, "Scenario A (Total Control)")
a1_summary <- calculate_summary(scenario_a1, "Scenario A1 (Total Control & MF)")
b_summary <- calculate_summary(scenario_b, "Scenario B (GLP1)")
b1_summary <- calculate_summary(scenario_b1, "Scenario B1 (GLP1 & MF)")

# Print comparison
cat("1. Total Complications (2024-2061)\n")
cat(sprintf("   Base case:      %12.0f complications\n", sq_summary$total_complications))
cat(sprintf("   Scenario A:     %12.0f complications (%.1f%% vs base)\n",
            a_summary$total_complications,
            100 * (a_summary$total_complications - sq_summary$total_complications) / sq_summary$total_complications))
cat(sprintf("   Scenario A1:    %12.0f complications (%.1f%% vs base)\n",
            a1_summary$total_complications,
            100 * (a1_summary$total_complications - sq_summary$total_complications) / sq_summary$total_complications))
cat(sprintf("   Scenario B:     %12.0f complications (%.1f%% vs base)\n",
            b_summary$total_complications,
            100 * (b_summary$total_complications - sq_summary$total_complications) / sq_summary$total_complications))
cat(sprintf("   Scenario B1:    %12.0f complications (%.1f%% vs base)\n\n",
            b1_summary$total_complications,
            100 * (b1_summary$total_complications - sq_summary$total_complications) / sq_summary$total_complications))


cat("   Complication breakdown (Scenario A vs Base):\n")
cat(sprintf("   - Macrovascular: %10.0f → %10.0f (%.1f%% change)\n",
            sq_summary$macro_complications, a_summary$macro_complications,
            100 * (a_summary$macro_complications - sq_summary$macro_complications) / sq_summary$macro_complications))
cat(sprintf("   - Microvascular: %10.0f → %10.0f (%.1f%% change)\n",
            sq_summary$micro_complications, a_summary$micro_complications,
            100 * (a_summary$micro_complications - sq_summary$micro_complications) / sq_summary$micro_complications))
cat(sprintf("   - Other:         %10.0f → %10.0f (%.1f%% change)\n",
            sq_summary$other_complications, a_summary$other_complications,
            100 * (a_summary$other_complications - sq_summary$other_complications) / sq_summary$other_complications))
cat("\n")

cat("2. Population Distribution (Final Year, 2061)\n")
cat(sprintf("   Base case:      %.0f total population\n", sq_summary$final_population))
cat(sprintf("     - No diabetes: %.0f (%.1f%%)\n", 
            sq_summary$final_no_diabetes, 
            100 * sq_summary$final_no_diabetes / sq_summary$final_population))
cat(sprintf("     - Uncontrolled T2D: %.0f (%.1f%%)\n", 
            sq_summary$final_uncontrolled,
            100 * sq_summary$final_uncontrolled / sq_summary$final_population))
cat(sprintf("     - Controlled T2D: %.0f (%.1f%%)\n\n", 
            sq_summary$final_controlled,
            100 * sq_summary$final_controlled / sq_summary$final_population))

cat("3. Total Deaths (2024-2061)\n")
cat(sprintf("   Base case:      %10.0f deaths\n", sq_summary$total_deaths))
cat(sprintf("   Scenario A:     %10.0f deaths (%.1f%% vs base)\n",
            a_summary$total_deaths,
            100 * (a_summary$total_deaths - sq_summary$total_deaths) / sq_summary$total_deaths))
cat(sprintf("   Scenario A1:    %10.0f deaths (%.1f%% vs base)\n",
            a1_summary$total_deaths,
            100 * (a1_summary$total_deaths - sq_summary$total_deaths) / sq_summary$total_deaths))
cat(sprintf("   Scenario B:     %10.0f deaths (%.1f%% vs base)\n",
            b_summary$total_deaths,
            100 * (b_summary$total_deaths - sq_summary$total_deaths) / sq_summary$total_deaths))
cat(sprintf("   Scenario B1:    %10.0f deaths (%.1f%% vs base)\n\n",
            b1_summary$total_deaths,
            100 * (b1_summary$total_deaths - sq_summary$total_deaths) / sq_summary$total_deaths))


if (!is.na(sq_summary$direct_costs)) {
  cat("4. Health Economics (Scenario A vs Base)\n")
  cat(sprintf("   Direct Healthcare Costs:\n"))
  cat(sprintf("     Base case:      £%12.0f million\n", sq_summary$direct_costs / 1e6))
  cat(sprintf("     Scenario A:     £%12.0f million (%.1f%% vs base)\n",
              a_summary$direct_costs / 1e6,
              100 * (a_summary$direct_costs - sq_summary$direct_costs) / sq_summary$direct_costs))
  cat("\n")
  
  cat(sprintf("   Quality-Adjusted Life Years Lost:\n"))
  cat(sprintf("     Base case:      %12.0f QALYs lost\n", sq_summary$qaly_loss))
  cat(sprintf("     Scenario A:     %12.0f QALYs lost\n", a_summary$qaly_loss))
  cat(sprintf("     QALYs Gained:   %12.0f (%.1f%% improvement)\n",
              sq_summary$qaly_loss - a_summary$qaly_loss,
              100 * (sq_summary$qaly_loss - a_summary$qaly_loss) / sq_summary$qaly_loss))
  cat("\n")
}

## 3. Create Visualizations ####################################################

cat("Creating visualizations...\n")

# Prepare data for plotting - Population by diabetes status
sq_long <- status_quo %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  rename(
    "No Diabetes" = f_D0_aX,
    "Uncontrolled T2D" = f_D1_aX,
    "Controlled T2D" = f_D2_aX
  ) %>%
  pivot_longer(cols = -time, names_to = "Status", values_to = "Population") %>%
  mutate(Scenario = "Base case")

a_long <- scenario_a %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  rename(
    "No Diabetes" = f_D0_aX,
    "Uncontrolled T2D" = f_D1_aX,
    "Controlled T2D" = f_D2_aX
  ) %>%
  pivot_longer(cols = -time, names_to = "Status", values_to = "Population") %>%
  mutate(Scenario = "Scenario A")

a1_long <- scenario_a1 %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  rename(
    "No Diabetes" = f_D0_aX,
    "Uncontrolled T2D" = f_D1_aX,
    "Controlled T2D" = f_D2_aX
  ) %>%
  pivot_longer(cols = -time, names_to = "Status", values_to = "Population") %>%
  mutate(Scenario = "Scenario A1")

b_long <- scenario_b %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  rename(
    "No Diabetes" = f_D0_aX,
    "Uncontrolled T2D" = f_D1_aX,
    "Controlled T2D" = f_D2_aX
  ) %>%
  pivot_longer(cols = -time, names_to = "Status", values_to = "Population") %>%
  mutate(Scenario = "Scenario B")

b1_long <- scenario_b1 %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  rename(
    "No Diabetes" = f_D0_aX,
    "Uncontrolled T2D" = f_D1_aX,
    "Controlled T2D" = f_D2_aX
  ) %>%
  pivot_longer(cols = -time, names_to = "Status", values_to = "Population") %>%
  mutate(Scenario = "Scenario B1")

combined <- rbind(sq_long, a_long, a1_long, b_long, b1_long)

# Plot 1: Population by diabetes status over time (faceted by scenario)
p1 <- ggplot(combined, aes(x = time, y = Population / 1e6, color = Status)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Scenario, ncol = 2) +
  labs(
    title = "Population by Diabetes Status Over Time - All Scenarios",
    x = "Year",
    y = "Population (millions)",
    color = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("outputs/plot_population_by_status.png", p1, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/plot_population_by_status.png\n")

# Plot 2: Annual complications over time - comparing all scenarios
sq_comp <- status_quo %>%
  head(-1) %>%  # Remove last row (NAs)
  select(time, fC_Ma, fC_Mi, fC_o) %>%
  rename(
    "Macrovascular" = fC_Ma,
    "Microvascular" = fC_Mi,
    "Other" = fC_o
  ) %>%
  pivot_longer(cols = -time, names_to = "Type", values_to = "Complications") %>%
  mutate(Scenario = "Base case")

a_comp <- scenario_a %>%
  head(-1) %>%
  select(time, fC_Ma, fC_Mi, fC_o) %>%
  rename(
    "Macrovascular" = fC_Ma,
    "Microvascular" = fC_Mi,
    "Other" = fC_o
  ) %>%
  pivot_longer(cols = -time, names_to = "Type", values_to = "Complications") %>%
  mutate(Scenario = "Scenario A")

a1_comp <- scenario_a1 %>%
  head(-1) %>%
  select(time, fC_Ma, fC_Mi, fC_o) %>%
  rename(
    "Macrovascular" = fC_Ma,
    "Microvascular" = fC_Mi,
    "Other" = fC_o
  ) %>%
  pivot_longer(cols = -time, names_to = "Type", values_to = "Complications") %>%
  mutate(Scenario = "Scenario A1")

b_comp <- scenario_b %>%
  head(-1) %>%
  select(time, fC_Ma, fC_Mi, fC_o) %>%
  rename(
    "Macrovascular" = fC_Ma,
    "Microvascular" = fC_Mi,
    "Other" = fC_o
  ) %>%
  pivot_longer(cols = -time, names_to = "Type", values_to = "Complications") %>%
  mutate(Scenario = "Scenario B")

b1_comp <- scenario_b1 %>%
  head(-1) %>%
  select(time, fC_Ma, fC_Mi, fC_o) %>%
  rename(
    "Macrovascular" = fC_Ma,
    "Microvascular" = fC_Mi,
    "Other" = fC_o
  ) %>%
  pivot_longer(cols = -time, names_to = "Type", values_to = "Complications") %>%
  mutate(Scenario = "Scenario B1")

comp_combined <- rbind(sq_comp, a_comp, a1_comp, b_comp, b1_comp)


p2 <- ggplot(comp_combined, aes(x = time, y = Complications, color = Type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Scenario, ncol = 2) +
  labs(
    title = "Annual Complications by Type - All Scenarios",
    x = "Year",
    y = "Number of Complications per Year",
    color = "Complication Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("outputs/plot_complications_by_type.png", p2, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/plot_complications_by_type.png\n")

# Plot 3: Cumulative complications comparison (all scenarios)
sq_cumulative <- status_quo %>%
  head(-1) %>%
  mutate(
    CumulativeMacro = cumsum(fC_Ma),
    CumulativeMicro = cumsum(fC_Mi),
    CumulativeOther = cumsum(fC_o)
  ) %>%
  select(time, CumulativeMacro, CumulativeMicro, CumulativeOther) %>%
  mutate(Scenario = "Base case")

a_cumulative <- scenario_a %>%
  head(-1) %>%
  mutate(
    CumulativeMacro = cumsum(fC_Ma),
    CumulativeMicro = cumsum(fC_Mi),
    CumulativeOther = cumsum(fC_o)
  ) %>%
  select(time, CumulativeMacro, CumulativeMicro, CumulativeOther) %>%
  mutate(Scenario = "Scenario A")

a1_cumulative <- scenario_a1 %>%
  head(-1) %>%
  mutate(
    CumulativeMacro = cumsum(fC_Ma),
    CumulativeMicro = cumsum(fC_Mi),
    CumulativeOther = cumsum(fC_o)
  ) %>%
  select(time, CumulativeMacro, CumulativeMicro, CumulativeOther) %>%
  mutate(Scenario = "Scenario A1")

b_cumulative <- scenario_b %>%
  head(-1) %>%
  mutate(
    CumulativeMacro = cumsum(fC_Ma),
    CumulativeMicro = cumsum(fC_Mi),
    CumulativeOther = cumsum(fC_o)
  ) %>%
  select(time, CumulativeMacro, CumulativeMicro, CumulativeOther) %>%
  mutate(Scenario = "Scenario B")

b1_cumulative <- scenario_b1 %>%
  head(-1) %>%
  mutate(
    CumulativeMacro = cumsum(fC_Ma),
    CumulativeMicro = cumsum(fC_Mi),
    CumulativeOther = cumsum(fC_o)
  ) %>%
  select(time, CumulativeMacro, CumulativeMicro, CumulativeOther) %>%
  mutate(Scenario = "Scenario B1")

cumulative_combined <- rbind(sq_cumulative, a_cumulative, a1_cumulative, b_cumulative, b1_cumulative) %>%
  pivot_longer(cols = c(CumulativeMacro, CumulativeMicro, CumulativeOther),
               names_to = "Type", values_to = "Cumulative") %>%
  mutate(Type = gsub("Cumulative", "", Type))


p3 <- ggplot(cumulative_combined, aes(x = time, y = Cumulative / 1e3, 
                                      color = Type, linetype = Scenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Cumulative Complications Over Time - All Scenarios",
    x = "Year",
    y = "Cumulative Complications (thousands)",
    color = "Complication Type",
    linetype = "Scenario"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("outputs/plot_cumulative_complications.png", p3, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/plot_cumulative_complications.png\n")

cat("\n=== Analysis Complete ===\n")
cat("Generated files:\n")
cat("  - outputs/plot_population_by_status.png\n")
cat("  - outputs/plot_complications_by_type.png\n")
cat("  - outputs/plot_cumulative_complications.png\n")
cat("\nThese plots compare all 5 scenarios:\n")
cat("  - Base case (Status Quo)\n")
cat("  - Scenario A (Total Control)\n")
cat("  - Scenario A1 (Total Control & Multifactorial)\n")
cat("  - Scenario B (GLP1)\n")
cat("  - Scenario B1 (GLP1 & Multifactorial)\n")
cat("\nPlots can be included in your manuscript or presentation.\n")
