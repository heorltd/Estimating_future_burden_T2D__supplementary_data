#!/usr/bin/env Rscript

#' Quick validation script to check if the model setup is correct
#' This doesn't run the full model, just checks file structure and dependencies

cat("=== T2D Model Setup Validation ===\n\n")

# Check working directory
cat("1. Checking working directory...\n")
if (file.exists("run_T2D_model.R")) {
  cat("   ✓ Working directory is correct\n")
} else {
  cat("   ✗ ERROR: Cannot find run_T2D_model.R\n")
  cat("   Please set working directory to T2D_Engine_Minimal/\n")
  quit(status = 1)
}

# Check directory structure
cat("\n2. Checking directory structure...\n")
required_dirs <- c("inputs", "source")
for (dir in required_dirs) {
  if (dir.exists(dir)) {
    cat(sprintf("   ✓ %s/ directory exists\n", dir))
  } else {
    cat(sprintf("   ✗ ERROR: Missing %s/ directory\n", dir))
  }
}

# Check input files
cat("\n3. Checking required input files...\n")
required_inputs <- c(
  "precalibrated_population_2016+.csv",
  "precalibrated_params_2016+.csv",
  "static_params.csv",
  "net_migration_age_breakdown.csv"
)

for (file in required_inputs) {
  filepath <- file.path("inputs", file)
  if (file.exists(filepath)) {
    cat(sprintf("   ✓ %s\n", file))
  } else {
    cat(sprintf("   ✗ ERROR: Missing %s\n", file))
  }
}

# Check source files
cat("\n4. Checking required source files...\n")
required_sources <- c(
  "library_loader.R",
  "load_precalibrated_initial_population.R",
  "load_params.R",
  "T2D_SDM.R",
  "T2D_PHA.R"
)

for (file in required_sources) {
  filepath <- file.path("source", file)
  if (file.exists(filepath)) {
    cat(sprintf("   ✓ %s\n", file))
  } else {
    cat(sprintf("   ✗ ERROR: Missing %s\n", file))
  }
}

# Check for R packages
cat("\n5. Checking R packages...\n")
required_packages <- c("deSolve", "openxlsx", "ggplot2", "dplyr", 
                       "tidyr", "purrr", "scales", "Metrics", 
                       "ggpubr", "growthmodels")

installed_pkgs <- installed.packages()[, "Package"]
missing_pkgs <- required_packages[!required_packages %in% installed_pkgs]

if (length(missing_pkgs) == 0) {
  cat("   ✓ All required packages are installed\n")
} else {
  cat("   ⚠ Missing packages:\n")
  for (pkg in missing_pkgs) {
    cat(sprintf("     - %s\n", pkg))
  }
  cat("\n   To install missing packages, run:\n")
  cat(sprintf("   install.packages(c(%s))\n", 
              paste(sprintf('"%s"', missing_pkgs), collapse = ", ")))
}

# Summary
cat("\n=== Validation Summary ===\n")
cat("If all checks passed (✓), you can run the model with:\n")
cat("  Rscript run_T2D_model.R\n")
cat("\nOr in R console:\n")
cat("  source('run_T2D_model.R')\n")
