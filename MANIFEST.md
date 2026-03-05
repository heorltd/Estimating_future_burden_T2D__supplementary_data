# T2D Engine Minimal Implementation - File Manifest

This document lists all files in the minimal implementation and their purpose.

## Core Files

### Main Scripts

1. **run_T2D_model.R**
   - Main executable script
   - Runs the T2D TSV model with two scenarios
   - Generates output CSV files
   - Purpose: Primary entry point for running the model

2. **example_analysis.R**
   - Example analysis and visualization script
   - Compares scenarios and generates plots
   - Purpose: Demonstrates how to analyze model output

3. **validate_setup.R**
   - Setup validation script
   - Checks for required files and packages
   - Purpose: Verify installation before running the model

### Documentation

4. **README.md**
   - Comprehensive documentation (15KB)
   - Installation, usage, and technical details
   - Purpose: Complete user guide

5. **INSTALL.md**
   - Quick start guide for non-R users (3KB)
   - Step-by-step installation instructions
   - Purpose: Simplified setup guide

6. **MANIFEST.md** (this file)
   - File listing and descriptions
   - Purpose: Document repository contents

7. **.gitignore**
   - Git ignore file
   - Excludes output files, plots, and temporary files
   - Purpose: Keep repository clean

## Source Code (source/)

Core R functions that implement the model:

8. **source/library_loader.R**
   - Loads required R packages
   - Lists all dependencies

9. **source/preamble.R**
   - Setup and initialization
   - Sets random seed

10. **source/load_precalibrated_initial_population.R**
    - Loads initial population state for 2024
    - Returns compartment occupancies

11. **source/load_params.R**
    - Loads model parameters (static and calibrated)
    - Handles multifactorial parameter set

12. **source/T2D_SDM.R**
    - Systems Dynamics Model (290 lines)
    - Implements ODE system
    - Core mathematical model

13. **source/T2D_PHA.R**
    - Post-Hoc Analysis (1283 lines)
    - Calculates health and economic outcomes
    - Processes model output

14. **source/post_hoc_functions.R**
    - Additional analysis functions
    - Helper functions for outcome calculation

15. **source/post_processing_functions.R**
    - Data processing utilities
    - Formatting and transformation functions

## Input Data (inputs/)

Pre-calibrated parameters and epidemiological data:

### Population and Parameters

16. **inputs/precalibrated_population_2016+.csv**
    - Initial population by compartment (2024)
    - Used by default

17. **inputs/precalibrated_population.csv**
    - Alternative initial population (archived)
    - Not used in current version

18. **inputs/precalibrated_params_2016+.csv**
    - Calibrated transition rates
    - Used by default

19. **inputs/precalibrated_params.csv**
    - Alternative calibrated parameters (archived)
    - Not used in current version

20. **inputs/static_params.csv**
    - Fixed model parameters
    - Default parameter set

21. **inputs/static_params_multifactorial.csv**
    - Parameters for multifactorial interventions
    - Used when multifactorial = TRUE

22. **inputs/net_migration_age_breakdown.csv**
    - Age distribution of net migration
    - Used for demographic changes

### Complication Data

23. **inputs/complication_hosp_rates.csv**
    - Hospitalization rates by complication type

24. **inputs/complication_hosp_LOS.csv**
    - Hospital length of stay (LOS) by complication

25. **inputs/complication_direct_costs.csv**
    - Direct healthcare costs per complication

26. **inputs/complication_direct_costs_HEX.csv**
    - Alternative cost estimates (HEX method)

27. **inputs/complication_indirect_costs.csv**
    - Indirect costs (productivity loss) per complication

28. **inputs/complication_disutilities.csv**
    - Quality of life loss (disutility) per complication

### Treatment and Diagnosis Costs

29. **inputs/diagnosis_direct_costs.csv**
    - Costs of T2D diagnosis

30. **inputs/routine_tx_costs.csv**
    - Routine treatment costs by diabetes status

31. **inputs/glucose_monitoring_costs.csv**
    - Costs of glucose monitoring

32. **inputs/routine_appointments.csv**
    - Frequency and costs of routine medical appointments

### Health Outcomes

33. **inputs/life_years_lost.csv**
    - Life years lost (LYL) by complication

34. **inputs/deaths_utility_loss.csv**
    - Quality of life loss from premature death

35. **inputs/dynamic_mort_exp_age_grouped.csv**
    - Dynamic mortality rates (exponential model)

36. **inputs/dynamic_mort_linear_age_grouped.csv**
    - Dynamic mortality rates (linear model)

### Environmental Impact

37. **inputs/env_impacts.csv**
    - Environmental impacts per complication
    - Carbon footprint data

## Output Files (Generated)

These files are created when you run the model:

38. **output_status_quo.csv** (generated)
    - Results for baseline scenario
    - ~103 KB, 38 years × ~100 columns

39. **output_scenario_total_control.csv** (generated)
    - Results for intervention scenario
    - ~103 KB, 38 years × ~100 columns

40. **plot_population_by_status.png** (generated by example_analysis.R)
    - Visualization of population trajectories
    - 10" × 6" at 300 DPI

41. **plot_complications_by_type.png** (generated by example_analysis.R)
    - Annual complications over time
    - 10" × 6" at 300 DPI

42. **plot_cumulative_complications.png** (generated by example_analysis.R)
    - Cumulative complications comparison
    - 10" × 6" at 300 DPI

## File Size Summary

- **Total repository size**: ~150 KB (excluding outputs)
- **Input data**: ~110 KB
- **Source code**: ~30 KB
- **Documentation**: ~20 KB
- **Generated outputs**: ~310 KB (CSV + plots)

## Dependencies

Required R packages (automatically loaded):
- deSolve - ODE solver
- openxlsx - Excel output
- ggplot2 - Plotting
- dplyr - Data manipulation
- tidyr - Data tidying
- purrr - Functional programming
- scales - Scaling functions
- Metrics - Model metrics
- ggpubr - Publication plots
- growthmodels - Gompertz models

## Version Information

- **Version**: 1.0
- **Date**: 2026-03-03
- **R Version Required**: 4.0 or higher
- **Platform**: Cross-platform (Windows, Mac, Linux)

## Usage Flow

1. Validate setup: `Rscript validate_setup.R`
2. Run model: `Rscript run_T2D_model.R`
3. Analyze results: `Rscript example_analysis.R`
4. Review outputs: Open CSV files and PNG plots

## Notes

- All file paths are relative to the T2D_Engine_Minimal/ directory
- Input files use comma-separated values (CSV) format
- Output files are CSV format, readable in any spreadsheet software
- Plots are PNG format, 300 DPI for publication quality
- The model is deterministic (no randomness beyond fixed seed)
