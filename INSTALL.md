# Quick Start Installation Guide

## For Non-R Users

If you're new to R, follow these steps to get the model running.

### Step 1: Install R

1. Go to [https://cran.r-project.org/](https://cran.r-project.org/)
2. Click on your operating system (Windows, Mac, or Linux)
3. Download and install R
   - **Windows**: Download "base" → "Download R-4.x.x for Windows"
   - **Mac**: Download "R-4.x.x.pkg"
   - **Linux**: Follow instructions for your distribution

### Step 2: Install RStudio (Optional but Recommended)

RStudio is a user-friendly interface for R:

1. Go to [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
2. Download and install RStudio Desktop (free version)

### Step 3: Install Required Packages

**Option A: Using RStudio (Easier)**

1. Open RStudio
2. In the Console panel (usually bottom-left), copy and paste:
   ```r
   install.packages(c("deSolve", "openxlsx", "ggplot2", "dplyr", "tidyr", 
                      "purrr", "scales", "Metrics", "ggpubr", "growthmodels"))
   ```
3. Press Enter
4. Wait for installation to complete (this may take 5-10 minutes)

**Option B: Using R directly**

1. Open R (command line or R GUI)
2. Type the same command as above
3. Press Enter

### Step 4: Download the Model

Download and extract the `T2D_Engine_Minimal` folder to your computer.

### Step 5: Run the Model

**Option A: Using RStudio**

1. Open RStudio
2. Go to File → Open File
3. Navigate to `T2D_Engine_Minimal` folder
4. Open `run_T2D_model.R`
5. Click the "Source" button at the top-right of the editor
6. Wait for the model to run (30 seconds to 2 minutes)

**Option B: Using Command Line**

1. Open Terminal (Mac/Linux) or Command Prompt (Windows)
2. Navigate to the folder:
   ```bash
   cd /path/to/T2D_Engine_Minimal
   ```
3. Run the script:
   ```bash
   Rscript run_T2D_model.R
   ```

### Step 6: View Results

After the model runs, you'll find two CSV files in the `T2D_Engine_Minimal` folder:

- `output_status_quo.csv` - Baseline scenario results
- `output_scenario_total_control.csv` - Intervention scenario results

You can open these in:
- Excel or Google Sheets
- R or RStudio (for analysis)
- Python (using pandas)

## Troubleshooting

### "Package not found" error

Install the missing package individually:
```r
install.packages("package_name")
```

### "Cannot open file" error

Make sure you're in the correct directory:
```r
getwd()  # Check current directory
setwd("/path/to/T2D_Engine_Minimal")  # Set correct directory
```

### Model runs very slowly

This is normal - the model solves 45 differential equations over 37 years. Expected time: 30 seconds to 2 minutes.

## Testing Your Setup

Before running the full model, test your setup:

```bash
Rscript validate_setup.R
```

Or in R:
```r
source("validate_setup.R")
```

This will check that all files and packages are present.

## Need More Help?

See the main `README.md` file for detailed documentation.
