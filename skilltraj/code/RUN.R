# Import Environment

if (!require("renv")) {
  install.packages("renv")
  library(renv)
}
renv::restore()

# Import Library
source("ImportLibrary.R")

# Import Data
source("ImportData.R")

# Target Population
source("TargetPopulation.R")

# Feature Engineering
source("FeatureEngineering.R")

# Descriptive Statistics
source("DescriptiveStatistics.R")

# Model Estimation
source("ModelEstimation.R")

# Robustness Checks
source("RobustnessChecks.R")
