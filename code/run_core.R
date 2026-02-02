# Minimal script to run core figures
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(forcats)
  library(purrr)
  library(tidyr)
  library(gtools)
  library(cowplot)
  library(stringr)
  library(effects)
})

# Helper functions
source("functions.R")

# Preprocessing
cat("Loading data...\n")
source("preprocess1.R")
cat("Study 1 loaded: n =", nrow(study1), "\n")
source("preprocess2.R")
cat("Study 2 loaded: n =", nrow(study2), "\n")
source("preprocess3.R")
cat("Study 3 loaded: n =", nrow(study3), "\n")
source("preprocess4.R")
cat("Study 4 loaded: n =", nrow(study4), "\n")

# Create results directory
dir.create("../results", showWarnings = FALSE)

# Core figures
cat("\nGenerating Figure 1...\n")
source("figure1.R")

cat("Generating Figure 2...\n")
source("figure2.R")

cat("Generating Figure 3...\n")
source("figure3.R")

cat("Generating Figure 4...\n")
source("figure4.R")

cat("Generating Figure 5...\n")
source("figure5.R")

cat("Generating Figure 6...\n")
source("figure6.R")

cat("\n=== KEY RESULTS ===\n")
cat("\nStudy 1 - Justified (Political treatments):\n")
print(study1 %>%
  filter(alignment != 'Apolitical Driver') %>%
  group_by(passed) %>%
  summarise(prop_justified = round(mean(justified, na.rm=TRUE), 4), n=n()))

cat("\nStudy 2 - Justified (Political treatments):\n")
print(study2 %>%
  filter(alignment != 'Apoltical Shooter') %>%
  group_by(passed) %>%
  summarise(prop_justified = round(mean(justified, na.rm=TRUE), 4), n=n()))

cat("\nPartial ID Bounds...\n")
source("partial_id_bounds.R")

cat("\nDone! Check ../results/ for figures.\n")
