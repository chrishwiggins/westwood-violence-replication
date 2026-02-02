
# Load tidyverse components individually (tidyverse meta-package install issues)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(forcats)
library(purrr)
library(psy)
library(gtools)
library(cowplot)
library(stargazer)
library(stringr)
library(questionr)
library(estimatr)
library(vtable)
library(modelsummary)
library(texreg)
library(effects)
#library(GDAtools)
# load helper functions
source("functions.R")

# load and recode data
source("preprocess1.R")
source("preprocess2.R")
source("preprocess3.R")
source("preprocess4.R")
source("preprocess5.R")

#####################################################################
#####################################################################
# Analysis
#
# Note: each of the files below contain the code to replicate the 
# associated table/figure.
#####################################################################
#####################################################################

##### Table and Figures from the main manuscript

# Figure 1
source("figure1.R")

# Figure 2
source("figure2.R")

# Study Summary statistics
source("study_results.R")

# Table 1
source("table1.R")

# Figure 2
source("figure3.R")

# Figure 4
source("figure4.R")

# Figure 5
source("figure5.R")

# Figure 6
source("figure6.R")

# Figure 7
source("figure7.R")

###### Additional results referenced in the main manuscript

# Partial identification bounds
source("partial_id_bounds.R")

# news analysis
source("appendix_news.R")

#incentive study results
source("appendix_incentive.R")

# Pardon result, Study 4
source("appendix_pardon.R")

# test of engagement score by demographics
source("appendix_passedengagementbydemographics.R")

# Correlate tables

source("appendix_correlates.R")

# Cheerleading and trolling
source("appendix_cheerleadingtrolling.R")

# Study 5
source("appendix_study5.R")

######################
# Sample descriptives, covariates analysis and other models
######################
source("appendix.R")
