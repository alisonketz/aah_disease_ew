##########################################################################################################
###
### 
### Age at harvest incorporating disease testing model
###
### Alison Ketz
### 02/22/2022
###
##########################################################################################################

rm(list = ls())
setwd("~/Documents/aah/aah_disease_ew")

library(ggplot2)
library(nimble)
library(Matrix)
library(coda)
library(lattice)
library(splines)
library(Hmisc)
library(lubridate)
library(readxl)
library(gridExtra)
library(xtable)
library(beepr)
library(RColorBrewer)
library(reshape2)
library(viridis)
library(ggridges)
library(doParallel)
library(dplyr)
library(tidyr)
library(data.table)
library(abind)
library(sf)
library(MetBrewer)
library(terra)
library(tidyterra)

#############################################
### Source summary function for posteriors
#############################################

source("summarize.R")

####################################
### Load and clean data
####################################

source("02_load_clean_data.R")
source("02_agg_format_data.R")
# source("02_load_data.R")

####################################
### Format data
####################################

# source("03_preliminaries.R")
source("03_preliminaries_fake.R")

##############################################
### Functions to calculate
### probability of survival &
### probability of changing infection status
###############################################

# source("04_functions_SLOW.R")
# source("04_functions_GES.R")
# source("04_functions_FAST.R")

source("04_functions_FASTEST_fixed.R")

###################################
### Run model
###################################

source("05_run_model.R")

###################################
### Post Process summary and plots
###################################

source("06_results_plots_sum.R")
