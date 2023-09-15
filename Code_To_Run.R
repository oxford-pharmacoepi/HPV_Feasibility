rm(list=ls())

# Install dependencies -----
# install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()

# Open libraries  -----
library(CDMConnector)
library(DBI)
library(dbplyr)
library(dplyr)
library(PatientProfiles)
library(here)
library(tictoc)
library(CDMConnector)

# Options and set-up:  directories and settings  -----
database_name <- "....."            ## Database name
cohort_json_dir <- here("Cohorts/") ## JSON cohorts folder

# Database connection details -----
db <- dbConnect(".....")
cdm <- CDMConnector::cdm_from_con(".....")

# Run the study ------
source(here("Main_Code.R"))