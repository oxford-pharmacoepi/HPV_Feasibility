 rm(list=ls())

## Install Needed Packages
# install.packages("CDMConnector")
# install.packages("DBI")
# install.packages("dbplyr")
# install.packages("dplyr")
# install.packages("PatientProfiles")
# install.packages("here")
# install.packages("tictoc")

### Open libraries
library(CDMConnector)
library(DBI)
library(dbplyr)
library(dplyr)
library(PatientProfiles)
library(here)
library(tictoc)

tic.clearlog()
tic.clear()
tic(msg = "total time run: ")

##### Connect to database using CDM COnnector ########
tic(msg = "Connect to database")

# server_dbi <- Sys.getenv("DB_SERVER_DBI_Pharmetrics") 
server_dbi <- Sys.getenv("DB_SERVER_DBI_CPRDgold") 
user <- Sys.getenv("DB_USER") 
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(), 
                dbname = server_dbi, 
                port = port, 
                host = host, 
                user = user, 
                password = Sys.getenv("DB_PASSWORD") ) 

cdm <- cdm_from_con(con = db,
                               cdm_schema = "public",
                               write_schema = "results")


toc(log = TRUE)



##### Options and set-up:  directories and settings ######
tic(msg = "Settings ")

cohort_json_dir <- here("Cohorts/")
cohorts_name <- "feasibility_hpv"
vaccine_codes <- read.csv(here("HPV_vac.csv")) %>% select(concept.CONCEPT_ID)
database_name <- "CPRD_Gold"
toc(log = TRUE)








############### Feasibility study starts here ######################


####### Step 1: Get cohorts and generate them #######

tic(msg = "Generate Cohort Set")

cohort_set <- read_cohort_set(cohort_json_dir)
cdm <-   generateCohortSet(cdm, 
                           cohort_set,
                           name = cohorts_name,
                           computeAttrition = TRUE,
                           overwrite = TRUE)

toc(log = TRUE)

   
######### 4 - Cohort Counts #########
# Subjects and records can be retrieved from the cohort_count 
   
tic(msg = "Cohort counts, attrition, subset cdm")
   
cohort_count <- cohort_count(cdm[[cohorts_name]])
#cohort_attrition <- cohort_attrition(cdm[[cohorts_name]])
cohort_set_cdm <- cohort_set(cdm[[cohorts_name]])

cohort_count_named <- cohort_count %>% left_join(cohort_set_cdm) %>% 
  mutate(number_subjects = case_when(as.integer(number_subjects) < 6 ~ 0,
                       TRUE ~ as.integer(number_subjects) ))

toc(log = TRUE)


######### 6 - Age #########
# Age distribution of cohorts

tic(msg = "Age distribution")
cdm$results_dx <- cdm[[cohorts_name]]

Age_profile <- cdm$results_dx %>%
   addDemographics(cdm, age=T, sex= T, ageGroup = list(
     "age_group" =
       list(
         "0 to 11" = c(0, 11),
         "12 to 18" = c(12, 18),
         "19 to 25" = c(19, 25),
         ">150" = c(26, 150)
       ))) %>% group_by(cohort_definition_id, age_group, sex) %>% tally() %>%  collect()  %>% 
  mutate(n = case_when(as.integer(n) < 6 ~ 0,
                       TRUE ~ as.integer(n) ))

toc(log = TRUE)


########  7 - Index Event Breakdown ##############
# Only missing thing !
#  Add source field and Standard fields subjects and records
# TEST: ONLY FOR CONDITIONS
tic(msg = "Vaccine Event Breakdown: ")

Index_events_drugs <- cdm[[cohorts_name]] %>%
                        left_join(
                          cdm$drug_exposure,
                          by=join_by(subject_id==person_id, cohort_start_date==drug_exposure_start_date)) %>%
                        group_by( drug_concept_id, drug_source_concept_id, drug_source_value) %>%
                        tally()  %>% 
                        left_join(cdm$concept %>% select(concept_id , concept_name), by=join_by(drug_concept_id==concept_id))  %>% 
                        rename( standard_concept=concept_name)  %>% 
                        left_join(cdm$concept %>% select(concept_id , concept_name), by=join_by(drug_source_concept_id==concept_id)) %>% 
                        rename( source_concept=concept_name)  %>%
                        collect() %>% filter(drug_concept_id %in% vaccine_codes$concept.CONCEPT_ID) %>% 
                        mutate(n = case_when(as.integer(n) < 6 ~ 0,
                                             TRUE ~ as.integer(n) ))



toc(log = TRUE)

############# Logs ############

toc(log = TRUE)
tic.log(format = TRUE)
tic_log <- tic.log(format = TRUE)



############# save.image ############
rm(cdm, 
   db, vaccine_codes, codes,
   json2, cohortExpresion, original_codes_counts,
   recommended_codes, recommended_codes_counts,
   cohort_count, cohort_attrition, cohort_set)
rm( cohort_json_dir, host, port, server_dbi, user)
 rm(list = ls.str(mode = 'numeric'))



save.image(file = here(paste0(cohorts_name, database_name, format(Sys.time(), "_%Y_%m_%d") , ".RData")))


