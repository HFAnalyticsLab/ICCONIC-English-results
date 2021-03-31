#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Read in raw CPRD and linked data sets, combine files and save as RDS files
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'vroom', 'here') #package list
sapply(pkgs, require, character.only = TRUE) #load them up

#NB: all scripts are intended to be run from within an RStudio project at the root folder so {here} works
here() #check here sees project root folder

source(here('R', 'file_paths.R')) #source folder paths: rawCPRDpath, rawHESpath, rawLookupsPath
cohortIDcodes <- here('Reference_data', 'CPRD_cohort_definitions') #set path for cohort definition files (ICD/Read code lists)

#___________________________________________________

#Function for reading in and saving data to RDS ----
#NB: had to temporarily change delim to read csv for the 'hesop_clin' dataset run of the saveData function

saveData <- function(rawfolders, type, colspec = NULL){
  
  filepaths <- unlist(map(rawfolders, ~ list.files(., pattern = type, full.names = TRUE))) #list file paths
  print(filepaths) #print the filepaths of the files that will be read
  dat <- filepaths %>% map_dfr(vroom, delim = '\t', col_types = colspec, locale = locale(date_format = '%d/%m/%Y')) #read in files with column types specified
  print(paste('Rows: ', nrow(dat))) #print out the number of rows
  print('Vars (spec):') #print out variables names
  print(paste0(names(dat), ' (', sapply(dat, class), ')')) #print out variable specs in the dataset
  saveRDS(dat, here('../Data', paste0(tolower(type), '.rds'))) #save to RDS file '[type].rds' in Data folder
  #NB: due to our use of GitLab, we keep our Data folder one level up from our individual copies of the local git repository																				
}

#________________________________________

#Read in and save various data types ----

#Cohort identification
saveData(rawfolders = cohortIDcodes, type = 'ICD-10 codes for Hip', colspec = 'c') #hip fracture ICD codes
saveData(rawfolders = cohortIDcodes, type = 'ICD-10 codes for Heart', colspec = 'c') #heart failure ICD codes
saveData(rawfolders = cohortIDcodes, type = 'ICD-10 codes for T2 Diabetes', colspec = 'c') #diabetes ICD codes
saveData(rawfolders = cohortIDcodes, type = 'ICD-10 codes for COPD', colspec = 'c') #COPD ICD codes
saveData(rawfolders = cohortIDcodes, type = 'Dementia_Therapy', colspec = 'iiccccccccc') #dementia gemscript codes for therapies
saveData(rawfolders = cohortIDcodes, type = 'Dementia_Diagnosis', colspec = 'iiiiiccc') #dementia medcodes codes for diagnoses

#CPRD GOLD data
#NB: we had a problem with downloading data from CPRD, and so we ended up downloading twice to get all the datasets.
#    Most we only had one version of, but Staff and Practice we had two copies of.
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Patient', colspec = 'iniiiiiiDiiDDiiiDiDi') #patient data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Clinical', colspec = 'iDDiiiiiii') #clinical data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Therapy', colspec = 'iDDiiicininii') #therapy data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Test', colspec = 'iDDiiiiinnnnnnnn') #test data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Referral', colspec = 'iDDiiiiiiiiii') #referral data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Consultation', colspec = 'iDDiiin') #consultation data

#THESE BECOME DUPLICATED!!! - because not patient-based, are in both source folders.
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Staff', colspec = 'iii') #staff data
saveData(rawfolders = c(rawCPRDpath1, rawCPRDpath2), type = 'Practice', colspec = 'iiDD') #practice data

#Linked data (HES/ONS)
saveData(rawfolders = rawHESpath, type = 'linkage_elig', colspec = 'iiDiiiii') #HES linkage eligibility
saveData(rawfolders = rawHESpath, type = 'death', colspec = 'iiiiiDDciccccccccccccccccccccccccc') #ONS death registrations
saveData(rawfolders = rawHESpath, type = 'imd', colspec = 'iii') #Index of multiple deprivation
saveData(rawfolders = rawHESpath, type = 'hes_epis', colspec = 'iinDDDDiiiciiiccciicc') #HES APC episodes data
saveData(rawfolders = rawHESpath, type = 'hes_ccare', colspec = 'iin-----D---D-----------------------') #HES APC critical care data
saveData(rawfolders = rawHESpath, type = 'hes_proc', colspec = 'iinDDDDcDi') #HES APC procedures data
saveData(rawfolders = rawHESpath, type = 'hes_diagnosis_epi', colspec = 'iinDDcci') #HES APC episode diagnosis data
saveData(rawfolders = rawHESpath, type = 'hesae_att', colspec = 'inDiiiiiiiiiiic') #HES A&E attendances data
saveData(rawfolders = rawHESpath, type = 'hesae_inv', colspec = 'incci') #HES A&E attendances data
saveData(rawfolders = rawHESpath, type = 'hesae_treat', colspec = 'inccci') #HES A&E attendances data
saveData(rawfolders = rawHESpath, type = 'hesop_app', colspec = 'inciDiiiDciiiDiiiii') #HES outpatient appointment data
#NB: there seemed to be a problem with 'hesop_clin' (perhaps a problem with the end of file marker?)
#So we read it first with Stata and saved it to csv in the project Data folder (one folder up from the user's GitLab copy of the project)
#Also, had to edit the saveData function to delim = ',' (rather than '\t') to read it in
saveData(rawfolders = here('../Data'), type = 'hesop_clin_WP', colspec = paste0(c('in', rep('c', 36), 'ccci'), collapse = '')) #HES outpatient clinical data

#Lookup/reference files
saveData(rawfolders = paste0(rawLookupsPath), type = 'product', colspec = 'icccccccc') #product lookup data from CPRD
saveData(rawfolders = paste0(rawLookupsPath, '\\TXTFILES'), type = 'COT', colspec = 'ic') #consultation type lookup data from CPRD
saveData(rawfolders = paste0(rawLookupsPath, '\\TXTFILES'), type = 'ROL', colspec = 'ic') #staff role lookup data from CPRD





















