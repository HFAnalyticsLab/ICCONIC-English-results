#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Identify cohorts, join linked demographic data and linkage indicators
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'here') #package list
sapply(pkgs, require, character.only = TRUE) #load them up

#______________________________________________________

#Read in all the cohort basic information datasets ----

patient <- readRDS(here('../Data', 'patient.rds')) %>% select(patid, gender, yob, crd, tod, accept) #read in and select cols
linkage <- readRDS(here('../Data', 'linkage_elig.rds')) %>% select(-linkdate) #read in and select cols
death <- readRDS(here('../Data', 'death.rds')) %>% select(patid, dod) #read in and select cols
imd <- readRDS(here('../Data', 'imd.rds')) %>% select(patid, imd_dec = imd2015_10) #read in and select cols
practice <- readRDS(here('../Data', 'practice.rds')) %>% group_by_all() %>% tally() %>% select(-n) #read in, summarise and select cols
#practice is summarised because the data was duplicated in the original data import (due to two sets of data being sourced from CPRD)
cohort <- list(patient, linkage, practice, death, imd) %>% Reduce(full_join, .) #list the datasets and join together to form basic initial cohort data
agegroups <- read_csv(here('Reference_data', 'agegroups.csv'), col_types = 'icc') #read in lookup table for age groupings (manually created)

#__________________________________________________________________

#Create hip and heart cohort identifiers from ICD10 code lists ----

#Prepare constants and datasets

#Three year eligibilty period: 2014/15 to 2016/17
elig_start <- as.Date('2014-04-01') #start date of study eligibility period
elig_end <- as.Date('2017-03-31') #end date of study eligibility period

#Prepare and bind ICD codelists
ICDdefs <- c('ICD-10 codes for Hip.rds', 'ICD-10 codes for Heart.rds', 
             'ICD-10 codes for T2 Diabetes.rds', 'ICD-10 codes for COPD.rds') %>%
  map(~ readRDS(here('../Data', .))) %>% #read in the codelists
  list(y = c('hip', 'heart', 'diab', 'copd'), x = .) %>%  #store names for the lists
  pmap(function(x, y) mutate(x, flag = y)) %>%  #add the name as a variable to each list
  bind_rows() #bind the lists together

#Read in hip operation codes
hip_op_codes <- read_csv(here('Reference_data', 'hip_ops.csv'), col_types = 'ccc')

#Read in APC data and ensure spells are for eligibility period
APCcipsIDs <- readRDS(here('../Data','APCcipsIDs.rds')) #read in IDs linking spells to CIPS
APCprocs <- readRDS(here('../Data', 'APCprocs.rds')) #read in checked inpatient procedures
APCcips <- readRDS(here('../Data','APCcips.rds')) %>% #read in checked HES spell-identifying index episodes
  filter(cipstart >= elig_start & cipstart <= elig_end) #filter for eligibility period
APCdiags <- readRDS(here('../Data', 'APCdiags.rds')) %>% #read in checked inpatient diagnoses
  inner_join(APCcipsIDs) #add cips identifier



#Identify types of hip operation in APCprocs data

#List and categorise all procedures
all_ops <-  APCprocs %>% #read in checked inpatient procedures
  select(patid, spno, epikey, OPCS, epistart, evdate) %>% #select columns
  inner_join(APCcipsIDs) %>% #join on cips identifier using inner join
  left_join(hip_op_codes) %>% #join on hip operation codes, keeping all procedures using left join
  select(-Description) %>% #remove column containing long text description of hip operation codes
  mutate(cat_ops = case_when( #categorise operations...
    !is.na(hip_ops) ~ hip_ops, #if it is a listed hip_op, return the hip_op type
    str_sub(OPCS, 1, 1) %in% c('U','Y','Z') ~ 'imaging_op.site', #if it is imaging, return 'imaging_op.site'
    str_sub(OPCS, 1, 3) == 'M47' ~ 'imaging_op.site', #if it is imaging, return 'imaging_op.site'
    OPCS == '&' ~ 'no_proc', #if it is an ampersand, return 'no_proc' (no procedure)
    TRUE ~ 'other_proc' #if it is something else, return 'other proc' (other procedure)
  ))

#Create patid and CIPS level wide dataset of categorised ops
op_counts <- all_ops %>% #take the list of all categorised ops
  pivot_wider(id_cols = c(patid, cips), names_from = cat_ops, #make wide dataset by type of operation, patid and CIPS
              values_from = cat_ops, values_fn = list(cat_ops = length)) #count numbers of ops of each type



#Identifying hip and heart cohorts using primary diagnoses in index episode

#Create dataset identifying hip and heart primary diagnoses in index episodes of CIPS
hipheart <- APCcips %>% group_by(patid, cips) %>% arrange(admidate) %>% #group and sort HES spell-identifying index episodes
  filter(row_number()==1) %>% #get first episode for each within-patient CIPS
  ungroup() %>% #ungroup the data
  left_join(APCdiags, by = c('patid', 'cips', 'spno', 'cipfirstepikey' = 'epikey', 'cipstart' = 'epistart')) %>% #join on diagnoses for the CIPS index episodes
  inner_join(ICDdefs) %>% #join on ICD definitions, containing flags identifying hip and heart cohorts by ICD code using inner join
  filter((d_order == 1 & flag == 'hip') | (d_order == 1 & flag == 'heart')) %>% #filter out primary diagnoses which are for hip/heart cohorts
  group_by(patid, flag) %>% #group by patid and cohort flags
  arrange(patid, cipstart) %>% #sort the data by patid and CIPS start date
  filter(row_number()==1) %>% #get earliest flag for each patient and cohort flag (hip and/or heart)
  ungroup() %>% #ungroup the data
  pivot_wider(id_cols = c(patid, cips, cipfirstepikey, ICD), names_from = flag, values_from = cipstart) %>% #separate index dates by hip/heart flag
  left_join(op_counts) #join on categorised operation counts, that match the retained CIPS


#Check hip operations have been categorised correctly
# check_hip_ops <- hipheart %>% drop_na(hip) %>% #retain only the hip admission cohort
#   left_join(all_ops) %>% #join operation codes (OPCS) onto hip cohort
#   group_by(cat_ops, OPCS) %>% tally() %>% ungroup() %>% #tally them up by their categorisation and code
#   mutate(cat_ops = ifelse(is.na(cat_ops), 'no_proc', cat_ops)) %>% #replace NAs with 'no_proc' where no operation code was linked to spell
#   arrange(cat_ops, desc(n))
# 
# write_csv(check_hip_ops, here('../Outputs', 'check_hip_ops.csv')) #write check to file


#Get first categorised hip op date for each patient and spell (total, partial, osteopin)
first_hip_op <- all_ops %>% #get all categorised operation data
  filter(cat_ops %in% c('hip_op_total', 'hip_op_partial', 'hip_op_osteopin')) %>% #filter out the categorised hip ops
  group_by(patid, cips) %>% arrange(evdate) %>% #group by patid and cips and sort by operation date
  filter(row_number() == 1) #get earliest operation for each patient/spell (i.e. there may be several in each CIPS)


#Identify dates of hip operations for hip cohort where they had a categorised hip op (total, partial, osteopin)
hipdates <- hipheart %>% drop_na(hip) %>% #keep only hip cohort
  mutate(hip_cat = case_when( #categorise ops into hierarchy: total > partial > osteopin > other > no procedure (including imaging_op.site)
    !is.na(hip_op_total) ~ 'hip_op_total',
    !is.na(hip_op_partial) ~ 'hip_op_partial',
    !is.na(hip_op_osteopin) ~ 'hip_op_osteopin',
    !is.na(other_proc) ~ 'other_procedure',
    TRUE ~ 'no_procedure'
  )) %>% 
  left_join(first_hip_op %>% select(patid, cips, evdate, op_epikey = epikey)) %>% #join on dates for the patid/cips/hip operation combination
  mutate(days_to_op = evdate - hip) %>% #calculate days to hip op from index date
  #NB: some weird days_to_op in both directions
  filter(days_to_op >= 0 | is.na(days_to_op)) #filter out negative days to op and leave any other/no-proc

saveRDS(hipdates, here('../Data', 'hipdates.rds')) #save to file


#Get only included hip patients
hip <- hipdates %>% inner_join(cohort) %>% #join on cohort demographic information
  mutate(hip_age = as.numeric(format(hip, '%Y')) - yob) %>% #calculate age at index date
  filter(hip_age >= 65) %>% #filter out any who are not aged 65+ at index date
  left_join(agegroups, by = c('hip_age' = 'age')) %>% #join on age groupings
  select(patid, hipcips = cips, hip, hip_cat, ICD, hip_age, hip_agegroup = agegroup, days_to_op) #keep required columns



#Cohort definitions for heart diagnoses built from hipheart
heart <- hipheart %>% drop_na(heart) %>% #get heart cohort index episodes (drop any which do not have a heart cohort index date)
  inner_join(cohort) %>% #join on cohort demographic information
  mutate(heart_age = as.numeric(format(heart, '%Y')) - yob) %>% #calculate age at index date
  filter(heart_age <= 90 & heart_age >= 65) %>% #filter out any who are not aged 65-90 at index date
  left_join(agegroups, by = c('heart_age' = 'age')) %>% #join on age groupings
  select(patid, cips, cipfirstepikey, heart, heart_age, heart_agegroup = agegroup2) #keep required columns

#Creating diabetes and COPD flags for heart cohort
dc_flag <- heart %>% select(patid, cips, cipfirstepikey, heart) %>% #get heart cohort info
  left_join(APCdiags) %>% #join on diagnoses
  inner_join(ICDdefs)%>% #join on ICD codes identifying cohorts
  filter(flag %in% c('diab', 'copd') &  #filter out: diabetes/COPD diagnoses AND...
           (cipfirstepikey == epikey & d_order > 1 | #...the first episode but not the primary diagnosis OR
                             cipfirstepikey != epikey)) %>% #...not for the first episode
  group_by(patid, cips, heart, flag) %>% #group by identifiers
  filter(row_number()==1) %>% #get one row for each patient-heartdate-flag combination
  ungroup() %>% #ungroup the data
  pivot_wider(id_cols = c(patid, cips, heart), names_from = flag, values_from = flag) #separate flags into two columns

#Merge all together
origindt <- as.Date('1970-01-01') #set date origin for use below...

heartdc <- left_join(heart, dc_flag) %>% #join diabetes and COPD flags onto heart
  mutate(heartd = as.Date(ifelse(diab == 'diab', heart, NA), origin = origindt), #create additional date columns based on flags: CHF + Diabetes
         heartdc = as.Date(ifelse(diab == 'diab' & copd == 'copd', heart, NA), origin = origindt), #CHF + Diabetes + COPD
         heartnodc = as.Date(ifelse(is.na(diab) & is.na(copd), heart, NA), origin = origindt)) %>% #CHF without Diabetes or COPD
  select(patid, hcips = cips, heart, heartd, heartdc, heartnodc, heart_age, heart_agegroup) #clean up columns

#___________________________________________________________________

#Create dementia cohort identifiers from CPRD primary care data ----

#NB: ICCONIC did not include the dementia cohort in this stage of their work 
#in the end, and so this section of code is redundant

#Function for creating dementia index dates
createIndexDates <- function(codelist, codename, dataset, datecol, indexname){
  readRDS(here('../Data', codelist)) %>% select(!!codename) %>% #read in codes to lookup
    left_join(readRDS(here('../Data', dataset))) %>% #left join healthcare dataset onto this list
    select(patid, index = !!datecol) %>% #keep only patid and date columns (renamed as 'index')
    filter(index <= elig_end) %>% #filter to ensure before end eligibility period is over (keep prevalent and emerging diagnoses)
    group_by(patid) %>% arrange(index) %>% #group by patid and sort by index date
    filter(row_number()==1) %>% #get earliest instance of relevant records for each patient
    rename(!!indexname := index) #rename index date so it is unique
}

#Create index dates for dementia therapies and diagnoses
dem_therapy <- createIndexDates('Dementia_Therapy.rds', 'prodcode', 'therapy.rds', 'eventdate', 'dem_ther_index')
dem_diagnosis <- createIndexDates('Dementia_Diagnosis.rds', 'medcode', 'clinical.rds', 'eventdate', 'dem_diag_index')

#Join together and process dementia index dates
dementia <- full_join(dem_therapy, dem_diagnosis) %>% 
  mutate(dementia = max(min(dem_ther_index, dem_diag_index, na.rm = TRUE), elig_start)) %>% #if before elig_start, use elig_start
  inner_join(cohort) %>% #join on cohort demographic information
  mutate(dem_age = as.numeric(format(dementia, '%Y')) - yob) %>% #calculate age at index date
  filter(dem_age >= 65) %>% #filter out any who are not aged 65+ at index date
  left_join(agegroups, by = c('dem_age' = 'age')) %>% #join on age groupings
  select(patid, dementia, dem_age, dem_agegroup = agegroup) #keep required columns

saveRDS(dementia, here('../Data', 'dementia.rds')) #save dementia index data to file

#_________________________________________________________________

#Join tables together to create cohort identification dataset ----

#dementia <- readRDS(here('../Data', 'dementia.rds')) #read in dementia dataset if already created as it takes a long time to run

cohorts <- cohort %>% left_join(hip) %>% left_join(heartdc) %>% left_join(dementia) %>% #join index dates to cohort data
  filter(!is.na(hip) | !is.na(heart) | !is.na(heartd) | !is.na(heartdc) | !is.na(heartnodc) | !is.na(dementia)) #remove any patients not in a study cohort

#______________________________________________

#General practice-based inclusion criteria ----

#This code runs additional eligibility criteria we often use, based on practice registration data
#lcd (practice last collection date)
#uts (up to standard date) 
#crd (current registration date)
#tod (transfer out date)
#dod (date of death)

#We needed a lookback year and a follow-up year in our study (from cohort member start date)
#lcd/tod must be a year after last index date... but, if there is a dod before tod, ignore tod
#uts/crd must be a year before first index date
#(accept field is all 1 in our data, so not included here)

check_cohort <- function(index, flag){
#index is a date column name (as string) at which a patient became a cohort member (date follow-up starts)
#flag is the name of an identifier (as a string) which will be created to say the cohort member passes the inclusion criteria
  
  cohorts %>% filter(!is.na(.data[[index]])) %>% #filter out patients with the chosen index date
    
    mutate(pat_end_date = .data[[index]] + 365, #create max follow-up year end date
           pat_end_date2 = if_else(dod > pat_end_date | is.na(dod), pat_end_date, dod), #if they die during it, use dod instead
           pat_start_date = .data[[index]] - 365, #create min look-back year start date
           lcd_check = lcd >= pat_end_date2, #check last collection date was after the patient end date
           tod_check = is.na(tod) | tod >= pat_end_date2, #check any transfer out date was after the patient end date
           uts_check = uts <= pat_start_date, #check that the practice was up-to-standard prior to the patient start date
           crd_check = crd <= pat_start_date, #check the current registration date was prior to the patient start date
           dod_check = is.na(dod) | dod >= .data[[index]]) %>% #check the patient did not die or, if they did, it was after the index date
  filter(lcd_check, tod_check, uts_check, crd_check, dod_check) %>% #filter out records passing all these tests
  mutate(flag = TRUE) %>% #create a flag to say that the record has passed the tests
  select(patid, !!flag := flag) #rename the flag column 
  
}

#Check each of the cohorts using the check_cohort function
c1 <- check_cohort('hip','inhip')
c2 <- check_cohort('heart','inheart')
c3 <- check_cohort('heartd','inheartd')
c4 <- check_cohort('heartdc','inheartdc')
c5 <- check_cohort('heartnodc','inheartnodc')
c6 <- check_cohort('dementia','indem')

cohorts_checked <- list(cohorts, c1, c2, c3, c4, c5, c6) %>% #list the datasets and...
  Reduce(left_join, .) #use Reduce to join them all onto 'cohorts'

#View some simple tallies of the checked cohorts
# cohorts_checked %>% filter(!is.na(hip)) %>% group_by(inhip) %>% tally() %>% View(title = 'hip')
# cohorts_checked %>% filter(!is.na(heart)) %>% group_by(inheart) %>% tally() %>% View(title = 'heart')
# cohorts_checked %>% filter(!is.na(heart)) %>% group_by(inheartd) %>% tally() %>% View(title = 'heartd')
# cohorts_checked %>% filter(!is.na(heart)) %>% group_by(inheartdc) %>% tally() %>% View(title = 'heartdc')
# cohorts_checked %>% filter(!is.na(heart)) %>% group_by(inheartnodc) %>% tally() %>% View(title = 'heartnodc')
# cohorts_checked %>% filter(!is.na(dementia)) %>% group_by(indem) %>% tally() %>% View(title = 'dem')

saveRDS(cohorts_checked, here('../Data','cohorts.rds')) #save the checked cohort data to file




