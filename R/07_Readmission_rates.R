#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Readmission rates for varying follow-up periods
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'here')
sapply(pkgs, require, character.only = TRUE)

#_________________________________________________________

#Create function to prepare readmits categorisation data ----
  
cohorts <- readRDS(here('../Data', 'cohorts.rds')) 
APCcipsinfo <- readRDS(here('../Data', 'APCcipsinfo.rds')) 

cat_readmissions <- function(cohort_in){

  cohort_flag <- paste0('in', cohort_in) #change hip/heart to inhip/inheart
  prepost <- paste0(cohort_in, 'post') #currently only doing this for follow-up year ('pre' is lookback year)
  cohort_index <- if(cohort_in == 'hip') {'hip'} else {'heart'} #all heart cohorts use same date column
  
  readmits <- cohorts %>% filter(.data[[cohort_flag]]) %>% #get cohort of interest
    left_join(APCcipsinfo) %>% #join on cips info
    filter(cipstart > .data[[cohort_index]]) %>% #filter out cips after index cips
    group_by(patid) %>% arrange(cipstart) %>% #group by patient and arrange
    filter(row_number() == 1) %>% #get first readmission for each patient
    select(patid, cipstart) %>% #reduce to just the patid and cipstart date
    right_join(cohorts %>% filter(.data[[cohort_flag]])) %>% #join back on to relevant cohort
    mutate(days_to_readmit = cipstart - .data[[cohort_index]]) %>% #get days between readmission and index date
    mutate(readmit30days = ifelse(days_to_readmit <= 30, TRUE, FALSE), #flag if within 30 days
           readmit90days = ifelse(days_to_readmit <= 90, TRUE, FALSE), #flag if within 90 days
           readmit365days = ifelse(days_to_readmit <= 365, TRUE, FALSE)) %>% #flag if within 365 days
    replace_na(list(readmit30days = F, readmit90days = F, readmit365days = F)) #replace any NAs with FALSE
																								 
}

hip_readmits <- cat_readmissions('hip') 
heart_readmits <- cat_readmissions('heart')

#___________________________

#Process and save stats ----

source(here('R', 'Output_functions.R'))
#NB: in this use of stats_func, the readmission flags are being used as the 'cost_var' to provide a mean of a boolean (i.e. a percentage)

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)

#Hip cohort
hipstats1 <- stats_func(hip_cohort, hip_readmits, 'readmit30days', NULL, 'hip_agegroup', hipcats)
hipstats2 <- stats_func(hip_cohort, hip_readmits, 'readmit90days', NULL, 'hip_agegroup', hipcats)
hipstats3 <- stats_func(hip_cohort, hip_readmits, 'readmit365days', NULL, 'hip_agegroup', hipcats)

write_csv(hipstats1, here('../Outputs', 'hip_readmit_stats1.csv'))
write_csv(hipstats2, here('../Outputs', 'hip_readmit_stats2.csv'))
write_csv(hipstats3, here('../Outputs', 'hip_readmit_stats3.csv'))


#Heart and diabetes cohort
heartstats1 <- stats_func(heartd_cohort, heart_readmits, 'readmit30days', NULL, 'heart_agegroup')
heartstats2 <- stats_func(heartd_cohort, heart_readmits, 'readmit90days', NULL, 'heart_agegroup')
heartstats3 <- stats_func(heartd_cohort, heart_readmits, 'readmit365days', NULL, 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heartd_readmit_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heartd_readmit_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heartd_readmit_stats3.csv'))

#____________________________________

#Additional work on heart cohort ----

heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)

make_tables <- function(cohort_in){
  outputs <- c('readmit30days', 'readmit90days', 'readmit365days')
  temp <- map(outputs, function(.x){
    stats_func(cohort_in, heart_readmits, .x, NULL, 'heart_agegroup')
  })
  names(temp) <- outputs
  bind_rows(temp, .id = 'stat')
}

heartstats1 <- make_tables(heart_cohort)
heartstats2 <- make_tables(heartdc_cohort)
heartstats3 <- make_tables(heartnodc_cohort)

write_csv(heartstats1, here('../Outputs', 'heart_readmit_stats.csv'))
write_csv(heartstats2, here('../Outputs', 'heartdc_readmit_stats.csv'))
write_csv(heartstats3, here('../Outputs', 'heartnodc_readmit_stats.csv'))


