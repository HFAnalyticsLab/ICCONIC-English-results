#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Mortality outcomes for varying follow-up periods
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'here')
sapply(pkgs, require, character.only = TRUE)

#_________________________________________________________

#Create function to prepare death categorisation data ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohort data
APCcipsinfo <- readRDS(here('../Data', 'APCcipsinfo.rds')) #read in APC cips info

cat_deaths <- function(cohort_in){
  
  cohort_flag <- paste0('in', cohort_in) #add 'in' to the name of the cohort supplied: hip becomes inhip
  cips <- if(cohort_in == 'hip') {'hipcips'} else {'hcips'} #store the correct index cips identifier
  prepost <- paste0(cohort_in, 'post') #set the mortality follow-up variable based on the cohort supplied
  
  cohorts %>% filter(.data[[cohort_flag]]) %>% #keep cohort of interest
    rename(cips = !!cips) %>% left_join(APCcipsinfo) %>% #rename index cips identifier to 'cips' and join on cips info
    mutate(discharge_to_death = dod - cipend, #create count of days after discharge the patient died
           admission_to_death = dod - cipstart) %>% #create count of days after admission the patient died
    mutate(hospitaldeath = ifelse(discharge_to_death <= 0, TRUE, FALSE), #if they died before discharge: hospital death
           death30days = ifelse(admission_to_death <= 30, TRUE, FALSE), #died <= 30 days post-admission
           death90days = ifelse(admission_to_death <= 90, TRUE, FALSE), #died <= 90 days post-admission
           death365days = ifelse(admission_to_death <= 365, TRUE, FALSE), #died <= 365 days post-admission
           hospitaldeath30 = ifelse(admission_to_death <= 30 & discharge_to_death <= 0, TRUE, FALSE), #died in hospital <= 30 days post-admission
           hospitaldeath365 = ifelse(admission_to_death <= 365 & discharge_to_death <= 0, TRUE, FALSE)) %>% #died in hospital <= 365 days post-admission
    replace_na(list(hospitaldeath = F, death30days = F, death90days = F, death365days = F, hospitaldeath30 = F)) #make any NAs into FALSEs
}

hipdeaths <- cat_deaths('hip')
heartdeaths <- cat_deaths('heart')

saveRDS(hipdeaths, here('../Data', 'hipdeaths.rds'))
saveRDS(heartdeaths, here('../Data', 'heartdeaths.rds'))

#_________________________

#Create summary stats ----

hipdeaths <- readRDS(here('../Data', 'hipdeaths.rds'))
heartdeaths <- readRDS(here('../Data', 'heartdeaths.rds'))
source(here('R', 'Output_functions.R'))
#NB: in this use of stats_func, the death flags are being used as the 'cost_var' to provide a mean of a boolean (i.e. a percentage)
#Thus, the mean costs are the percentages of patients dying in the period-location

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)


#Hip cohort
hipstats1 <- stats_func(hip_cohort, hipdeaths, 'hospitaldeath30', NULL, 'hip_agegroup', hipcats)
hipstats2 <- stats_func(hip_cohort, hipdeaths, 'death30days', NULL, 'hip_agegroup', hipcats)
hipstats3 <- stats_func(hip_cohort, hipdeaths, 'death90days', NULL, 'hip_agegroup', hipcats)
hipstats4 <- stats_func(hip_cohort, hipdeaths, 'death365days', NULL, 'hip_agegroup', hipcats)

write_csv(hipstats1, here('../Outputs', 'hip_death_stats1.csv'))
write_csv(hipstats2, here('../Outputs', 'hip_death_stats2.csv'))
write_csv(hipstats3, here('../Outputs', 'hip_death_stats3.csv'))
write_csv(hipstats4, here('../Outputs', 'hip_death_stats4.csv'))


#Heart cohort
heartstats1 <- stats_func(heartd_cohort, heartdeaths, 'hospitaldeath30', NULL, 'heart_agegroup')
heartstats2 <- stats_func(heartd_cohort, heartdeaths, 'death30days', NULL, 'heart_agegroup')
heartstats3 <- stats_func(heartd_cohort, heartdeaths, 'death90days', NULL, 'heart_agegroup')
heartstats4 <- stats_func(heartd_cohort, heartdeaths, 'death365days', NULL, 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heartd_death_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heartd_death_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heartd_death_stats3.csv'))
write_csv(heartstats4, here('../Outputs', 'heartd_death_stats4.csv'))

#_____________________________________

#Additional work on heart cohorts ----

heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)

heartstats1 <- stats_func(heart_cohort, heartdeaths, 'hospitaldeath30', NULL, 'heart_agegroup')
heartstats2 <- stats_func(heart_cohort, heartdeaths, 'death30days', NULL, 'heart_agegroup')
heartstats3 <- stats_func(heart_cohort, heartdeaths, 'death90days', NULL, 'heart_agegroup')
heartstats4 <- stats_func(heart_cohort, heartdeaths, 'death365days', NULL, 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heart_death_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heart_death_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heart_death_stats3.csv'))
write_csv(heartstats4, here('../Outputs', 'heart_death_stats4.csv'))

heartstats1 <- stats_func(heartdc_cohort, heartdeaths, 'hospitaldeath30', NULL, 'heart_agegroup')
heartstats2 <- stats_func(heartdc_cohort, heartdeaths, 'death30days', NULL, 'heart_agegroup')
heartstats3 <- stats_func(heartdc_cohort, heartdeaths, 'death90days', NULL, 'heart_agegroup')
heartstats4 <- stats_func(heartdc_cohort, heartdeaths, 'death365days', NULL, 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heartdc_death_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heartdc_death_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heartdc_death_stats3.csv'))
write_csv(heartstats4, here('../Outputs', 'heartdc_death_stats4.csv'))

heartstats1 <- stats_func(heartnodc_cohort, heartdeaths, 'hospitaldeath30', NULL, 'heart_agegroup')
heartstats2 <- stats_func(heartnodc_cohort, heartdeaths, 'death30days', NULL, 'heart_agegroup')
heartstats3 <- stats_func(heartnodc_cohort, heartdeaths, 'death90days', NULL, 'heart_agegroup')
heartstats4 <- stats_func(heartnodc_cohort, heartdeaths, 'death365days', NULL, 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heartnodc_death_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heartnodc_death_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heartnodc_death_stats3.csv'))
write_csv(heartstats4, here('../Outputs', 'heartnodc_death_stats4.csv'))


