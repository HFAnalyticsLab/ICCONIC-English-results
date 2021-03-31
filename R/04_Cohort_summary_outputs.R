#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Summary outputs relating to patient characteristics for cohorts
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'here') #package list
sapply(pkgs, require, character.only = TRUE) #load them up

#______________________________________________________

#Check how many hip patients are transferred for their index hip operation ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohorts data
APCepis <- readRDS(here('../Data', 'APCepis.rds')) %>% left_join(APCcipsIDs) #need to join cips number onto epis
hipdates <- readRDS(here('../Data', 'hipdates.rds')) #read in hip operation dates

transfers_hip <- cohorts %>% filter(!is.na(hip), inhip) %>% #get the hip patients
  left_join(hipdates) %>% left_join(APCepis, by = c('patid', 'cips', 'op_epikey' = 'epikey')) %>% #join hip operation dates and epis on
  mutate(transfer = admimeth %in% c('2B', '81') | #identify if the episode was a transfer (2B is converted to 67 in Fiona's HES pipeline)
           admisorc %in% c(51, 52, 53)) %>% 
  group_by(transfer) %>% tally() %>% #tally by whether a transfer
  mutate(perc = n/sum(n)) #add a percentage column

write_csv(transfers_hip, here('../Outputs', 'hip_transfers.csv')) #write transfer summary to file

#__________________________________________________________________________

#Check how many heart patients are transferred during their index CIPS ----

transfers_heart <- function(cohort_in){
  
  stats1 <- cohorts %>% filter(!is.na(heart), .data[[cohort_in]]) %>% #get the relevant heart cohort
    left_join(APCepis, by = c('patid', 'hcips' = 'cips')) %>% #join the episodes on
    mutate(transfer = admimeth %in% c('2B', '81') | #identify if the episode was a transfer (2B is converted to 67 in Fiona's HES pipeline)
             admisorc %in% c(51, 52, 53)) %>% 
    group_by(patid, gender) %>% summarise(transfers = sum(transfer) > 0) %>% #summarise transfers by patient
    group_by(gender, transfers) %>% tally() %>% mutate(perc = n/sum(n)) #count by transferred/gender and add percent
  
  stats2 <- stats1 %>% group_by(transfers) %>% summarise(n = sum(n)) %>% mutate(perc = n/sum(n)) #count by transferred and add percent
  bind_rows(list(stats2, stats1)) #bind the results together
  
}

t_heart <- transfers_heart('inheart') #create outputs for cohorts
t_heartd <- transfers_heart('inheartd')
t_heartdc <- transfers_heart('inheartdc')
t_heartnodc <- transfers_heart('inheartnodc')

write_csv(t_heart, here('../Outputs', 'heart_transfers.csv')) #write results to file
write_csv(t_heartd, here('../Outputs', 'heartd_transfers.csv'))
write_csv(t_heartdc, here('../Outputs', 'heartdc_transfers.csv'))
write_csv(t_heartnodc, here('../Outputs', 'heartnodc_transfers.csv'))

#________________________________________________________________________________________________

#Check how many no procedure hip patients died in hospital and within 30 days from admission ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohorts
APCcipsinfo <- readRDS(here('../Data', 'APCcipsinfo.rds')) #read in APC CIPS information

hipdeaths <- cohorts %>% drop_na(hip) %>% filter(hip_cat == 'no_procedure') %>% #get 'no procedure' hip cohort
  rename(cips = hipcips) %>% left_join(APCcipsinfo) %>% #join on CIPS info for index CIPS
  mutate(discharge_to_death = dod - cipend, #count days from discharge to death
         admission_to_death = dod - cipstart) %>% #count days from admission to death
  mutate(hospital_death = ifelse(discharge_to_death <= 0, TRUE, FALSE), #flag if died in hospital
         death30days = ifelse(admission_to_death <= 30, TRUE, FALSE)) %>% #flag if died within 30 days
  group_by(hospital_death, death30days) %>% tally() %>% mutate(cohort = 'hip') %>% ungroup() #tally the results

total <- sum(hipdeaths$n) #store total death count

hdeaths <- hipdeaths %>% filter(hospital_death == TRUE) %>% mutate(type = 'hospdeaths') %>% #filter out hospital deaths
  summarise(type = first(type), sum = sum(n), total = !!total, perc = sum/total) #work out percentage of total

deaths30 <- hipdeaths %>% filter(death30days == TRUE) %>% mutate(type = 'deaths30days') %>% #filter out deaths within 30 days
  summarise(type = first(type), sum = sum(n), total = !!total, perc = sum/total) #work out percentage of total

bind_rows(list(hdeaths, deaths30)) %>% write_csv(here('../Outputs', 'hip_no_procedure_deaths.csv')) #write results to file

#_________________________

#Create summary stats ----

#Hip/heart cohort denominator stats

statsfunc1 <- function(groups, cohortagevar, cohort_in){

  data_in <- if('gender' %in% groups){cohorts} else {cohorts %>% mutate(gender = NA)} #wipe gender if not one of 'groups'
  
  stats1 <- data_in %>% filter(!is.na(.data[[cohortagevar]]), .data[[cohort_in]]) %>% #use only patients in the cohort who have a non-missing age
    group_by(gender) %>% #group by gender (if previously wiped, this does nothing)
    summarise(mean_age = mean(.data[[cohortagevar]], na.rm = TRUE), #summarise age with... mean
              median_age = median(.data[[cohortagevar]], na.rm = TRUE), #median
              stdev_age = sd(.data[[cohortagevar]], na.rm = TRUE), #standard deviation
              N = sum(!is.na(.data[[cohortagevar]]))) %>% #count
    mutate(mean_sd = paste0(round(mean_age, 2), ' (', round(stdev_age, 2), ')'), #create text version: "mean (stdev)"
           median = paste0(median_age)) %>% select(gender, mean_sd, median) %>% #reduce to required columns
    pivot_longer(cols = c(mean_sd, median), names_to = 'type') #make long format
  
  stats2 <- cohorts %>% filter(!is.na(.data[[cohortagevar]]), .data[[cohort_in]]) %>% #use only patients in the cohort who have a non-missing age
    group_by_at(vars(one_of(!!groups))) %>% tally() %>% #group by 'groups' vars and tally
    mutate(value = as.character(n / sum(n)), type = 'perc') #create percent and type column (convert to percent character so row binding works later)
  
  stats3 <- stats2 %>% summarise(sum_n = sum(n)) %>% mutate(value = as.character(sum_n), type = 'N') #create sample size 'N' value (as character)
  
  output <- bind_rows(list(stats1, stats2, stats3)) %>% arrange(gender) %>% select(-n, -sum_n) #bind it all together, sort and remove unnecessary cols
  return(output) #return result
  
}

#Create stats for the hip cohort
hipstats1 <- statsfunc1(groups = c('hip_agegroup'), cohortagevar = 'hip_age', cohort_in = 'inhip') #do hip stats by age group
hipstats2 <- statsfunc1(groups = c('gender','hip_agegroup'), cohortagevar = 'hip_age', cohort_in = 'inhip') #do hip stats by gender and age group
bind_rows(list(hipstats1, hipstats2)) %>% write_csv(here('../Outputs', 'hip_denominators.csv')) #bind together and write to file

#Create stats for all the various heart cohorts
heart_stats <- function(cohort_in){
  heartstats1 <- statsfunc1(groups = c('heart_agegroup'), cohortagevar = 'heart_age', cohort_in = cohort_in) #do heart stats by age group
  heartstats2 <- statsfunc1(groups = c('gender','heart_agegroup'), cohortagevar = 'heart_age', cohort_in = cohort_in) #do heart stats by gender and age group
  bind_rows(list(heartstats1, heartstats2)) %>% write_csv(here('../Outputs', paste0('heart_denominators_', cohort_in, '.csv'))) #bind together and write to file
}
map(list('inheart', 'inheartdc', 'inheartnodc'), heart_stats) #send various heart cohorts to function



#Days to hip operation
hip_days <- cohorts %>% filter(!is.na(hip), inhip) %>%  #get hip cohort
  mutate(days = case_when( #categorise days to operation...
    days_to_op == 0 ~ '1_same_day', 
    days_to_op == 1 ~ '2_next_day',
    days_to_op == 2 ~ '3_third_day',
    days_to_op >= 3 ~ '4_fourth_day+')) %>%
  group_by(days) %>% tally() %>% #group and tally by days
  filter(!is.na(days)) %>% #filter out patients without an op
  mutate(perc = n/sum(n)) #create percent column
  
write_csv(hip_days, here('../Outputs', 'hip_days_to_op.csv')) #write to file



#Diagnosis and procedure overall
sortorder <- data.frame(hip_cat = c('hip_op_total','hip_op_partial','hip_op_osteopin','other_procedure','no_procedure'), sort = 1:5) #create sort order for the operations (for sorting results later)
cohorts <- cohorts %>% left_join(sortorder) #join sort order onto cohorts

dp1 <- cohorts %>% filter(!is.na(hip), inhip) %>% group_by(ICD, hip_cat, sort) %>% tally() #get hip cohort, group by ICD and operation category and tally
dp1_totals <- dp1 %>% group_by(ICD) %>% summarise(n = sum(n)) %>% mutate(hip_cat = 'TOTAL', sort = 0) #create totals by ICD code (adding sort order value)
bind_rows(list(dp1, dp1_totals)) %>% arrange(ICD, sort) %>% select(-sort) %>% write_csv(here('../Outputs', 'hip_diags_ops.csv')) #bind together, sort and write to file


#___________________________________________________________________________________

#Patient characteristics (for Elixhauser comorbidity stats see separate script) ----

#Hip patient characteristics
statsfunc2 <- function(groups, index, cohort_in, wide_vars){
  
  hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #store the hip op categories of interest
  if('hip_cat' %in% groups){groups <- c(groups, 'sort')} #if hip_cat is part of 'groups', add the sort col
  
  pc1 <- cohorts %>% filter(!is.na(.data[[index]]), .data[[cohort_in]]) %>% filter(hip_cat %in% hipcats) %>% #filter out the hip cohort and ops
    group_by_at(vars(one_of(!!groups))) %>% tally() #group by 'groups' and tally
  groups2 <- groups[!groups %in% index] #remove hip_agegroup from the groups
  pc1_totals <- pc1 %>% group_by_at(vars(one_of(!!groups2))) %>% #by each remaining group...
	summarise(n = sum(n)) #%>% mutate(hip_cat = 'TOTAL', sort = 0) #create total counts (setting sort order)
  results <- bind_rows(list(pc1, pc1_totals)) #bind the totals to the previous results

  results_all_genders <- results %>% mutate(gender = 3) %>% #wipe out the gender values
	group_by_at(vars(one_of(!!groups))) %>% summarise(n = sum(n)) #regroup and sum to get totals by group for both genders
  results <- bind_rows(list(results, results_all_genders)) #bind the results together
  
  if('hip_cat' %in% groups){ #if hip_cat is in 'groups'...
    results <- results %>% arrange(sort, -gender) %>% select(-sort) #sort using gender and the sort variable (and then remove it)
  } else { #otherwise...
    results <- results %>% arrange(-gender) #sort just using gender
  }
  results <- results %>% pivot_wider(names_from = wide_vars, values_from = n) %>% ungroup() #make wide format using 'wide-vars'
  if('ICD' %in% groups) {
    results %>% arrange(ICD, hip_agegroup) #if ICD in 'groups', sort using the agegroup and ICD
  } else { #otherwise...
    results %>% arrange(hip_agegroup) #just sort using agegroup
  }
}

#Run the function and write to file multiple times to get all the output requested by ICCONIC

patchar1 <- statsfunc2(groups = c('ICD', 'hip_agegroup', 'gender', 'hip_cat'), index = 'hip_agegroup', cohort_in = 'inhip', wide_vars = c('hip_cat', 'gender'))
write_csv(patchar1, here('../Outputs', 'hip_pat_chars1.csv'), na = '0')

patchar2 <- statsfunc2(groups = c('ICD', 'hip_agegroup', 'gender'), index = 'hip_agegroup', cohort_in = 'inhip', wide_vars = 'gender')
write_csv(patchar2, here('../Outputs', 'hip_pat_chars2.csv'), na = '0')

patchar3 <- statsfunc2(groups = c('hip_agegroup', 'gender', 'hip_cat'), index = 'hip_agegroup', cohort_in = 'inhip', wide_vars = c('hip_cat', 'gender'))
write_csv(patchar3, here('../Outputs', 'hip_pat_chars3.csv'), na = '0')

patchar4 <- statsfunc2(groups = c('hip_agegroup', 'gender'), index = 'hip_agegroup', cohort_in = 'inhip', wide_vars = c('gender'))
write_csv(patchar4, here('../Outputs', 'hip_pat_chars4.csv'), na = '0')



#Heart patient characteristics (simpler than for the hip cohort)
statsfunc2h <- function(groups, index, cohort_in, wide_vars){
    
  pc1 <- cohorts %>% filter(!is.na(.data[[index]]), .data[[cohort_in]]) %>% #filter out cohort
    group_by_at(vars(one_of(!!groups))) %>% tally() #group by 'groups' and tally
  groups2 <- groups[!groups %in% index] #remove agegroup from the groups
  pc1_totals <- pc1 %>% group_by_at(vars(one_of(!!groups2))) %>% #by each remaining group...
	summarise(n = sum(n)) #%>% mutate(hip_cat = 'TOTAL', sort = 0) #create total counts (setting sort order)
  results <- bind_rows(list(pc1, pc1_totals)) #bind the totals to the previous results
    
  results_all_genders <- results %>% mutate(gender = 3) %>% #wipe out the gender values
	group_by_at(vars(one_of(!!groups))) %>% summarise(n = sum(n)) #regroup and sum to get totals by group for both genders
  results <- bind_rows(list(results, results_all_genders)) %>% arrange(-gender) #bind the results together and sort by gender

  results <- results %>% pivot_wider(names_from = wide_vars, values_from = n) %>% ungroup() #make wide format using 'wide-vars' 
  results %>% arrange(!!index) #return the results sorted by the agegroup
}

patchar1heart <- statsfunc2h(groups = c('heart_agegroup', 'gender'), index = 'heart_agegroup', cohort_in = 'inheart', wide_vars = c('gender'))
write_csv(patchar1heart, here('../Outputs', 'heart_pat_chars1.csv'), na = '0') #write results to file



#_____________________________________

#Ages for patient characteristics ----

statsfunc3 <- function(groups, index, cohort_in){
  
  if(index == 'hip_age'){ #if working on the hip cohort...
    data_in <- cohorts %>% filter(hip_cat %in% hipcats) #filter for the hip operations
  } else { #otherwise...
    data_in <- cohorts %>% filter(!is.na(.data[[index]])) #filter on the index provided
  }
  
  data_in %>% filter(.data[[cohort_in]]) %>% #filter for the cohort
    group_by_at(vars(one_of(!!groups))) %>% #group by the grouping vars
    summarise(mean_age = mean(.data[[index]], na.rm = TRUE), #summarise with: mean
              median_age = median(.data[[index]], na.rm = TRUE), #median
              stdev_age = sd(.data[[index]], na.rm = TRUE), #standard deviation
              mean_sd = paste0(round(mean_age,2), ' (', round(stdev_age,2),')'), #reformat as text: "mean (stdev)"
              median = as.character(median_age), #reformat as text
              N = sum(!is.na(.data[[index]])), .groups = 'keep') %>% #add a sample size count (for checking)
    select(-mean_age, -median_age, -stdev_age, -N) #remove unnecessary cols
}

#Hip cohort
ages1 <- statsfunc3(groups = c('sort', 'hip_cat', 'gender'), index = 'hip_age', cohort_in = 'inhip') 
ages2 <- statsfunc3(groups = c('sort', 'hip_cat'), index = 'hip_age', cohort_in = 'inhip')
ages3 <- statsfunc3(groups = c('gender'), index = 'hip_age', cohort_in = 'inhip')
ages4 <- statsfunc3(groups = c(), index = 'hip_age', cohort_in = 'inhip')
bind_rows(list(ages1, ages2, ages3, ages4)) %>% 
  mutate(sort = ifelse(is.na(sort), 0, sort), gender = ifelse(is.na(gender),3, gender)) %>% arrange(sort, -gender) %>% 
  write_csv(here('../Outputs', 'hip_age_stats.csv'))

#Heart cohort
ages1 <- statsfunc3(groups = c('gender'), index = 'heart_age', cohort_in = 'inheart') 
ages2 <- statsfunc3(groups = c(), index = 'heart_age', cohort_in = 'inheart')
bind_rows(list(ages1, ages2)) %>% mutate(gender = ifelse(is.na(gender),3, gender)) %>% arrange(-gender) %>% 
  write_csv(here('../Outputs', 'heart_age_stats.csv'))





