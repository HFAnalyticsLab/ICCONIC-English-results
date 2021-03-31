#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Additional work on end of life utilisation and spending
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and read in data ----

pkgs <- c('tidyverse', 'readxl', 'here')
sapply(pkgs, require, character.only = TRUE)

#_____________________________________________________________

#Create function to tally patients in end of life cohorts ----

#Prepare data for hip cohort
hip_cats <- c('hip_op_total','hip_op_partial','hip_op_osteopin')
hipdeaths <- readRDS(here('../Data', 'hipdeaths.rds')) %>% #read in all hip deaths data
  filter(inhip, hip_cat %in% hip_cats) #filter out inhip and hip ops

deathN <- function(deathflag = NULL){ #function expects death flags from mortality outcomes analysis
  temp <- if(is.null(deathflag)){hipdeaths} else {hipdeaths %>% filter(!!sym(deathflag))} #filter for death flag
  x1 <- temp %>% group_by(gender, hip_agegroup) %>% tally() #count patients by gender and age group
  x2 <- temp %>% group_by(hip_agegroup) %>% tally() %>% #count patients just by age group 
    bind_rows(x1) %>% #bind to previous results
    pivot_wider(id_cols = hip_agegroup, values_from = n, names_from = gender) #pivot wide by gender
  x3 <- x2 %>% summarise(across(c('NA', '1', '2'), sum, na.rm = TRUE)) #add totals by gender
  return(bind_rows(x2,x3) %>% select(hip_agegroup, All = 'NA', Female = '2', Male = '1')) #rename columns and return results
}

write.csv(deathN(deathflag = NULL), here('../Outputs', 'death_hip_all.csv'))
write.csv(deathN(deathflag = 'death365days'), here('../Outputs', 'death_hip_365.csv'))
write.csv(deathN(deathflag = 'hospitaldeath365'), here('../Outputs', 'death_hip_hosp365.csv'), na = '0')

#________________________________________________________

#Creating statistics for various end of life periods ----

source(here('R', 'Output_functions.R')) #source summary stats functions

#Amend the death cohort to within 365 days only and add eol time period identifiers
hipdeaths365 <- hipdeaths %>% filter(death365days) %>% #filter cohort to deaths within 365 days of index admission
  rename(hipcips = cips) %>% #rename index cips identifier
  select(!(cipstart:cipdur)) %>% #remove index cips info (cips info from all costed cips is needed later)
  mutate(eol_start30   = dod - 30, #add end of life period start dates...
         eol_start90   = dod - 90,
         eol_start180  = dod - 180)

eol_start_vars <- c(eol30 = 'eol_start30', eol90 = 'eol_start90', eol180 = 'eol_start180') #store the eol start date var names

#_________________

#APC analysis ----

#Read in cost and utilisation data
APCcosts <- readRDS(here('../Data', 'APCcosts.rds')) %>% ungroup() %>% #read in APC data and remove grouping
  select(!(hippre:dempost)) #remove pre/post variables

#Create function to prepare APC data
APCeol_prep <- function(eol_start){
  temp <- hipdeaths365 %>% #get hip cohort deaths within 365 days with end of life period indicators
    inner_join(APCcosts, by = c('patid', 'gender', 'dod', 'hip_cat', 'hip_agegroup')) %>% #join on APC costs using inner join
    filter(cipend >= !!sym(eol_start) & cipstart <= dod) %>% #remove records not overlapping end of life period
    mutate(t_start = ifelse(cipstart < !!sym(eol_start), !!sym(eol_start), cipstart), #adjust start dates so they are not before eol_start date
           t_end = ifelse(dod < cipend, dod, cipend), #adjust end dates so they are not after death
           t_days = t_end - t_start + 1, #add new days in hospital for cips within eol period
           percent_of_cips = t_days / cipdays, #work out percentage of original cips days within eol period
           eol_cost = totcost * percent_of_cips) %>% #multiply cost of cips by percentage within eol period
    ungroup() %>% arrange(patid) #ungroup and sort data
}
#NB: check that joining fields used above are constant within-patient

#Function to create summary statistics for various eol periods
APC_eol  <- function(sum_var){ #sum_var is the variable being summarised
  map_dfr(eol_start_vars, function(.x) { #for each of the eol period start variables...
  APCeol_prep(.x) %>% #prepare the APC data then send that to the summary statistics output function
    stats_func(cohort_in = hipdeaths365, cost_data = ., cost_var = sum_var, pre_post = NULL, 
               age_group = 'hip_agegroup', hip_cats = hip_cats, extra_cat = NULL)}, .id = 'eol')}

#NB: when using function for days in hospital, the days are the 'cost_var'
APC_mean_eol_costs <- APC_eol('eol_cost') %>% select(eol:Male_NA) #APC costs at end of life
APC_mean_eol_days  <- APC_eol('t_days')   %>% select(eol:Male_NA) #APC days in hospital at end of life

#NB: in the eol_APC_days output the 'meancost' column is actually the mean days in this case
write.csv(APC_mean_eol_costs, here('../Outputs', 'eol_APC_costs.csv'))
write.csv(APC_mean_eol_days, here('../Outputs', 'eol_APC_days.csv'))

#_____________________________________________________________________________________________________

#Create function to prepare data for any types of activity that occur same-day (i.e. A&E, OP, PC) ----

eol_func <- function(data_in, event_date, eol_start){ #this version is simpler than the APCeol_prep function
  hipdeaths365 %>% #get hip cohort deaths within 365 days with end of life period indicators
    inner_join(data_in, by = c('patid', 'gender', 'dod', 'hip_cat', 'hip_agegroup')) %>% #join on costs using inner join
    filter(!!sym(event_date) >= !!sym(eol_start) & !!sym(event_date) <= dod) #remove records not in end of life period
}

#_________________

#A&E analysis ----

#Read in cost and utilisation data
AEcosts_no_hosp <- readRDS(here('../Data', 'AEcosts.rds')) %>% #read AE costs data
  select(patid, aekey, gender, dod, hip_cat, hip_agegroup, arrivaldate, aeattenddisp, totcost) %>% #select necessary cols
  filter(aeattenddisp != 1) #remove admitted patients

#Call stats_func on data returned by eolfunc for each end of life time period
AE_eol  <- map_dfr(eol_start_vars, function(.x) { #for each of the eol period start variables...
              eol_func(AEcosts_no_hosp, 'arrivaldate', .x) %>% #prepare data using eol_func then send to the summary statistics output function
                stats_func(cohort_in = hipdeaths365, cost_data = ., cost_var = 'totcost', pre_post = NULL, 
                           age_group = 'hip_agegroup', hip_cats = hip_cats, extra_cat = NULL)}, .id = 'eol') %>% 
  select(eol:Male_NA) #reduce to required cols

write.csv(AE_eol, here('../Outputs', 'eol_AE_no_hosp.csv')) #write to file

#________________

#OP analysis ----

#Read in specialty categorisations
OPbuckets <- read_excel(here('Reference_data', 'Outpatient specialty code buckets.xlsx')) #categories based on 'mainspef'

#Read in cost and utilisation data
OPcosts <- readRDS(here('../Data', 'OPcosts.rds')) %>% #read OP costs data
  left_join(OPbuckets) %>% #join on specialty 'category'
  select(patid, attendkey, gender, dod, hip_cat, hip_agegroup, apptdate, totcost, category) #select necessary cols

#Call stats_func on data returned by eol_func for each end of life time period
OP_eol  <- map_dfr(eol_start_vars, function(.x) { #for each of the eol period start variables...
  eol_func(OPcosts, 'apptdate', .x) %>% #prepare data using eol_func then send to the summary statistics output function
    stats_func(cohort_in = hipdeaths365, cost_data = ., cost_var = 'totcost', pre_post = NULL, 
               age_group = 'hip_agegroup', hip_cats = hip_cats, extra_cat = 'category')}, .id = 'eol') %>% 
  select(eol:Male_NA) #reduce to required cols

write.csv(OP_eol, here('../Outputs', 'eol_OP_by_category.csv')) #write to file

#________________

#PC analysis ----

GPcosts <- readRDS(here('../Data', 'PCvisits.rds')) %>% #read in costed primary care visits
  select(patid, gender, dod, hip_cat, hip_agegroup, eventdate, role_cat, cost) %>% #reduce to required columns
  filter(role_cat == 'GP') #restrict to GP consultations and costs

#Call statsfunc on data returned by eol_func for each end of life time period
GP_eol  <- map_dfr(eol_start_vars, function(.x) { #for each of the eol period start variables...
  eol_func(GPcosts, 'eventdate', .x) %>% #prepare data using eol_func then send to the summary statistics output function
    stats_func(cohort_in = hipdeaths365, cost_data = ., cost_var = 'cost', pre_post = NULL, 
               age_group = 'hip_agegroup', hip_cats = hip_cats, extra_cat = NULL)}, .id = 'eol') %>% 
  select(eol:Male_NA) #reduce to required cols

write.csv(GP_eol, here('../Outputs', 'eol_GP_primary_care.csv')) #write to file

#___________________

#Drugs analysis ----

#Drugs are slightly problematic, as in the 'Costing_primary_care' script they were grouped by 'prepost' and 
#drug substance. However, for this analysis, we need the date of the prescription to see which eol period
#they are for using the 'eol_func'

PCdrugs_recs <- readRDS(here('../Data', 'PCdrugs_recs.rds')) %>% #read in costed drugs
  select(patid, gender, dod, hip_cat, hip_agegroup, eventdate, drugsubstance, cost) #reduce to required columns

PCdrugscosts <- map(eol_start_vars, function(.x){ #for each of the eol period start variables...
  eol_func(PCdrugs_recs, 'eventdate', .x) %>% #prepare data using eol_func
    select(patid, gender, dod, hip_cat, hip_agegroup, drugsubstance, cost) %>% #select required columns
    group_by(across(-cost)) %>% #group by everything except the cost
    summarise(countrecs = n(), drugcost = sum(cost, na.rm = TRUE), .groups = 'keep')}) #summarise (by patient and drug substance, essentially)

drugs_eol <- map_dfr(PCdrugscosts, function(.x) { #send summarised data to summary statistics output function
  stats_func(cohort_in = hipdeaths365, cost_data = .x, cost_var = 'drugcost', pre_post = NULL, 
             age_group = 'hip_agegroup', hip_cats = hip_cats, extra_cat = NULL)}, .id = 'eol') %>% 
  select(eol:Male_NA) #reduce to required cols

write.csv(drugs_eol, here('../Outputs', 'eol_Drugs_primary_care.csv')) #write to file

#____________________

#Survival tables ----

tab365 <- hipdeaths365 %>% group_by(hip_agegroup, admission_to_death) %>% tally() %>% #summarise hip deaths within year follow-up
  ungroup() %>% mutate(result = NA) #ungroup and add a column for storing the result of the algorithm below

age_groups <- c('65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95+') #store age groups used in hip analysis

#Statistical Disclosure Control (SDC) survival table function
SDC_counter <- function(.g) { #function expects an age group
  
  x <- tab365 %>% filter(hip_agegroup == .g) #filter the summary table of counts (tab365) by age group (.g)
  y <- 0 #initialise counter y to zero
  L <- nrow(x) #store last row number of table x

  for (i in 1:L){ #for each row...
    y <- y + x$n[i] #add the value in the tally column (n) to y 
    if (y >= 10) { #if the value is >=10
      x$result[i] <- y #store in the result column
      z <- i #remember which row this occurred on
      y <- 0 #re-initialise y to zero
    }
    if (i == L & i != z){ #if you've reached the last row and it is not the last row >= 10
      x$result[i] <- y + x$result[z] #add y to the last row >= 10 and store in result column
      x$result[z] <- NA #remove the value that was last >= 10 from the results
    }
  }
  return(x)
}

base_N <- hipdeaths %>% group_by(hip_agegroup) %>% tally(n = 'base') #create a base column by age group

survival <- map_dfr(age_groups, SDC_counter) %>% filter(!is.na(result)) %>% #send age groups to SDC_counter and filter out results
  group_by(hip_agegroup) %>% mutate(cumsum = cumsum(result)) %>% #create a cumulative sum column by age group
  inner_join(base_N) %>% mutate(survival = (base - cumsum) / base) #join on the bases and create a cumulative percentage survival

write.csv(survival, here('../Outputs', 'eol_survival_table.csv')) #write to file

  
