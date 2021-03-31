#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Process and cost CPRD primary care data - consultations and therapies
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----

pkgs <- c('tidyverse', 'here')
sapply(pkgs, require, character.only = TRUE)

#__________________

#Consultations ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohorts
cons <- readRDS(here('../Data', 'consultation.rds')) %>% select(-sysdate, -consid) #read in consultations and remove unnecessary columns
staff <- readRDS(here('../Data', 'staff.rds')) %>% #read in staff dataset. NB: it was duplicated in our raw data...
  group_by(staffid, role) %>% tally() %>% select(staffid, role) #...so we group and summarise it here
cot <- readRDS(here('../Data', 'cot.rds')) #read in CPRD reference data: consultation codes
rol <- readRDS(here('../Data', 'rol.rds')) #read in CPRD reference data: staff role codes
deflators <- readRDS(here('Reference_data', 'deflators.rds')) #NB: not using this for primary care visits at the moment


# Face to face consultation codes:	Home consultation codes:		Telephone consultation codes:
# --------------------------------	------------------------		-----------------------------
# Clinic,1,							            Home Visit,27,					    Telephone call from a patient,10,
# Follow-up/routine visit,3,			  Hotel Visit,28,					    Telephone call to a patient,21,
# Night visit , practice,6,			    Nursing Home Visit,30,			Triage,33,
# Out of hours, Practice,7,			    Residential Home Visit,31,	Telephone Consultation,55,
# Surgery consultation,9,				    Twilight Visit,32,
# Acute visit,11,						        Night Visit,50,
# Emergency Consultation,18,
# Initial Post Discharge Review,48,
# 
# GP codes from rol:				Nurse codes from rol:				    Other clinician codes from rol:
# ------------------				---------------------				    -------------------------------
# Senior Partner,1,				  Practice Nurse,11,					    Physiotherapist,26,
# Partner,2,						    Other Nursing & Midwifery,54		Other Health Care Professional,33
# Assistant,3,
# Associate,4, 
# Locum,7,
# GP Registrar,8,
# Sole Practitioner,10,
# Salaried Partner,47,
# GP Retainer,50,
# Other Students,53


#PSSRU unit costs of health and social care are difficult to use due to many changes over the years.
#The overall estimates do not change massively over time, so we use:
# - £56 per patient contact hour (with quals) for nurses
# - £216 per patient contact hour (with quals) for GPs


dat1 <- cohorts %>% left_join(cons) %>% #join consultations onto cohort to reduce it down (cons is very large)
  mutate(heartpre = eventdate >= (heart - 365) & eventdate < heart, #add indicators for cohort pre/post relevance...
         heartpost = eventdate < (heart + 365) & eventdate >= heart,
         hippre = eventdate >= (hip - 365) & eventdate < hip,
         hippost = eventdate < (hip + 365) & eventdate >= hip,
         dempre = eventdate >= (dementia - 365) & eventdate < dementia,
         dempost = eventdate < (dementia + 365) & eventdate >= dementia) %>%
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) %>% #filter out rows with TRUE in cohort identifiers
  left_join(staff) %>% left_join(rol, by = c('role' = 'Code')) %>% #join on staff info
  left_join(cot, by = c('constype' = 'Code')) %>% #categorise consultation type
  mutate(role_cat = case_when( #categorise staff types
    role %in% c(11, 54) ~ 'Nurse',
    role %in% c(1, 2, 3, 4, 7, 8, 10, 47, 50, 53) ~ 'GP',
    TRUE ~ 'other'
  ),
  cons_cat = case_when( #categorise consultation types
    constype %in% c(1, 3, 6, 7, 9, 11, 18, 48) ~ 'F2F',
    constype %in% c(27, 28, 30, 31, 32, 50) ~ 'Home',
    constype %in% c(10, 21, 33, 55) ~ 'Telphn',
    TRUE ~ 'other'
  )) %>%
  filter(role_cat %in% c('Nurse', 'GP'), cons_cat %in% c('F2F', 'Home')) %>% #ICCONIC only cost nurse/GP, face-to-face/home visits
  mutate(year = as.integer(format(eventdate, '%Y')), #add a year as integer
         month = as.integer(format(eventdate, '%m')), #add a month as integer
         HRGyear = as.character(ifelse(month <= 3, year - 1, year))) %>% #work out the HRG year (e.g. 2014 => 2014/15)
  #left_join(deflators) %>% #ignoring deflation for now, because PSSRU costs don't seem to change reliably over time
  mutate(minute_cost = ifelse(role_cat == 'GP', 216/60, 56/60), #get cost per minute (e.g. £216 / 60 mins for a GP)
         cons_dur = ifelse(duration == 0, 0.5, ifelse(duration > 60, 60, duration)), #set min time 0.5 minutes, max time 60 minutes
         cost = cons_dur * minute_cost) #calculate cost of consultation

saveRDS(dat1, here('../Data', 'PCvisits.rds')) #save to file

#____________________

#Therapy records ----

dmdprices <- readRDS(here('Reference_data', 'dmd_prices.rds')) #read in drug price data
ther <- readRDS(here('../Data', 'therapy.rds')) %>% #read in therapy records
  select(patid, eventdate, prodcode, qty) #reduce to required columns (it is a very large dataset)

dat2 <- cohorts %>% left_join(ther) %>% left_join(dmdprices) %>% #join data onto cohorts
  mutate(heartpre = eventdate >= (heart - 365) & eventdate < heart, #add indicators for cohort pre/post relevance...
         heartpost = eventdate < (heart + 365) & eventdate >= heart,
         hippre = eventdate >= (hip - 365) & eventdate < hip,
         hippost = eventdate < (hip + 365) & eventdate >= hip,
         dempre = eventdate >= (dementia - 365) & eventdate < dementia,
         dempost = eventdate < (dementia + 365) & eventdate >= dementia) %>%
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) #filter out rows with TRUE in cohort identifiers

#Summarise success of pricing attempt
dat2 %>% summarise(perc_unmatched = sum(is.na(drugsubstance))/length(patid)) #therapy records not matched to drug substances: ~7% 

#Need to use 2020 deflator as a multiple because dm+d prices are from 2020 NHSBSA dm+d dataset
#(NB: actually prices each have a date associated with them in dm+d, but these are the most uptodate from TRUD at 2020)
def2020 <- deflators[deflators$HRGyear == 2020, 'def_mult']

dat3 <- dat2 %>% filter(!is.na(drugsubstance) & !is.na(price)) %>% #remove unmatched/unpriced records
  mutate(cost = qty * price * !!def2020) %>% #multiply price by quantity prescribed and deflator for 2020 -> 2016
  select(patid, gender, dod, hip:indem, heartpre:dempost, eventdate, drugsubstance, cost) #reduce dataset to required columns

saveRDS(dat3, here('../Data', 'PCdrugs_recs.rds')) #save to file

dat4 <- dat3 %>% select(-eventdate) %>% group_by(across(-cost)) %>% #remove eventdate and group by everything except the cost
  summarise(countrecs = n(), drugcost = sum(cost, na.rm = TRUE)) #summarise to get total cost by patid and drugsubstance
  #NB: countrecs stores the count (prescriptions) of the specific drug substance, but is not required by ICCONIC

saveRDS(dat4, here('../Data', 'PCdrugs.rds')) #save to file


#_________________________

#Create summary stats ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) %>% filter(inhip | inheart | indem)
pcvisits <- readRDS(here('../Data', 'PCvisits.rds'))
pcdrugs <- readRDS(here('../Data', 'PCdrugs.rds'))
source(here('R', 'Output_functions.R'))

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)
heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)


#Hip cohort
hipstats1 <- stats_func(hip_cohort, pcvisits, 'cost', 'hippost', 'hip_agegroup', hipcats)
hipstats2 <- stats_func(hip_cohort, pcvisits, 'cost', 'hippost', 'hip_agegroup', hipcats, extra_cat = 'role_cat')
hipstats3 <- stats_func(hip_cohort, pcdrugs, 'drugcost', 'hippost', 'hip_agegroup', hipcats)

write_csv(hipstats1, here('../Outputs', 'hip_pc_visits_stats1.csv'))
write_csv(hipstats2, here('../Outputs', 'hip_pc_visits_stats2.csv'))
write_csv(hipstats3, here('../Outputs', 'hip_pc_drugs_stats1.csv'))


#Heart and diabetes cohort
heartstats1 <- stats_func(heartd_cohort, pcvisits, 'cost', 'heartpost', 'heart_agegroup')
heartstats2 <- stats_func(heartd_cohort, pcvisits, 'cost', 'heartpost', 'heart_agegroup', hip_cats = NULL, extra_cat = 'role_cat')
heartstats3 <- stats_func(heartd_cohort, pcdrugs, 'drugcost', 'heartpost', 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'heartd_pc_visits_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'heartd_pc_visits_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'heartd_pc_drugs_stats1.csv'))

#__________________

#Lookback year ----

hipstats1 <- stats_func(hip_cohort, pcvisits, 'cost', 'hippre', 'hip_agegroup', hipcats)
hipstats2 <- stats_func(hip_cohort, pcvisits, 'cost', 'hippre', 'hip_agegroup', hipcats, extra_cat = 'role_cat')
hipstats3 <- stats_func(hip_cohort, pcdrugs, 'drugcost', 'hippre', 'hip_agegroup', hipcats)

write_csv(hipstats1, here('../Outputs', 'lookback_hip_pc_visits_stats1.csv'))
write_csv(hipstats2, here('../Outputs', 'lookback_hip_pc_visits_stats2.csv'))
write_csv(hipstats3, here('../Outputs', 'lookback_hip_pc_drugs_stats1.csv'))

heartstats1 <- stats_func(heartd_cohort, pcvisits, 'cost', 'heartpre', 'heart_agegroup')
heartstats2 <- stats_func(heartd_cohort, pcvisits, 'cost', 'heartpre', 'heart_agegroup', hip_cats = NULL, extra_cat = 'role_cat')
heartstats3 <- stats_func(heartd_cohort, pcdrugs, 'drugcost', 'heartpre', 'heart_agegroup')

write_csv(heartstats1, here('../Outputs', 'lookback_heart_pc_visits_stats1.csv'))
write_csv(heartstats2, here('../Outputs', 'lookback_heart_pc_visits_stats2.csv'))
write_csv(heartstats3, here('../Outputs', 'lookback_heart_pc_drugs_stats1.csv'))

#_____________________________________

#Additional work on heart cohorts ----

heartstats_all <- stats_func(heart_cohort, pcvisits, 'cost', 'heartpost', 'heart_agegroup')
heartstats_dc <- stats_func(heartdc_cohort, pcvisits, 'cost', 'heartpost', 'heart_agegroup')
heartstats_nodc <- stats_func(heartnodc_cohort, pcvisits, 'cost', 'heartpost', 'heart_agegroup')

write_csv(heartstats_all, here('../Outputs', 'heart_pc_visits_stats.csv'))
write_csv(heartstats_dc, here('../Outputs', 'heartdc_pc_visits_stats.csv'))
write_csv(heartstats_nodc, here('../Outputs', 'heartnodc_pc_visits_stats.csv'))

#_______________________________________________________

#Additional work on cumulative statistics by decile ----

dec_hipall_visits <- decile_func(hip_cohort, pcvisits, 'hippost', 'cost')
dec_heartd_visits <- decile_func(heartd_cohort, pcvisits, 'heartpost', 'cost')

dec_hipall_drugs <- decile_func(hip_cohort, pcdrugs, 'hippost', 'drugcost')
dec_heartd_drugs <- decile_func(heartd_cohort, pcdrugs, 'heartpost', 'drugcost')

rbind(dec_hipall_visits, dec_heartd_visits, dec_hipall_drugs, dec_heartd_drugs) %>% #bind the results together
  write_csv(here('../Outputs', 'pc_visits_drugs_deciles.csv')) #write to file









