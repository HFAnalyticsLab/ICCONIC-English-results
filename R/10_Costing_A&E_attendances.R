#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Process A&E data and use HRG grouper to create indicative costs for attendances
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----

pkgs <- c('tidyverse', 'readxl', 'here')
sapply(pkgs, require, character.only = TRUE)  

#__________________________________

# Prepare data for HRG grouper ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in the cohorts data

AEatts <- readRDS(here('../Data', 'hesae_att.rds')) %>% #read in the A&E attendances data
  inner_join(cohorts) %>% #join on cohort information
  filter(is.na(dod) | arrivaldate <= dod) %>% #check dates are not after death
  mutate(heartpre = arrivaldate >= (heart - 365) & arrivaldate < heart, #add indicators for cohort pre/post relevance...
         heartpost = arrivaldate < (heart + 365) & arrivaldate >= heart,
				 hippre = arrivaldate >= (hip - 365) & arrivaldate < hip,
				 hippost = arrivaldate < (hip + 365) & arrivaldate >= hip,
				 dempre = arrivaldate >= (dementia - 365) & arrivaldate < dementia,
				 dempost = arrivaldate < (dementia + 365) & arrivaldate >= dementia)

AEinv <- readRDS(here('../Data', 'hesae_inv.rds')) %>% #read in the investigations data
			select(patid, aekey, invest2, invest_order) %>% #select columns
			 pivot_wider(names_from = invest_order, names_prefix = 'INV_', values_from = invest2) #spread the investigations to wide format

AEtreat <- readRDS(here('../Data', 'hesae_treat.rds')) %>% #read in the treatment data
			select(patid, aekey, treat3, treat_order) %>% #select columns
			 pivot_wider(names_from = treat_order, names_prefix = 'TREAT_', values_from = treat3) #spread the investigations to wide format

 
#Join everything together ----

dat <- list(AEatts, AEinv, AEtreat) %>% #list the tables to join
  Reduce(left_join, .) %>% #use Reduce to achieve multiple joins
  mutate(year = as.integer(format(arrivaldate, '%Y')), #add a year as integer
         month = as.integer(format(arrivaldate, '%m')), #add a month as integer
         HRGyear = ifelse(month <= 3, year - 1, year), #work out the HRG year (e.g. 2014 => 2014/15)
         age =  HRGyear - yob, #add age of patient
         recID = paste0(patid, '-', aekey)) %>% #create an ID variable
  select(age, aepatientgroup = aepatgroup, INV_1:INV_12, TREAT_1:TREAT_12, #select variables needed for HRG groupers
         recID, patid, aekey, HRGyear, ends_with('pre'), ends_with('post')) %>% #keep HRGyear, patid and cohort identifiers at end
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) %>% #filter out rows with TRUE in cohort identifiers
  filter(HRGyear %in% 2013:2017) %>% #filter out relevant HRG years
  arrange(HRGyear, patid, aekey) #sort the data

names(dat) <- names(dat) %>% ifelse(str_sub(., -2, -2)=='_', str_replace(., '_', '_0'), .) #rename variables to get leading zeroes

#Write HRG grouper input files ----
dat %>% group_by(HRGyear) %>% #group by HRGyear
	do(write_csv(., here('../Data', paste0('AE_HRG_grouper_input_', unique(.$HRGyear),'.csv')), na = '')) #write to csv (by HRGyear)

#Check number of attendances per year to compare with HRG grouper output later
# dat %>% group_by(HRGyear) %>% select(aekey) %>% distinct() %>% tally()


#Write RDF file for HRG grouper ----
nms <- names(dat)
rdf <- c('COMMA DELIMITED\r', paste(toupper(nms), toupper(nms), 1:length(nms), '', '', '', '\r', sep = ','))
write_lines(rdf, here('../Data', 'AE.rdf'))


#[WP: At this point I ran all of the HRG groupers for each of the years of data from 2013/14 to 2017/18]

#Performance of HRG groupers
#2013: ~1% errors
#2014: ~3% errors
#2015: ~2% errors
#2016: ~3% errors
#2017: ~3% errors


#_______________________________

#Read in HRG grouper output ----

#List HRG grouper output files and read in
files <- list.files(here('../Data', 'HRG_grouper_output'), pattern = 'AE_...._attend.csv', full.names = TRUE) #list HRG output files for attendances
years <- str_extract(files, '_...._') %>% str_sub(2,5) #get HRG years
coltypes <- paste(c('ii', rep('c', 24), 'cincllllllnc'), collapse = '') #store column types

HRGoutput <- files %>% map(read_csv, col_types = coltypes) #read in HRG grouper output
names(HRGoutput) <- years #name the datasets using their years

HRGoutput <- HRGoutput %>% bind_rows(.id = 'HRGyear2') %>% #bind each year of data together
  rename(HRG = EM_HRG) #change the name of the EM_HRG column to HRG
saveRDS(HRGoutput, here('../Data', 'AE_HRGoutput.rds')) #save to file

#_________________________

#Cost A&E attendances ----

HRGoutput <- readRDS(here('../Data', 'AE_HRGoutput.rds')) #read in HRG output data
HRGcosts <- readRDS(here('Reference_data', 'HRG_reference_costs', 'HRGcosts.rds')) #read in HRG costs data
deflators <- readRDS(here('Reference_data', 'deflators.rds')) %>% select(HRGyear, def_mult) #get deflators by year

amb_cost <- HRGcosts %>% filter(type == 'AMB' & str_detect(currencydescription, 'convey')) %>% #get ambulance conveyance costs
  select(HRGyear, ambcost = unitcost) #rename unitcost to ambcost and select columns
  
ae_cost <- HRGcosts %>% filter(type == 'EM') %>%  #get EM costs
  pivot_wider(id_cols = c(HRGyear, HRG), names_from = servicecode, values_from = unitcost) #make wide format by service code

AEcosts <- HRGoutput %>% inner_join(AEatts) %>% inner_join(ae_cost) %>% #join everything together
  inner_join(amb_cost) %>% inner_join(deflators) %>%
  mutate(ambulance = ifelse(aearrivalmode == 1, ambcost, 0), #add ambulance cost where conveyance occurred 
         maincost = case_when( #set main cost depending on aedepttype and aeattenddisp...
           aedepttype == 1 & aeattenddisp != 1 ~ T01NA,
           aedepttype == 1 & aeattenddisp == 1 ~ T01A,
           aedepttype == 2 & aeattenddisp != 1 ~ T02NA,
           aedepttype == 2 & aeattenddisp == 1 ~ T02A,
           aedepttype == 3 & aeattenddisp != 1 ~ T03NA,
           aedepttype == 3 & aeattenddisp == 1 ~ T03A,
           aedepttype == 4 & aeattenddisp != 1 ~ T04NA,
           aedepttype == 4 & aeattenddisp == 1 ~ T04A
         ),
         totcost = (maincost + ambulance) * def_mult) %>% #calculate total cost using deflators
  filter(!is.na(totcost)) #remove records we couldn't cost

saveRDS(AEcosts, here('../Data', 'AEcosts.rds'))

#_________________________

#Create summary stats ----

source(here('R', 'Output_functions.R')) #source output functions for creating summary statistics
cohorts <- readRDS(here('../Data', 'cohorts.rds')) %>% filter(inhip | inheart | indem) #filter out only the valid cohorts
AEcosts <- readRDS(here('../Data', 'AEcosts.rds'))

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)
heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)


#Summary stats for non-admitted A&E attendances

AE_no_hosp <- AEcosts %>% select(patid, aekey, gender, hip_cat, hip_agegroup, #get AEcosts and select columns...
                                 heart_agegroup, dem_agegroup, heartpre:dempost, aeattenddisp, totcost) %>% 
  filter(aeattenddisp != 1) %>% #remove admitted patients
  inner_join(cohorts %>% select(patid, inhip:indem)) #join on cohort information

#Hip patients
hipstats1 <- stats_func(hip_cohort, AE_no_hosp, 'totcost', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats1, here('../Outputs', 'hip_AE_stats.csv'), na = '0')

#Heart patients
heartstats1 <- stats_func(heartd_cohort, AE_no_hosp, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats1, here('../Outputs', 'heart_AE_stats.csv'), na = '0')

#_______________________________________________________________________

#Create summary stats for 'Total Inpatient/Acute Hospital Spending' ----
#i.e. all A&E and all inpatient 

all_AE <- AEcosts %>% ungroup() %>% #get all AEcosts
  select(patid, gender, hip_cat, hip_agegroup, heart_agegroup, dem_agegroup, heartpre:dempost, totcost) %>% #select columns
  inner_join(cohorts %>% select(patid, inhip:indem)) %>% #join cohort information on
  group_by(across(c(everything(), -totcost))) %>%  #group by all the variables except totcost
  summarise(aecost = sum(totcost, na.rm = TRUE)) #re-sum the totcost variable into aecost

APCcosts <- readRDS(here('../Data', 'APCcosts.rds')) %>% ungroup() %>% #get the APCcosts data
  select(-cips, -cipstart, -cipend, -maincost, -ubcost, -cipnights, -cipdays) %>% #remove columns that aren't required
  inner_join(cohorts %>% select(patid, inhip:indem)) %>% #join on cohort information
  group_by(across(c(everything(), -totcost))) %>% #group by all the variables except totcost
  summarise(apccost = sum(totcost, na.rm = TRUE)) #re-sum the totcost variable into apccost

acute <- full_join(all_AE, APCcosts) %>% #join the A&E and inpatient costs together
  mutate(totcost = sum(aecost, apccost, na.rm = TRUE)) #sum the costs


#Hip patients
hipstats2 <- stats_func(hip_cohort, acute, 'totcost', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats2, here('../Outputs', 'hip_all_acute_costs.csv'), na = '0')


#Heart patients
heartstats2_all <- stats_func(heart_cohort, acute, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats2_all, here('../Outputs', 'heart_all_acute_costs.csv'), na = '0')

heartstats2_d <- stats_func(heartd_cohort, acute, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats2_d, here('../Outputs', 'heartd_all_acute_costs.csv'), na = '0')

heartstats2_dc <- stats_func(heartdc_cohort, acute, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats2_dc, here('../Outputs', 'heartdc_all_acute_costs.csv'), na = '0')

heartstats2_nodc <- stats_func(heartnodc_cohort, acute, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats2_nodc, here('../Outputs', 'heartnodc_all_acute_costs.csv'), na = '0')


#__________________

#Lookback year ----


#Hip patients
hipstats1 <- stats_func(hip_cohort, AE_no_hosp, 'totcost', 'hippre', 'hip_agegroup', hipcats)
write_csv(hipstats1, here('../Outputs', 'lookback_hip_AE_stats.csv'), na = '0')

#Heart patients
heartstats1 <- stats_func(heartd_cohort, AE_no_hosp, 'totcost', 'heartpre', 'heart_agegroup')
write_csv(heartstats1, here('../Outputs', 'lookback_heart_AE_stats.csv'), na = '0')


#Create summary stats for 'Total Inpatient/Acute Hospital Spending' ----
#i.e. all A&E and all inpatient 

#Hip patients
hipstats2 <- stats_func(hip_cohort, acute, 'totcost', 'hippre', 'hip_agegroup', hipcats)
write_csv(hipstats2, here('../Outputs', 'lookback_hip_all_acute_costs.csv'), na = '0')

#Heart patients
heartstats2 <- stats_func(heartd_cohort, acute, 'totcost', 'heartpre', 'heart_agegroup')
write_csv(heartstats2, here('../Outputs', 'lookback_heart_all_acute_costs.csv'), na = '0')



