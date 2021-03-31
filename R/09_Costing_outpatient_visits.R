#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Process outpatient data and use HRG grouper to create indicative costs for attendances
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----

pkgs <- c('tidyverse', 'readxl', 'here')
sapply(pkgs, require, character.only = TRUE)

#__________________________________

# Prepare data for HRG grouper ----

HRGyears <- 2013:2017 #set study years for which we want outpatient data
cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohort data

OPapps <- readRDS(here('../Data', 'hesop_app.rds')) %>% #read in outpatient appointments data
  inner_join(cohorts) %>% #join on cohort information
  filter(is.na(dod) | (apptdate <= dod)) %>% #check dates are not after death
  filter(HES_yr %in% HRGyears) %>% #filter out study years
  filter(!attended %in% c(0, 3)) %>% #remove DNAs and future appointments
  mutate(heartpre = apptdate >= (heart - 365) & apptdate < heart, #add indicators for cohort pre/post relevance...
         heartpost = apptdate < (heart + 365) & apptdate >= heart,
         hippre = apptdate >= (hip - 365) & apptdate < hip,
         hippost = apptdate < (hip + 365) & apptdate >= hip,
         dempre = apptdate >= (dementia - 365) & apptdate < dementia,
         dempost = apptdate < (dementia + 365) & apptdate >= dementia) %>%
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) %>% #filter out rows with TRUE in pre/post indicators
  left_join(readRDS(here('../Data', 'hesop_clin_wp.rds'))) #join on clinical information

group_by(OPapps, stafftyp) %>% tally() #check counts of staff types - lots of 99 (NOT KNOWN)

dat <- OPapps %>% mutate(dummy = paste0(patid, '-', attendkey)) %>% #add an ID variable to the data
  select(startage = apptage, sex = gender, mainspef, tretspef, firstatt, #restrict to fields required for the HRG grouper
         opertn_01:opertn_12, dummy, HES_yr) %>% #...keeping the dummy ID and HES_yr columns
  mutate(opertn_01 = ifelse(opertn_01 == '-', NA, opertn_01)) %>% #remove any '-' values in the opertn_01 field
  rename_at(vars(starts_with('opertn')), list(~sub('opertn', 'oper', .))) #rename opertn variables to 'oper'

names(dat) <- toupper(names(dat)) #make names all upper case for the HRG grouper

#Write HRG grouper input files ----
dat %>% group_by(HES_YR) %>% #group by HES_yr
	do(write_csv(., here('../Data', paste0('OP_HRG_grouper_input_', unique(.$HES_YR),'.csv')), na = '')) #write to csv (by HES_yr)

#Write RDF file for HRG grouper ----
nms <- names(dat)
rdf <- c('COMMA DELIMITED\r', paste(nms, nms, 1:length(nms), '', '', '', '\r', sep = ','))
write_lines(rdf, here('../Data', 'OP.rdf'))



#[WP: At this point I ran all of the HRG groupers for each of the years of data from 2013/14 to 2017/18]

#Performance of HRG groupers
#2013: ~ 1% errors
#2014: ~ 4% errors
#2015: ~12% errors - PRETTY BAD!
#2016: ~10% errors - PRETTY BAD!
#2017: ~ 8% errors - PRETTY BAD!

#Check errors in 2015, which was particularly bad
# errors2015 <- read_csv(here('../Data', 'HRG_grouper_output', 'OP_2015_quality.csv')) %>% group_by(`Error Message`) %>% 
#   tally() %>% arrange(desc(n)) 
# View(errors2015)

#___________________________________________

#Read in HRG grouper output files ----

#Store names of HRG output columns in a vector
col_nms <- c('startage','sex','mainspef','tretspef','firstatt','oper_01','oper_02','oper_03','oper_04','oper_05','oper_06','oper_07',
             'oper_08','oper_09','oper_10','oper_11','oper_12','dummy','HES_yr','RowNo','NAC_HRG','GroupingMethodFlag',
             'DominantProcedure','AttendanceHRG','AttendSSC_Ct','AttendSSC1','AttendSSC2','AttendSSC3','AttendSSC4','AttendSSC5',
             'AttendBP_Ct','AttendBP1','AttendBP2','AttendBP3','AttendBP4','AttendBP5','AttendFlag_Ct','AttendFlag1','AttendFlag2',
             'AttendFlag3','AttendFlag4','AttendFlag5')

ub_nms <- paste0('UnbundledHRGs', 1:25) #create 25 unbundled HRG column names (HRG grouper does not create headers for these in outputs)
cols <- c(col_nms, ub_nms) #add unbundled column names to HRG output column names
coltypes <- paste(c('iiici', rep('c', 12), c('ciiccc'), rep('i', 19), rep('c', 25)), collapse = '') #set the types for the columns and store as vector


#List HRG grouper spell output files and read in

files <- list.files(here('../Data', 'HRG_grouper_output'), pattern = 'OP_...._attend.csv', full.names = TRUE) #list HRG output files for attendances
years <- str_extract(files, '_...._') %>% str_sub(2,5) #get HRG years

#NB: ignore warnings on running read_csv; this is just because unbundled columns often do not have much data and no headers
HRGoutput <- files %>% map(read_csv, skip = 1, col_types = coltypes, col_names = cols) #read in HRG grouper output (spell level files) replacing the column names
names(HRGoutput) <- years #name the datasets using their years
HRGoutput <- HRGoutput %>% bind_rows(.id = 'HRGyear') #bind each year of data together
saveRDS(HRGoutput, here('../Data', 'OP_HRGoutput.rds')) #save to file

#________________________________

#Cost the grouped OP spells ----

HRGoutput <- readRDS(here('../Data', 'OP_HRGoutput.rds')) %>% #read in the HRG grouper output RDS file created above
  separate(dummy, c('patid', 'attendkey'), '-') %>% #create separate ID fields (patid, attendkey) from the dummy column
  mutate(patid = as.numeric(patid), attendkey = as.numeric(attendkey)) %>% #format them as numeric (were character)
  select(HRGyear, patid, attendkey, tretspef, NAC_HRG, GroupingMethodFlag, UnbundledHRGs1:UnbundledHRGs25) #select required fields

HRGcosts <- readRDS(here('Reference_data', 'HRG_reference_costs', 'HRGcosts.rds')) #read in HRG costs file

mainHRGcosts <- HRGcosts %>% filter(type %in% c('CL', 'NCL', 'OPROC')) %>% #filter out outpatient-relevant costs
  pivot_wider(id_cols = c(HRGyear, HRG, servicecode), names_from = type, values_from = unitcost) #make wide format

#stafftyp: 3 = lead care professional, 4 = member of professional care team, 99 = unknown
#NB: where stafftyp is 99 we've assumed consultant/lead care professional
#GroupingMethodFlag: O=outpatient default, P=procedure driven, G=global exception, U=Error

OPmain <- OPapps %>% select(patid, attendkey, stafftyp) %>% #get stafftyp from OPapps
  left_join(HRGoutput, by = c('patid', 'attendkey')) %>% #join HRG output onto these
  filter(GroupingMethodFlag != 'U') %>% #remove failed groupings
  left_join(mainHRGcosts, by = c('NAC_HRG' = 'HRG', 'HRGyear', 'tretspef' = 'servicecode')) %>% #join the main HRG costs onto the HRG output
  mutate(across(c(NCL, CL, OPROC), ~ replace_na(.x, 0))) %>% #change any NA values to zero
  mutate(staffcost = ifelse(stafftyp == 4, NCL, CL), #if staff is not a consultant/lead care professional, use NCL cost
         cost = staffcost + OPROC) #sum staff based cost and OPROC (there is only ever one or the other)


#Dealing with unbundled costs ----

# Unbundled costs that are included:
# CHEM - Outpatient
# IMAG - Outpatient
# NM - Outpatient
# HCD - Outpatient
# RAD - Outpatient
# SPC - Outpatient

#Get the unbundled HRG grouper output in long format
unbundled <- HRGoutput %>% select(HRGyear, patid, attendkey, UnbundledHRGs1:UnbundledHRGs25) %>% #select unbundled HRGs
  pivot_longer(cols = starts_with('UnbundledHRGs'), values_to = 'HRGcode', values_drop_na = TRUE) %>% #make dataset long
  separate(HRGcode, c('HRG', 'multiplier'), sep = '\\*', fill = 'right') %>% #separate out the multiplier into another column
  mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier), #ensure every row has a multiplier of at least 1
         multiplier = as.numeric(str_sub(multiplier, 1, 1))) #assume typos, where higher than 10 (debateable)

HRGcosts_unbundled <- HRGcosts %>% #get HRG costs
  filter(type %in% c('CHEM', 'IMAG', 'NM', 'HCD', 'RAD', 'SPC') & str_detect(servicedescription, 'Outpat')) %>% #filter out outpatient unbundled types 
  right_join(unbundled) %>% #join onto the unbundled grouper output
  mutate(ubcost = unitcost * multiplier) %>% #multiply unit cost by unbundled multiplier
  group_by(HRGyear, patid, attendkey) %>%  #group by the year, patid and attendance ID and...
  summarise(ubcost = sum(ubcost, na.rm = TRUE)) #sum total unbundled costs by these groups


#Cost the outpatient attendances ----

deflators <- readRDS(here('Reference_data', 'deflators.rds'))

OPcosts <- OPapps %>% select(HES_yr, patid, attendkey, apptdate, gender, dod, hip, hip_cat, heart, dementia, #get required columns of outpatient appointments
                             heartpre:dempost, hip_agegroup, heart_agegroup, dem_agegroup, mainspef) %>% 
  left_join(OPmain) %>% left_join(HRGcosts_unbundled) %>% left_join(deflators) %>% #join on the main HRG costs, the unbundled costs and the deflators
  mutate(across(c(ubcost, cost), ~ replace_na(.x, 0))) %>% #change any NA values to zeroes
  mutate(totcost = (ubcost + cost) * def_mult) %>% #calculate total cost (2016/17 prices) of attendance by summing and multiplying by deflator
  select(patid:GroupingMethodFlag, totcost) #reduce the dataset size

saveRDS(OPcosts, here('../Data', 'OPcosts.rds'))

#_________________________

#Create summary stats ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) %>% filter(inhip | inheart | indem) #filter out only the valid cohorts
OPbuckets <- read_excel(here('Reference_data', 'Outpatient_specialty_buckets.xlsx')) #read in the categorised specialties (based on 'mainspef')
OPcosts <- readRDS(here('../Data', 'OPcosts.rds')) %>% left_join(OPbuckets) %>% inner_join(cohorts %>% select(patid, inhip:indem)) #join everything together
source(here('R', 'Output_functions.R')) #source the output functions for creating summary statistics as required by the ICCONIC team

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)
heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)


#Hip cohort
hipstats1 <- stats_func(hip_cohort, OPcosts, 'totcost', 'hippost', 'hip_agegroup', hipcats, extra_cat = 'category') #create summary statistics by categorised specialties
write_csv(hipstats1, here('../Outputs', 'hip_op_stats1.csv'), na = '0') #write to file

hipstats1 <- stats_func(hip_cohort, OPcosts, 'totcost', 'hippost', 'hip_agegroup', hipcats) #create summary statistics 
write_csv(hipstats2, here('../Outputs', 'hip_op_stats2.csv'), na = '0') #write to file


#Heart cohort 
heartstats1 <- stats_func(heartd_cohort, OPcosts, 'totcost', 'heartpost', 'heart_agegroup', hip_cats = NULL, extra_cat = 'category') #create summary statistics by categorised specialties
write_csv(heartstats1, here('../Outputs', 'heart_op_stats1.csv'), na = '0') #write to file

heartstats2 <- stats_func(heartd_cohort, OPcosts, 'totcost', 'heartpost', 'heart_agegroup') #create summary statistics
write_csv(heartstats2, here('../Outputs', 'heart_op_stats2.csv'), na = '0') #write to file

heartstats3 <- stats_func(heart_cohort, OPcosts, 'totcost', 'heartpost', 'heart_agegroup') #create summary statistics
write_csv(heartstats3, here('../Outputs', 'heart_op_stats3.csv'), na = '0') #write to file

heartstats4 <- stats_func(heartdc_cohort, OPcosts, 'totcost', 'heartpost', 'heart_agegroup') #create summary statistics
write_csv(heartstats4, here('../Outputs', 'heart_op_stats4.csv'), na = '0')

heartstats5 <- stats_func(heartnodc_cohort, OPcosts, 'totcost', 'heartpost', 'heart_agegroup') #create summary statistics
write_csv(heartstats5, here('../Outputs', 'heart_op_stats5.csv'), na = '0') #write to file

#__________________

#Lookback year ----

#Hip cohort
hipstats1 <- stats_func(hip_cohort, OPcosts, 'totcost', 'hippre', 'hip_agegroup', hipcats, extra_cat = 'category') #create summary statistics by categorised specialties
write_csv(hipstats1, here('../Outputs', 'lookback_hip_op_stats1.csv'), na = '0') #write to file

hipstats2 <- stats_func(hip_cohort, OPcosts, 'totcost', 'hippre', 'hip_agegroup', hipcats) #create summary statistics
write_csv(hipstats2, here('../Outputs', 'lookback_hip_op_stats2.csv'), na = '0') #write to file


#Heart cohort 
heartstats1 <- stats_func(heartd_cohort, OPcosts, 'totcost', 'heartpre', 'heart_agegroup', hip_cats = NULL, extra_cat = 'category') #create summary statistics by categorised specialties
write_csv(heartstats1, here('../Outputs', 'lookback_heart_op_stats1.csv'), na = '0') #write to file

heartstats2 <- stats_func(heartd_cohort, OPcosts, 'totcost', 'heartpre', 'heart_agegroup') #create summary statistics
write_csv(heartstats2, here('../Outputs', 'lookback_heart_op_stats2.csv'), na = '0') #write to file


#_______________________________________________________

#Additional work on cumulative statistics by decile ----

x <- OPcosts %>% filter(category %in% c('1_MD specialist', '2_Outpatient Diagnostic MD'))
dec_hipall_opcosts <- decile_func(hip_cohort, x, 'hippost', 'totcost')
dec_heartd_opcosts <- decile_func(heartd_cohort, x, 'heartpost', 'totcost')

rbind(dec_hipall_opcosts, dec_heartd_opcosts) %>% #bind the results together
  write_csv(here('../Outputs', 'op_costs_deciles.csv')) #write to file

