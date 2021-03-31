#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Process inpatient data and use HRG grouper to create indicative costs for spells
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----
pkgs <- c('tidyverse', 'readxl', 'here')
sapply(pkgs, require, character.only = TRUE)

#__________________________________

# Prepare data for HRG grouper ----

										  
																						  
usedate <- 'cipstart' #this version includes all episodes where cips starts in follow-up period
																																

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in data
APCepis <- readRDS(here('../Data', 'APCepis.rds'))  
APCcipsIDs <- readRDS(here('../Data', 'APCcipsIDs.rds'))
APCcipsinfo <- readRDS(here('../Data', 'APCcipsinfo.rds'))
APCspells <- readRDS(here('../Data', 'APCspells.rds'))
APCcips <- readRDS(here('../Data', 'APCcips.rds'))
														  
														

APCcipsepis <- APCepis %>% inner_join(APCcipsIDs) %>% #join cips identifiers onto episodes
  inner_join(cohorts) %>% inner_join(APCcipsinfo) %>% #join cohort and APCcipsinfo onto episodes
  mutate(procodet = 'RJ1', #set procodet to random trust (does not affect grouping), 
         rehabilitationdays = 0, #set rehab days to zero (we are not analysing these)
         spcdays = ifelse(tretspef == 315 & (mainspef %in% c(315, 950, 960)), epidur, 0), #create palliative care days variable
         neocare = 8, #neonatal care not relevant for our study, so set to 8 ('not applicable')
         heartpre = get(usedate) >= (heart - 365) & get(usedate) < heart, #add indicators for cohort pre/post relevance...
         heartpost = get(usedate) < (heart + 365) & get(usedate) >= heart,
         hippre = get(usedate) >= (hip - 365) & get(usedate) < hip,
         hippost = get(usedate) < (hip + 365) & get(usedate) >= hip,
         dempre = get(usedate) >= (dementia - 365) & get(usedate) < dementia,
         dempost = get(usedate) < (dementia + 365) & get(usedate) >= dementia)

#APCcipsepis %>% mutate(check = epidur == (epiend - epistart)) %>% group_by(check) %>% tally #check epidur values

APCccare <- readRDS(here('../Data', 'APCccare.rds')) %>% #read in critical care data
  mutate(criticalcaredays = ccdisdate - ccstartdate + 1) #create a critical care duration variable (need to add 1 - see grouper manual)

APCprocs <- readRDS(here('../Data', 'APCprocs.rds')) %>% #read in procedures
  select(patid, spno, epikey, epistart, OPCS, p_order) %>% #keep required fields
  pivot_wider(names_from = p_order, names_prefix = 'OPER_', values_from = OPCS) #spread the operations to wide format

APCdiags <- readRDS(here('../Data', 'APCdiags.rds')) %>% #read in diagnoses
  mutate(ICD = str_replace_na(ICD, ''), #remove any NAs from ICD
         ICD = str_replace_all(ICD, '[[:punct:]]', ''), #remove all punctuation from ICD
         ICD = paste0(ICD, 'X'), #add an 'X' place holder to account for ICDs that have fewer than 4 characters (e.g. I10)
         ICD = str_sub(ICD, 1, 4)) %>% #trim ICD to 4 characters (e.g. J440X becomes J440)
  select(patid, spno, epikey, epistart, d_order, ICD) %>% #keep required fields
  pivot_wider(names_from = d_order, names_prefix = 'DIAG_', values_from = ICD) #reshape by pivoting wide


#Join everything together ----
dat <- list(APCcipsepis, APCccare, APCprocs, APCdiags) %>% #list the tables to join
  reduce(left_join) %>% #use reduce to achieve multiple joins onto cohorts
  mutate(year = as.integer(format(admidate, '%Y')), #add a year as integer
         month = as.integer(format(admidate, '%m')), #add a month as integer
         HRGyear = ifelse(month <= 3, year - 1, year), #work out the HRG year (e.g. 2014 implies 2014/15)
         startage =  HRGyear - yob) %>% #add startage of patient
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) %>% #filter out rows with TRUE in any cohort pre/post identifiers
  select(procodet, provspno = spno, epiorder = eorder, startage, sex = gender, classpat, #select variables needed for HRG groupers (rename where needed)
         admisorc, admimeth, disdest, dismeth, epidur, mainspef, neocare, tretspef, starts_with('DIAG'), starts_with('OPER'), 
         criticalcaredays, rehabilitationdays, spcdays, HRGyear, patid, ends_with('pre'), ends_with('post')) %>% #keep HRGyear, patid and cohort identifiers at end
  mutate_at(vars(criticalcaredays, rehabilitationdays, spcdays), ~ replace(., is.na(.), 0)) %>% #replace any missing day count values with zero
  arrange(patid, provspno, epiorder) #sort the data

names(dat) <-  str_to_upper(names(dat)) %>% #change names to upper case for grouper
  ifelse(str_sub(., -2, -2)=='_', str_replace(., '_', '_0'), .) #rename DIAG/OPER variables to get leading zeroes (e.g. OPER_1 to OPER_01)


#Write HRG grouper input files ----
dat %>% group_by(HRGYEAR) %>% do(write_csv(., here('../Data', paste0('APC_HRG_grouper_input_', unique(.$HRGYEAR),'.csv')), na = ''))

#Check number of spells per year to compare with HRG grouper output later
dat %>% group_by(HRGYEAR) %>% select(PROVSPNO) %>% distinct() %>% tally()


#Write RDF file for HRG grouper ----
nms <- names(dat)
rdf <- c('COMMA DELIMITED\r', paste(nms, nms, 1:length(nms), '', '', '', '\r', sep = ','))
write_lines(rdf, here('../Data', 'APC.rdf'))


#[WP: At this point I ran all of the HRG groupers for each of the years of data from 2013/14 to 2017/18]

#Performance of HRG groupers
#2013: ~2% errors
#2014: ~1% errors
#2015: ~2% errors
#2016: ~2% errors
#2017: ~2% errors

#___________________________________________

#Read in HRG grouper spell output files ----

#Create vector to contain col_names
col_nms <- c('RowNo','PROCODET','PROVSPNO','SpellHRG','SpellGroupingMethodFlag','SpellDominantProcedure','SpellPDiag','SpellSDiag',
             'SpellEpisodeCount','SpellLOS','ReportingSpellLOS','SpellTrimpoint','SpellExcessBeddays','SpellCCDays','SpellPBC',
             'SpellSSC_Ct','SpellSSC1','SpellSSC2','SpellSSC3','SpellSSC4','SpellSSC5','SpellSSC6','SpellSSC7','SpellBP_Ct','SpellBP1',
             'SpellBP2','SpellBP3','SpellBP4','SpellBP5','SpellBP6','SpellBP7','SpellFlag_Ct','SpellFlag1','SpellFlag2','SpellFlag3',
             'SpellFlag4','SpellFlag5','SpellFlag6','SpellFlag7')
ub_nms <- paste0('UnbundledHRGs', 1:25) #create 25 unbundled HRG column names (HRG grouper does not create headers for these in outputs)
cols <- c(col_nms, ub_nms) #join the vectors together
coltypes <- paste(c('iciccccciiiiii', rep('c', 50)), collapse = '') #create vector of column types

#List HRG grouper spell output files and read in (NB: we saved grouper output to a folder called 'HRG_grouper_output' in the Data folder)
files <- list.files(here('../Data', 'HRG_grouper_output'), pattern = 'spell.csv', full.names = TRUE) #list HRG output files at spell level
years <- str_extract(files, '_...._spell') %>% str_sub(2,5) #get HRG years

#NB: ignore warnings on running read_csv; this is just because unbundled columns do not have much data and no headers
HRGoutput <- files %>% map(vroom, delim = ',', skip = 1, col_types = coltypes, col_names = cols) #read in HRG grouper output (spell level files) replacing the column names
names(HRGoutput) <- years #name the datasets using their years

HRGoutput <- HRGoutput %>% bind_rows(.id = 'HRGyear') %>% rename(spno = PROVSPNO) #bind each year of data together and rename PROVSPNO
saveRDS(HRGoutput, here('../Data', 'APC_HRGoutput_cips.rds')) #save to file

#________________________________

#Cost the grouped APC spells ----

usedate <- 'cipstart' #we costed CIPS where the start date of the CIPS falls in the patients' follow-up periods
APCcips <- readRDS(here('../Data', 'APCcips.rds')) #read in data...
cohorts <- readRDS(here('../Data', 'cohorts.rds'))
HRGoutput <- readRDS(here('../Data', 'APC_HRGoutput_cips.rds'))
HRGcosts <- readRDS(here('Reference_data', 'HRG_reference_costs', 'HRGcosts.rds'))


#Characterise type of admission using spell data (admimeth and classpat)
APCcatspells <- APCcips %>% inner_join(cohorts) %>% #join on cohorts info
  mutate(admin_cat = as.numeric(str_sub(admimeth,1,1)), #create admission category from admimeth
         elective = (classpat == 1 & admin_cat == 1), #categorise the admissions...
         emergency = (classpat == 1 & admin_cat == 2),
         day_case = (classpat == 2),
         regular = (classpat %in% c(3, 4)),
         maternity = (classpat == 5),
         other = (!elective & !emergency & !day_case & !regular & !maternity), #catch-all for uncategorised spells
         heartpre = get(usedate) >= (heart - 365) & get(usedate) < heart, #add indicators for cohort pre/post relevance...
         heartpost = get(usedate) < (heart + 365) & get(usedate) >= heart,
         hippre = get(usedate) >= (hip - 365) & get(usedate) < hip,
         hippost = get(usedate) < (hip + 365) & get(usedate) >= hip,
         dempre = get(usedate) >= (dementia - 365) & get(usedate) < dementia,
         dempost = get(usedate) < (dementia + 365) & get(usedate) >= dementia) %>%
  filter(heartpre | heartpost | hippre | hippost | dempre | dempost) #filter out rows with TRUE in cohort identifiers

mainHRGcosts <- HRGcosts %>% 
  filter(type %in% c('EL','EL_XS','NES','NEL','NEL_XS','DC','RP','TotalHRGs')) %>% #filter out relevant costs
  pivot_wider(id_cols = c(HRGyear, HRG), names_from = type, values_from = unitcost) #make wide format

mainHRG <- HRGoutput %>% #get HRG grouper output
  select(HRGyear, spno, HRG = SpellHRG, SpellLOS, SpellExcessBeddays, SpellGroupingMethodFlag) %>% #select relevant columns
  right_join(APCcatspells) %>% filter(SpellGroupingMethodFlag != 'U') %>% #join to spell info and remove records where grouping failed
  left_join(mainHRGcosts) %>% #join on main HRG costs (for the spell HRG)
  replace_na(list(TotalHRGs=0, EL=0, EL_XS=0, NEL=0, NEL_XS=0, NES=0, DC=0, RP=0)) %>% #make any NA values zeroes
  mutate(maincost = case_when( #depending on categorisation of spell...
    elective ~ EL + (SpellExcessBeddays * EL_XS), #calculate main spell HRG cost...
    emergency & SpellLOS == 0 ~ NES + (SpellExcessBeddays * NEL_XS),
    emergency & SpellLOS > 0 ~ NEL + (SpellExcessBeddays * NEL_XS),
    day_case ~ DC,
    regular ~ RP,
    maternity ~ TotalHRGs, #use average cost for maternity
    other ~ TotalHRGs, #use average cost for any others
  ))

#Failed spell groupings: ~5,000 (~3%)
#~12,000 uncosted records out of ~180,000 successfully grouped (~7%)
#...of these, ~7,000 were main HRGs that had no cost in the HRG costs data (~4%)
#~5,000 were in the HRG costs data, but had no cost (zero cost or a blank cell) (~3%)

#___________________________

#Costing unbundled HRGs ----

#Get the unbundled HRG grouper output in long format
unbundled <- HRGoutput %>% select(HRGyear, spno, UnbundledHRGs1:UnbundledHRGs25) %>% #select unbundled HRGs
  pivot_longer(cols = starts_with('UnbundledHRGs'), values_to = 'HRGcode', values_drop_na = TRUE) %>% #make dataset long
  separate(HRGcode, c('HRG', 'multiplier'), sep = '\\*', fill = 'right') %>% #separate out the multiplier into another column
  mutate(multiplier = ifelse(is.na(multiplier), 1, multiplier), #ensure every row has a multiplier of at least 1
         multiplier = as.numeric(str_sub(multiplier, 1, 1))) %>% #make numeric, assume typos where higher than 10 (debateable)
  left_join(APCcatspells %>% select(patid, spno, day_case, regular)) %>% #join on daycase and regular identifiers
  mutate(daycasereg = day_case | regular) #create indicator that daycasereg version of unbundled cost should be used

#Check which HRG codes are unbundled
# unbundled %>% group_by(HRG) %>% tally() %>% write_csv(here('../Outputs', 'APCunbundledHRGs.csv'))



#Unbundled types that can be costed
# c('CHEM', 'RAD', 'RENAL', 'SPAL', 'HCD')
# cannot cost unbundled...
# - REHAB: they are classified by 'rehab level' in the HRG costs
# - IMAG: no inpatient classified reference costs
# - NM: no inpatient classified reference costs
# - CC: costed by critical care days, and there are no unbundled CC HRGs in grouper output

# CHEM - Service Description: Daycase and Regular Day/Night; Inpatient
# RAD - Service Description: Daycase and Regular Day/Night; Inpatient
# RENAL - Service Description: Renal Dialysis at Base
# SPAL (relevant HRG codes are unique to inpatients)
# HCD - Service Description: Admitted patient care

HRGcosts_unbundled <- HRGcosts %>% 
  filter((type == 'CHEM' & str_detect(servicedescription, 'Daycase and Reg|Inpatient')) |
           (type == 'RAD' & str_detect(servicedescription, 'Daycase and Reg|Inpatient')) |
           (type == 'RENAL' & str_detect(servicedescription, 'Renal .ialysis at .ase')) |
           (type == 'SPAL' & str_detect(servicedescription, 'Daycase and Reg|Inpatient')) |
           (type == 'HCD' & str_detect(servicedescription, 'Admitted'))) %>%
  mutate(daycasereg = str_detect(servicedescription, 'Daycase and Reg')) %>% #add identifier for costs with two categories
  right_join(unbundled) %>% #join costs onto unbundled HRGs from grouper
  mutate(ubcost = unitcost * multiplier) #multiply unit cost by unbundled multiplier



#Check which HRGs are not matched to costs
# HRGcosts_unbundled %>% filter(is.na(ubcost)) %>% group_by(HRG) %>% tally() %>% arrange(desc(n)) %>% write_csv(here('../Outputs', 'check_unbundled_costs.csv'))


#______________________

#Sum costs by cips ----

deflators <- readRDS(here('Reference_data', 'deflators.rds')) #read in deflators

cips_maincosts <- mainHRG %>% left_join(deflators) %>% #join deflators onto main HRG costs
  mutate(maincost_defl = def_mult * maincost) %>% #multiply to get costs in 2016/17 prices
  group_by(patid, cips, cipstart, cipend, gender, dod, hip, hip_cat, hip_agegroup, #group by all the cips and cohort variables
           heart, heart_agegroup, dementia, dem_agegroup, hippre, hippost, heartpre, heartpost, dempre, dempost) %>% #and then...
  summarise(maincost = sum(maincost_defl, na.rm = TRUE)) #summarise by summing up the deflated main HRG costs
  #NB: this grouping is because we wanted all our costs to be at the patient-CIPS level

cips_ubcosts <- HRGcosts_unbundled %>% left_join(deflators) %>% #join the deflators onto the unbundled costs
  mutate(ubcost_defl = def_mult * ubcost) %>% #multiply to get costs in 2016/17 prices
  inner_join(APCcipsIDs) %>% #join on cips identifiers
  group_by(patid, cips) %>% #group by patid and cips, and then...
  summarise(ubcost = sum(ubcost_defl, na.rm = TRUE)) #summarise by summing up the deflated unbundled costs

costs <- cips_maincosts %>% left_join(cips_ubcosts) %>% #join the unbunbled costs onto the main HRG costs
  mutate(totcost = sum(maincost, ubcost, na.rm = TRUE), #create a total cost column
         cipnights = as.numeric(cipend - cipstart), #add cips nights
         cipdays = cipnights + 1) #add cips duration in inclusive days
         
saveRDS(costs, here('../Data','APCcosts.rds')) #save to file


#_________________________

#Create summary stats ----

cohorts <- readRDS(here('../Data', 'cohorts.rds'))
costs <- readRDS(here('../Data', 'APCcosts.rds'))
hipindexcosts <- costs %>% filter(cipstart == hip) #get index cips only
heartindexcosts <- costs %>% filter(cipstart == heart) #get index cips only
source(here('R', 'Output_functions.R')) #load up stats output creation and formatting function

#Set cohorts to use
hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin') #hip cats to include
hip_cohort <- cohorts %>% filter(inhip, hip_cat %in% hipcats)
heartd_cohort <- cohorts %>% filter(inheartd)


#Hip stats

#Acute costs and cips
hipstats1 <- stats_func(hip_cohort, costs, 'totcost', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats1, here('../Outputs', 'hip_acutecost_stats1.csv'))

#Acute days in hospital (use duration var as 'costvar' in function)
hipstats2 <- stats_func(hip_cohort, costs, 'cipdays', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats2, here('../Outputs', 'hip_acutedays_stats1.csv'))

#Acute nights in hospital (use duration var as 'costvar' in function)
hipstats3 <- stats_func(hip_cohort, costs, 'cipnights', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats3, here('../Outputs', 'hip_acutenights_stats1.csv'))

#Acute costs in index hospitalisation
hipstats4 <- stats_func(hip_cohort, hipindexcosts, 'totcost', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats4, here('../Outputs', 'hip_acutecostindex_stats1.csv'))

#Acute days in index hospitalisation (use duration var as 'costvar' in function)
hipstats5 <- stats_func(hip_cohort, hipindexcosts, 'cipdays', 'hippost', 'hip_agegroup', hipcats)
write_csv(hipstats5, here('../Outputs', 'hip_acuteindexdays_stats1.csv'))


#Heart stats

#Acute costs and cips
heartstats1 <- stats_func(heartd_cohort, costs, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats1, here('../Outputs', 'heartd_acutecost_stats1.csv'))

#Acute days in hospital (use duration var as 'costvar' in function)
heartstats2 <- stats_func(heartd_cohort, costs, 'cipdays', 'heartpost', 'heart_agegroup')
write_csv(heartstats2, here('../Outputs', 'heartd_acutedays_stats1.csv'))

#Acute nights in hospital (use duration var as 'costvar' in function)
heartstats3 <- stats_func(heartd_cohort, costs, 'cipnights', 'heartpost', 'heart_agegroup')
write_csv(heartstats3, here('../Outputs', 'heartd_acutenights_stats1.csv'))

#Acute costs in index hospitalisation
heartstats4 <- stats_func(heartd_cohort, heartindexcosts, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats4, here('../Outputs', 'heartd_acute_cost_index_cips.csv'))

#Acute days in index hospitalisation (use duration var as 'costvar' in function)
heartstats5 <- stats_func(heartd_cohort, heartindexcosts, 'cipdays', 'heartpost', 'heart_agegroup')
write_csv(heartstats5, here('../Outputs', 'heartd_acuteindexdays_stats1.csv'))

#________________________

#Lookback year stats ----

#Hip stats

#Acute costs and cips
hipstats1 <- stats_func(hip_cohort, costs, 'totcost', 'hippre', 'hip_agegroup', hipcats)
write_csv(hipstats1, here('../Outputs', 'lookback_hip_acutecost_stats1.csv'))

#Acute days in hospital (use duration var as 'costvar' in function)
hipstats2 <- stats_func(hip_cohort, costs, 'cipdays', 'hippre', 'hip_agegroup', hipcats)
write_csv(hipstats2, here('../Outputs', 'lookback_hip_acutedays_stats1.csv'))

#Acute nights in hospital (use duration var as 'costvar' in function)
hipstats3 <- stats_func(hip_cohort, costs, 'cipnights', 'hippre', 'hip_agegroup', hipcats)
write_csv(hipstats3, here('../Outputs', 'lookback_hip_acutenights_stats1.csv'))


#Heart stats

#Acute costs and cips
heartstats1 <- stats_func(heartd_cohort, costs, 'totcost', 'heartpre', 'heart_agegroup')
write_csv(heartstats1, here('../Outputs', 'lookback_heart_acutecost_stats1.csv'))

#Acute days in hospital (use duration var as 'costvar' in function)
heartstats2 <- stats_func(heartd_cohort, costs, 'cipdays', 'heartpre', 'heart_agegroup')
write_csv(heartstats2, here('../Outputs', 'lookback_heart_acutedays_stats1.csv'))

#Acute nights in hospital (use duration var as 'costvar' in function)
heartstats3 <- stats_func(heartd_cohort, costs, 'cipnights', 'heartpre', 'heart_agegroup')
write_csv(heartstats3, here('../Outputs', 'lookback_heart_acutenights_stats1.csv'))

#_____________________________________________________

#Heart stats with new cohorts for additional work ----

heart_cohort <- cohorts %>% filter(inheart)
heartdc_cohort <- cohorts %>% filter(inheartdc)
heartnodc_cohort <- cohorts %>% filter(inheartnodc)

#Acute costs in index hospitalisation

heartstats6 <- stats_func(heart_cohort, costs, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats6, here('../Outputs', 'heart_acute_cost_index_cips.csv'))

heartstats7 <- stats_func(heartdc_cohort, costs, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats7, here('../Outputs', 'heartdc_acute_cost_index_cips.csv'))

heartstats8 <- stats_func(heartnodc_cohort, costs, 'totcost', 'heartpost', 'heart_agegroup')
write_csv(heartstats8, here('../Outputs', 'heartnodc_acute_cost_index_cips.csv'))

#_______________________________________________________

#Additional work on cumulative statistics by decile ----

#Acute costs
dec_hipall_APCcosts <- decile_func(hip_cohort, costs, 'hippost', 'totcost')
dec_heartd_APCcosts <- decile_func(heartd_cohort, costs, 'heartpost', 'totcost')

#Acute costs in index hospitalisation
dec_hipall_indexAPCcosts <- decile_func(hip_cohort, hipindexcosts, 'hippost', 'totcost')
dec_heartd_indexAPCcosts <- decile_func(heartd_cohort, heartindexcosts, 'heartpost', 'totcost')

rbind(dec_hipall_APCcosts, dec_heartd_APCcosts, dec_hipall_indexAPCcosts, dec_heartd_indexAPCcosts) %>% #bind the results together
  write_csv(here('../Outputs', 'APC_costs_deciles.csv')) #write to file

#Acute days in hospital (use duration var as 'costvar' in function)
dec_hipall_APCdays <- decile_func(hip_cohort, costs, 'hippost', 'cipdays')
dec_heartd_APCdays <-  decile_func(heartd_cohort, costs, 'heartpost', 'cipdays')

#Acute days in index hospitalisation (use duration var as 'costvar' in function)
dec_hipall_indexAPCdays <- decile_func(hip_cohort, hipindexcosts, 'hippost', 'cipdays')
dec_heartd_indexAPCdays <- decile_func(heartd_cohort, heartindexcosts, 'heartpost', 'cipdays')

rbind(dec_hipall_APCdays, dec_heartd_APCdays, dec_hipall_indexAPCdays, dec_heartd_indexAPCdays) %>% #bind the results together
  write_csv(here('../Outputs', 'APC_days_deciles.csv')) #write to file


