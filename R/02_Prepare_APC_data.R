#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Prepare APC data to undertake basic quality checks and identify spells and CIPS
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'here') #package list
sapply(pkgs, require, character.only = TRUE) #load them up

#______________________________________

#Pull in patids and dates of death ----

death <- readRDS(here('../Data', 'death.rds')) %>% select(patid, dod) #get patid and dod (date of death) from ONS death registrations
patient <- readRDS(here('../Data', 'patient.rds')) %>% select(patid) #get patid for all patients in CPRD data
patdod <- left_join(patient, death) #left join so all patids are retained even if they do not have a dod

#_________________________________________

#Create and save checked APC datasets ----

#Limit to greatest extent of date range relevant to study
study_start <- as.Date('2013-04-01') #one year before eligibility start date (for pre-index/lookback year estimates)
study_end <- as.Date('2018-03-31') #one year after eligibility end date (for follow-up estimates)


#Create function to undertake basic checks
prepAPC <- function(type, startdate, enddate){ #function to do some standard steps
  readRDS(here('../Data', paste0(type, '.rds'))) %>% #read in data
    inner_join(patdod) %>% #join onto patient patids to ensure only initial cohort are included and we have the date of death
    filter(.data[[startdate]] >= study_start & .data[[enddate]] <= study_end) %>% #restrict to study-relevant date range
    filter(.data[[enddate]] >= .data[[startdate]]) %>% #check dates makes sense (end date >= start date)
    filter(is.na(dod) | (.data[[startdate]] <= dod & .data[[enddate]] <= dod)) %>% #check dates are not after death
    select(-dod) #remove date of death column
}



#Create checked APC diagnosis dataset
APCdiags <- prepAPC('hes_diagnosis_epi', 'epistart', 'epiend')

#Create checked APC procedures dataset
APCprocs <- prepAPC('hes_proc', 'epistart', 'epiend')

#Create checked APC critical care dataset
APCccare <- prepAPC('hes_ccare', 'ccstartdate', 'ccdisdate')

#Create checked APC episodes (hes_epis) dataset
APCepis <- prepAPC('hes_epis', 'epistart', 'epiend') #date fields are epistart and epiend
#NB: at the moment, this code trims the APCepis to not include those ending after the study end date
#In subsequent code, this means some episodes in spells/CIPS are excluded even if the spell/CIPS starts within the study period																			



#Create spell identifying dataset from episodes (this is based on the usual HES definition of a spell - i.e. grouped episodes)

APCspells <- APCepis %>% filter(eorder == 1) %>% #filter out first episodes in spells
  mutate(spelldur = discharged - epistart) %>% #add spell duration
  filter(spelldur >= 0) %>% #get rid of records with weird or non-existent duration
  filter(admidate == epistart) %>% #keep episodes where the episode starts on the admission start date
  group_by(patid, epistart) %>% #group by patient and episode start date
  arrange(patid, epistart, desc(spelldur)) %>% #sort the data to get longest spell for each patient and start date
  filter(row_number()==1) %>% #extract longest spell starting on that day for that patient
  ungroup() #ungroup the data



#Identify Continuous InPatient Spells (CIPS) 

#These are based on the difference in admission date and previous discharge date for a patient, in combination
#with the discharge destination for a previous discharge. If the latter indicates a transfer, and the difference in
#dates is <= 3 days, the spell is regarded a part of a CIPS (Continuous InPatient Spell)
#NB the resulting 'cips' field is a within-patient identifier - i.e. it MUST be used in combination with the patid

#Create initial CIPS working dataset
APCcips1 <- APCspells %>% arrange(patid, admidate) %>% #sort the spells data
  group_by(patid) %>% #group by patient
  mutate(prevdisdest = lag(disdest, n = 1), #identify the discharge destination for the previous admission
         prevdisdate = lag(discharged, n = 1)) %>% #identify the discharge date for the previous admission
  ungroup() %>% #ungroup the data
  mutate(datediff = admidate - prevdisdate) %>% #calculate days between previous discharge date and current admission date
  mutate(transfer = (prevdisdest %in% c(51, 52, 53) | #identify if any of the codes suggest a transfer occurred
                       admimeth %in% c('2B', '81') | #           (NB: 2B is converted to 67 in Fiona Grimm's HES pipeline on the 
                       admisorc %in% c(51, 52, 53)) & #AND...     THF Github because she changes the string codes into numeric)
           datediff <= 3) %>% #AND the difference in dates of the two spells is <= three days
  mutate(transfer = ifelse(is.na(transfer), FALSE, transfer)) %>% #where row does not have a previous spell, set transfer to FALSE 
  group_by(patid) %>% #regroup by patid
  mutate(cips = cumsum(transfer ==  FALSE)) %>% #create within-patient cips id field: a running count of records that are not transfers by patid (i.e. they are new CIPS) 
  select(patid, cips, spno, transfer, admidate, admimeth, admisorc, disdest, classpat, discharged, spelldur, #select columns to keep
         spellstart = epistart, firstepikey = epikey)

#(The following steps were taking a long time to run, so I split them into separate steps...)
APCcips2 <- APCcips1 %>% group_by(patid, cips) %>% #group by patid and cips
  mutate(cipstart = first(admidate)) #pull out the admission date for each CIPS
APCcips3 <- APCcips2 %>% mutate(cipadmimeth = first(admimeth)) #pull out the admission method for each CIPS
APCcips4 <- APCcips3 %>% mutate(cipadmisorc = first(admisorc)) #pull out the admission source for each CIPS
APCcips5 <- APCcips4 %>% mutate(cipend = last(discharged)) #pull out the discharge date for each CIPS
APCcips6 <- APCcips5 %>% mutate(cipdisdest = last(disdest)) #pull out the discharge destination for each CIPS
APCcips7 <- APCcips6 %>% mutate(cipfirstepikey = first(firstepikey)) #pull out the first episode key for each CIPS
APCcips8 <- APCcips7 %>% ungroup() %>% #ungroup the data
  mutate(cipdur = cipend - cipstart) #add a CIPS duration field

APCcipsIDs <- APCcips8 %>% select(patid, cips, spno) #create a dataset which links the HES spell IDs to the new CIPS IDs for each patient

APCcipsinfo <- APCcips8 %>% #take the CIPS/spells data
  select(patid, cips, cipstart, cipend, cipadmimeth, cipadmisorc, cipdisdest, cipfirstepikey, cipdur) %>% #reduce down to CIPS fields
  group_by(patid, cips) %>% #regroup by patid and cips
  filter(row_number()==1) %>% #get the first row (therefore retaining one record per within-patient CIPS)
  ungroup() #ungroup the data




#Save checked datasets direct from HES
saveRDS(APCepis, here('../Data', 'APCepis.rds')) #APC episodes
saveRDS(APCdiags, here('../Data', 'APCdiags.rds')) #APC diagnoses
saveRDS(APCprocs, here('../Data', 'APCprocs.rds')) #APC procedures
saveRDS(APCccare, here('../Data', 'APCccare.rds')) #APC critical care


#Save specially created datasets
saveRDS(APCspells, here('../Data', 'APCspells.rds')) #APC spells
saveRDS(APCcips8, here('../Data', 'APCcips.rds')) #APC HES spells with CIPS information
saveRDS(APCcipsIDs, here('../Data', 'APCcipsIDs.rds')) #APC within-patient CIPS IDs linked to spell IDs
saveRDS(APCcipsinfo, here('../Data', 'APCcipsinfo.rds')) #APC within-patient CIPS info














