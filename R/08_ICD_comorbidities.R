#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Create Elixhauser and Cambridge comorbidity scores for hip and heart/diabetes cohorts
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages and source folder locations ----

pkgs <- c('tidyverse', 'readxl', 'here')
sapply(pkgs, require, character.only = TRUE)

#__________________________________________________

#Create ICD list for identifying comorbidities ----
#This and the source file are a bit messy because they have been converted from previous work in SAS
#which involved the Charlson and Cambridge comorbidity codings too. Can definitely improve both

ICDcomorb1 <- read_excel(here('Reference_data', 'Comorbidities.xlsx'), sheet = 'list1') %>% 
  separate(condition, into = paste0('cond', 1:5), sep=', ', remove = TRUE, fill = 'right') %>% 
  pivot_longer(cond1:cond5, names_to = 'num', names_prefix = 'cond', values_to = 'condition', values_drop_na = TRUE)

ICDcomorb2 <- read_excel(here('Reference_data', 'Comorbidities.xlsx'), sheet = 'list2') %>% 
  separate(ICD, into = paste0('ICD', 1:20), sep=', ', remove = TRUE, fill = 'right') %>% 
  pivot_longer(ICD1:ICD20, names_to = 'num', names_prefix = 'ICD', values_to = 'ICD', values_drop_na = TRUE)

ICDcomorbid <- bind_rows(ICDcomorb1, ICDcomorb2) %>% select(ICD, condition)

#___________________________________________________________________________

#Prepare comorbidity flags and calculate Elixhauser Comorbidities count ----

cohorts <- readRDS(here('../Data', 'cohorts.rds')) #read in cohorts data

APCcipsIDs <- readRDS(here('../Data', 'APCcipsIDs.rds')) #read in CIPS IDs

APCdiags <- readRDS(here('../Data', 'APCdiags.rds')) %>% #read in diagnoses
  mutate(ICD = str_replace_na(ICD, ''), #remove any NAs from ICD
         ICD = str_replace_all(ICD, '[[:punct:]]', '')) %>% #remove all punctuation from ICD
  select(patid, spno, ICD) %>% inner_join(APCcipsIDs) #select cols and join on CIPS IDs

AllConds <- unique(ICDcomorbid$condition) %>% #identify unique conditions that are in ICD list
  set_names(data.frame(matrix(nrow = 0, ncol = length(.))), .) #create empty data frame of conditions for binding later

#Function for flagging conditions and creating Elixhauser Comorbidities count
prepComorbid <- function(cips_in, cohort_flag){

  cohorts %>% select(patid, .data[[cohort_flag]], .data[[cips_in]]) %>% rename(cips = !!cips_in) %>% #select required cohort and rename hip/heart cips variable
    filter(!is.na(cips), .data[[cohort_flag]]) %>% inner_join(APCdiags, by = c('patid', 'cips')) %>% #filter relevant cohort out and join on APC diagnoses
    mutate(ICD2 = str_sub(ICD, 1, 2), #get two character version of ICD
           ICD3 = str_sub(ICD, 1, 3)) %>% #get three character version of ICD
    left_join(ICDcomorbid, by = c('ICD' = 'ICD')) %>% #join on matching ICD codes... (3 formats used)
    left_join(ICDcomorbid, by = c('ICD2' = 'ICD')) %>%
    left_join(ICDcomorbid, by = c('ICD3' = 'ICD')) %>%
    select(patid, cips, cond1 = condition, cond2 = condition.x, cond3 = condition.y) %>% #select and rename variables
    pivot_longer(cond1:cond3, names_to = 'num', names_prefix = 'cond', values_to = 'condition', values_drop_na = TRUE) %>% #reshape to long format
    group_by(patid, cips, condition) %>% tally() %>% #tally the conditions by patient, cips and condition
    pivot_wider(id_cols = c(patid, cips), names_from = condition, values_from = n, values_fn = list(n = is_integer), #make wide and change present conditions to TRUE
                values_fill = list(n = FALSE)) %>% #Missing conditions are filled with FALSE
    bind_rows(AllConds) %>% #bind with empty data frame of Elixhauser conds to ensure all conditions are included (even if not in data)
	#NB: there are some additional logic rules to identifying conditions. Talk to Therese if you need more info.
    mutate(bph = ifelse(!bphNOT & bph, TRUE, FALSE)) %>% select(-bphNOT) %>% #deal with unusal rule for bph condition
    mutate(tumournonmets = ifelse(tumourmets, FALSE, tumournonmets), #deal with superceding severity rules for certain conditions...
           diabuncomp = ifelse(diabcomp, FALSE, diabuncomp),
           hypertenuncomp = ifelse(hypertencomp, FALSE, hypertenuncomp)) %>%
    mutate(elix = sum(chf, arrhythmias, valvular, pulmcirc, pvd, hypertenuncomp, hypertencomp, plegia, neuroother, cpd,
             diabuncomp, diabcomp, hypothyroid, renalfail, liver, pepticnonbleed, hiv, lymphoma, tumourmets,
             tumournonmets, rheumarth, coagulopathy, obesity, weightloss, fluid, anaemiabloodloss, anaemiadeficiency,
             alcoabuse, drugabuse, psychoses, depression, na.rm = TRUE)) %>% #add total Elixhauser count
    ungroup() %>%
    inner_join(cohorts %>% select(patid, !!cips_in, gender, hip, heartdiab, hip_cat, hip_age, heart_age, #join required cohort info back on
                                  hip_agegroup, heart_agegroup), by = c('patid', 'cips' = cips_in))
  }

hip <- prepComorbid('hipcips', 'inhip') #run function for hip cohort
heartdiab <- prepComorbid('hdcips', 'inheartd') #run function for heartdiab cohort

saveRDS(hip, here('../Data', 'Elix_hip.rds')) #save data
saveRDS(heartdiab, here('../Data', 'Elix_heartdiab.rds')) #save data

#NB: Mai's original code for Elixhauser also identified Charlson comorbidity flags
#and she then went on to combine with Cambridge Score flags from primary care

#_________________________

#Create summary stats ----

# The conditions in Elixhauser are:
# elix_cols <- c('chf', 'arrhythmias', 'valvular', 'pulmcirc', 'pvd', 'hypertenuncomp', 'hypertencomp', 'plegia', 'neuroother', 'cpd',
#                'diabuncomp', 'diabcomp', 'hypothyroid', 'renalfail', 'liver', 'pepticnonbleed', 'hiv', 'lymphoma', 'tumourmets',
#                'tumournonmets', 'rheumarth', 'coagulopathy', 'obesity', 'weightloss', 'fluid', 'anaemiabloodloss', 'anaemiadeficiency',
#                'alcoabuse', 'drugabuse', 'psychoses', 'depression')

hip <- readRDS(here('../Data', 'Elix_hip.rds'))
heartdiab <- readRDS(here('../Data', 'Elix_heartdiab.rds'))


#Hip/heart comorbidity detail stats

hipcats <- c('hip_op_total','hip_op_partial','hip_op_osteopin')

starthip <-  hip %>% filter(hip_cat %in% hipcats) %>% #get hip cohort
  mutate(hyperten = hypertenuncomp|hypertencomp, #add a hypertension overall category
         diab = diabuncomp|diabcomp) #add a diabetes overall category

startheart <- heartdiab %>% #get heart cohort
  mutate(hyperten = hypertenuncomp|hypertencomp, #add a hypertension overall category
         diab = diabuncomp|diabcomp) #add a diabetes overall category



#Function for creating counts of conditions based on specified grouping

cond_order <- read_csv(here('Reference_data', 'elix_order.csv')) %>% mutate(sort = row_number())
conds <- cond_order[['cond']]

elixfunc1 <- function(data, groups = NULL){
  {{data}} %>% select(!!conds, !!groups) %>% #reduce to Elixhauser conditions and grouping variables
    pivot_longer(!c(!!groups), names_to = 'cond', values_to = 'flag') %>% #make long format
    group_by(across(c(cond, !!groups, flag))) %>% #group the data
    summarise(count = length(flag), .groups = 'keep') %>% #count the values by all groups
    pivot_wider(names_from = flag, names_prefix = 'x', values_from = count) %>% #spread TRUE/FALSE/NA flag counts across wide columns
    mutate(base = sum(xFALSE, xTRUE, xNA, na.rm = TRUE), #count across to get the base
           xTRUE = ifelse(is.na(xTRUE), 0, xTRUE), #if there were no TRUE flags, set to zero
           perc = xTRUE/base) #create a percentage from the counts of TRUEs over the base
}


#Stats not split by hip_cat

hipstats0 <- elixfunc1(starthip) %>% mutate(gender = 3) #set gender to 3 (this is our code for 'all sexes')
hipstats1 <- elixfunc1(starthip, 'gender')  #group by gender
x <- bind_rows(list(hipstats0, hipstats1)) %>% select(cond, perc, gender) %>% #bind results and reduce to required cols
  pivot_wider(names_from = gender, names_prefix = 'gender', values_from = perc) %>% #pivot wider by gender code
  select(cond, gender3, gender2, gender1) #reduce to required cols

hipstats2 <- elixfunc1(starthip, 'hip_agegroup') %>% mutate(gender = 3) #group by age group 
hipstats3 <- elixfunc1(starthip, c('hip_agegroup', 'gender')) #group by age group and gender
y <- bind_rows(list(hipstats2, hipstats3)) %>% select(cond, hip_agegroup, perc, gender) %>% 
  pivot_wider(names_from = gender, names_prefix = 'gender', values_from = perc) %>% 
  select(cond, hip_agegroup, gender3, gender2, gender1)

z <- bind_rows(list(x, y)) %>% inner_join(cond_order) %>% #bind x and y together and join on condition order
  mutate(hip_agegroup = ifelse(is.na(hip_agegroup), 'Overall', hip_agegroup)) #if there is no age group, rename as 'Overall'

spacer <- z %>% select(sort, sheetorder) %>% unique() %>% mutate(hip_agegroup = 'zSpacer') #create spacing rows for the output

results <- bind_rows(list(z, spacer)) %>% arrange(sort, hip_agegroup) %>% #bind together z and the spacers and arrange
  select(sheetorder, hip_agegroup, gender3, gender2, gender1) #reduce to required cols

write_csv(results, here('../Outputs', 'hip_elix_cips1.csv'), na = '0') #write to file



#Same stats but split by hip_cat

hipstats0 <- elixfunc1(starthip, c('hip_cat')) %>% mutate(gender = 3)
hipstats1 <- elixfunc1(starthip, c('hip_cat', 'gender')) 
x <- bind_rows(list(hipstats0, hipstats1)) %>% select(cond, hip_cat, gender, perc) %>% 
  pivot_wider(names_from = c(hip_cat, gender), values_from = perc)

hipstats2 <- elixfunc1(starthip, c('hip_cat', 'hip_agegroup')) %>% mutate(gender = 3)
hipstats3 <- elixfunc1(starthip, c('hip_cat', 'hip_agegroup', 'gender')) 
y <- bind_rows(list(hipstats2, hipstats3)) %>% select(cond, hip_agegroup, hip_cat, gender, perc) %>% 
  pivot_wider(names_from = c(hip_cat, gender), values_from = perc)
 
z <- bind_rows(list(x, y)) %>% inner_join(cond_order) %>%
  mutate(hip_agegroup = ifelse(is.na(hip_agegroup), 'Overall', hip_agegroup))

spacer <- z %>% select(sort, sheetorder) %>% unique() %>% mutate(hip_agegroup = 'zSpacer')

results <- bind_rows(list(z, spacer)) %>% arrange(sort, hip_agegroup) %>% 
  select(cond, hip_agegroup, ends_with('3'), ends_with('2'), ends_with('1')) %>% 
  select(cond, hip_agegroup, contains('total'), contains('partial'), contains('osteo'))

write_csv(results, here('../Outputs', 'hip_elix_cips2.csv'), na = '0')



#Stats for heart cohort

heartstats0 <- elixfunc1(startheart) %>% mutate(gender = 3)
heartstats1 <- elixfunc1(startheart, 'gender') 
x <- bind_rows(list(heartstats0, heartstats1)) %>% select(cond, perc, gender) %>% 
  pivot_wider(names_from = gender, names_prefix = 'gender', values_from = perc) %>% 
  select(cond, gender3, gender2, gender1)

heartstats2 <- elixfunc1(startheart, 'heart_agegroup') %>% mutate(gender = 3)
heartstats3 <- elixfunc1(startheart, c('heart_agegroup', 'gender'))
y <- bind_rows(list(heartstats2, heartstats3)) %>% select(cond, heart_agegroup, perc, gender) %>% 
  pivot_wider(names_from = gender, names_prefix = 'gender', values_from = perc) %>% 
  select(cond, heart_agegroup, gender3, gender2, gender1)

z <- bind_rows(list(x, y)) %>% inner_join(cond_order) %>%
  mutate(heart_agegroup = ifelse(is.na(heart_agegroup), 'Overall', heart_agegroup))

spacer <- z %>% select(sort, sheetorder) %>% unique() %>% mutate(heart_agegroup = 'zSpacer')

results <- bind_rows(list(z, spacer)) %>% arrange(sort, heart_agegroup) %>% 
  select(sheetorder, heart_agegroup, gender3, gender2, gender1) 

write_csv(results, here('../Outputs', 'heart_elix_cips1.csv'), na = '0')




#Means, standard deviations and medians of counts of Elix conds

elixfunc2 <- function(data, groups = NULL){

  dat <- {{data}} #write data to temp var
  if(!is.null(groups)){ #if groups are provided... 
    dat <- {{data}} %>% group_by(across(!!groups)) #group the data
  } 
  
  dat %>% select(elix, !!groups) %>% #retain just Elixhauser count and the groups
    summarise(sum = sum(elix, na.rm = TRUE), #summarise with... sum of the counts
      mean = mean(elix, na.rm = TRUE), #mean
      sd = sd(elix, na.rm = TRUE), #standard deviation
      mean_sd = paste0(round(mean, 2), ' (', round(sd, 2), ')'), #text formatted version "mean (sd)"
      median = median(elix, na.rm = TRUE), #median
      N = length(elix)) #number of patients (N)
}

#Hip cohort - means (sd)

#Without hip_cat breakdown
hipstats0 <- elixfunc2(starthip) %>% mutate(gender = 3)
hipstats1 <- elixfunc2(starthip, 'gender')
hipstats2 <- elixfunc2(starthip, 'hip_agegroup') %>% mutate(gender = 3)
hipstats3 <- elixfunc2(starthip, c('hip_agegroup','gender'))

x <- bind_rows(list(hipstats0, hipstats1, hipstats2, hipstats3)) %>% select(gender, hip_agegroup, mean_sd) %>% 
  pivot_wider(names_from = c(gender), names_prefix = 'gender', values_from = mean_sd) %>% arrange(hip_agegroup) %>%
  select(hip_agegroup, gender3, gender2, gender1)

write_csv(x, here('../Outputs', 'hip_elix_cips3.csv'), na = '0')


#With hip_cat breakdown
hipstats0c <- elixfunc2(starthip, 'hip_cat') %>% mutate(gender = 3)
hipstats1c <- elixfunc2(starthip, c('hip_cat', 'gender'))
hipstats2c <- elixfunc2(starthip, c('hip_cat', 'hip_agegroup')) %>% mutate(gender = 3)
hipstats3c <- elixfunc2(starthip, c('hip_cat', 'hip_agegroup','gender'))

x <- bind_rows(list(hipstats0c, hipstats1c, hipstats2c, hipstats3c)) %>% select(hip_cat, gender, hip_agegroup, mean_sd) %>% 
  pivot_wider(names_from = c(hip_cat, gender), values_from = mean_sd) %>% arrange(hip_agegroup) %>%
  select(hip_agegroup, contains('3'), contains('2'), contains('1')) %>%
  select(hip_agegroup, contains('total'), contains('partial'), contains('osteo'))

write_csv(x, here('../Outputs', 'hip_elix_cips4.csv'), na = '0')


#Same but with medians

x <- bind_rows(list(hipstats0, hipstats1, hipstats2, hipstats3)) %>% select(gender, hip_agegroup, median) %>% 
  pivot_wider(names_from = c(gender), names_prefix = 'gender', values_from = median) %>% arrange(hip_agegroup) %>%
  select(hip_agegroup, gender3, gender2, gender1)

write_csv(x, here('../Outputs', 'hip_elix_cips5.csv'), na = '0')

x <- bind_rows(list(hipstats0c, hipstats1c, hipstats2c, hipstats3c)) %>% select(hip_cat, gender, hip_agegroup, median) %>% 
  pivot_wider(names_from = c(hip_cat, gender), values_from = median) %>% arrange(hip_agegroup) %>%
  select(hip_agegroup, contains('3'), contains('2'), contains('1')) %>%
  select(hip_agegroup, contains('total'), contains('partial'), contains('osteo'))

write_csv(x, here('../Outputs', 'hip_elix_cips6.csv'), na = '0')


#Heart cohort - means (sd)

heartstats0 <- elixfunc2(startheart) %>% mutate(gender = 3)
heartstats1 <- elixfunc2(startheart, 'gender')
heartstats2 <- elixfunc2(startheart, 'heart_agegroup') %>% mutate(gender = 3)
heartstats3 <- elixfunc2(startheart, c('heart_agegroup','gender'))

x <- bind_rows(list(heartstats0, heartstats1, heartstats2, heartstats3)) %>% select(gender, heart_agegroup, mean_sd) %>% 
  pivot_wider(names_from = c(gender), names_prefix = 'gender', values_from = mean_sd) %>% arrange(heart_agegroup) %>%
  select(heart_agegroup, gender3, gender2, gender1)

write_csv(x, here('../Outputs', 'heart_elix_cips2.csv'), na = '0')

#Heart cohort - medians
x <- bind_rows(list(heartstats0, heartstats1, heartstats2, heartstats3)) %>% select(gender, heart_agegroup, median) %>% 
  pivot_wider(names_from = c(gender), names_prefix = 'gender', values_from = median) %>% arrange(heart_agegroup) %>%
  select(heart_agegroup, gender3, gender2, gender1)

write_csv(x, here('../Outputs', 'heart_elix_cips3.csv'), na = '0')








