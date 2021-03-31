#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Download and save HRG reference costs
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----

pkgs <- c('here', 'tidyverse', 'utils', 'readxl') #package list
sapply(pkgs, require, character.only = TRUE) #load packages

savedir <- here('Reference_data', 'HRG_reference_costs') #create path for saving HRG reference cost files
dir.create(savedir, showWarnings = TRUE) #create folder if it doesn't exist

#__________________________________________

#Download the HRG reference cost files ----
#NB: an internet connection is required for this section of code

specs <- list( #list web addresses of HRG reference cost files - NB: these addresses could change in future
  source = c('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/397469/03a_2013-14_National_Schedule_-_CF-NET_updated.xls', 
             'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/480791/2014-15_National_Schedules.xlsx', 
             'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/577084/National_schedule_of_reference_costs_-_main_schedule.xlsx', 
             'https://improvement.nhs.uk/documents/6467/201617_ReferenceCostData.zip', 
             'https://improvement.nhs.uk/documents/6468/201718_reference_costs_data_and_guidance.zip'),
  year = 2013:2017, #list years
  fileformat = c('.xls', '.xlsx', '.xlsx', '.zip', '.zip') #list associated file types
)
#Download the files/folders and save using, renaming using the year and file format listed
pmap(specs, function(source, year, fileformat) download.file(source, paste0(savedir, '/HRG_ref_costs_', year, fileformat), mode = 'wb'))

#Dealing with zip folders
list.files(savedir, full.names = TRUE) %>% .[str_sub(., -4, -1) == '.zip'] %>% map(unzip, exdir = savedir) #unzip any zip files
file.copy(list.files(savedir, full.names = TRUE, pattern = "2 - National schedule of reference costs*"), #copy required files from the unzipped zip files
          paste0(savedir, '/HRG_ref_costs_' , 2016:2017, '.xlsx')) #save with new, consistent filenames

#Remove any files that are not needed
list.files(savedir, full.names = TRUE) %>% #list all files in folder
  .[!. %in% list.files(savedir, full.names = TRUE, pattern = 'HRG_ref_costs_.*\\.xls*')] %>% #identify any files that are not needed
    unlink(force = TRUE, recursive = TRUE) #delete them

#_____________________________________

#Read in HRG reference cost files ----

filepaths <- list.files(savedir, full.names = TRUE, pattern = '\\.xls*') #get filepaths of reference costs
types <- c('Total HRGs', "Total HRG's", 'EL', 'EL_XS', 'NEL', 'NEL_XS', 'NES', 'DC', 'RP', 'CL', 'NCL', 'OPROC', 
           'AE', 'EM', 'SPC', 'SPAL', 'CHEM', 'HCD', 'RAD', 'RENAL', 'IMAG', 'NM', 'AMB') #list all sheets we want to include

#There are other cost sheets that we don't normally use: 'CMDT', 'CC', 'REHAB'
#For rehab we cannot identify rehab level, so cannot cost this activity type
#Also, some sheets are not consistently named: 'Total HRGs'/"Total HRG's", 'AE'/'EM', 'SPC'/'SPAL'

files <- list.files(savedir, full.names = FALSE, pattern = '^HRG_ref_costs_....\\.xls*') #list the filenames (without paths)
sheets <- map(filepaths, excel_sheets) #list the sheets for each file
in_sheets <- sheets %>% map(unlist) %>% map(~ . %in% types) %>% map2(sheets, ., ~ .x[.y]) #keep only those sheets of the types we want
in_files <- map2(filepaths, in_sheets, ~ rep(.x, length(.y))) #repeat the filepaths for each sheet kept for each file
years <- map2(files, in_sheets, ~ rep(str_extract(.x, '[0-9]+'), length(.y))) #repeat the year for each sheet listed
specs <- list(f = unlist(in_files), sh = unlist(in_sheets), yr = unlist(years)) #create a specification for reading in the data

#The below section creates many new names for variables when attempting to read in the sheets. This is expected and not a problem.
HRGcosts1 <- specs %>% pmap(function(f, sh, yr) 
  read_excel(f, sheet = sh, col_types = 'text')) %>%  map(~ .[[1]]) %>% #read in data as messy original
  map(grep, pattern = '^Currency|^Service|^Department') %>% unlist(.) %>% list_modify(specs, sk =.) %>% #identify empty rows to skip
  pmap(function(f, sh, yr, sk) read_excel(f, sheet = sh, col_types = 'text', skip = sk)) #re-read the file skipping empty rows

sheet_names <- str_replace_all(specs$sh, " |\\'", '') %>% str_replace_all('SPC', 'SPAL') %>% str_replace_all('AE', 'EM') #edit inconsistent sheet names
names(HRGcosts1) <- paste0(sheet_names, specs$yr) #label the data in HRGcosts1 using the sheet name and year

cols_regex <- '^Service|^Department|^Currency|Unit Cost...4$|National Average Unit Cost' #identify variables to keep
HRGcosts2 <- HRGcosts1 %>% map(.,select, matches(cols_regex)) %>%  #select them
  map(rename_all, tolower) %>% map(rename_all, ~str_replace_all(., '\\s', '')) %>% #make all lower case and remove spaces
  map(rename_at, vars(contains('unitcost')), ~str_replace(., '.*', 'unitcost')) %>%  #rename unit cost variables
  map(rename_at, vars(matches('^currencycode$|^currency$')), ~str_replace(., '.*', 'HRG')) %>% #rename Currency Code/Currency to HRG
  map(rename_at, vars(matches('^servicedesc')), ~str_replace(., '.*', 'servicedescription')) %>% #fix inconsistent variable names
  map(mutate_at, 'unitcost', as.numeric) %>% #make unit cost numeric
  bind_rows(.id = 'label') %>% #bind into one dataset
  mutate(type = str_sub(label, 1, -5), HRGyear = str_sub(label, -4, -1)) #split the label into type (i.e. sheet name) and year

saveRDS(HRGcosts2, paste0(savedir, '/HRGcosts.rds')) #save the HRG reference costs dataset to file

#______________________________________________________

#Create deflators from Treasury GDP Deflators file ----
#Used to deflate costs to a reference year (in our study, this was 2016/17)

#Deflators were downloaded from: www.gov.uk/government/collections/gdp-deflators-at-market-prices-and-money-gdp

deflators <- read_excel(here('Reference_data', 'GDP_Deflators_Budget_March_2020_update.xlsx'), skip = 6) %>% #read the Excel file
  separate(1, c('HRGyear', 'endyear'), '-') %>% #separate the first column into two, to get the HRG year
  .[59:66, c(1:4)] %>% #reduce the rows and columns after eye-balling the data
  rename(HRGyear = 1, endyear = 2, deflator = 3, perc_change = 4) %>% #rename the columns
  mutate(deflator = as.numeric(deflator)) #change the deflator to numeric (was charater)

deflation_base <- deflators[deflators$HRGyear == 2016, 'deflator'] #store base year (2016/17) deflator as single figure

#The file we used was not complete up to 2020, so we had to use the % change variables to edit it
def <- deflators %>% #need to use the available information to fill-in the deflators column up to 2020
  mutate(d1 = lag(deflator, 1), #lag the deflator
         d2 = d1 * (1 + (perc_change/100)), #multiply by the perc_change columns
         d3 = lag(d2, 1), #lag again
         d4 = d3 * (1 + (perc_change/100)), #multiply again
         deflator = ifelse(is.na(d4), deflator, d4), #fill in deflators column if there is no value for the year
         def_mult = as.numeric(deflation_base)/as.numeric(deflator)) #calculate multipliers

saveRDS(def, here('Reference_data', 'deflators.rds')) #save to file


