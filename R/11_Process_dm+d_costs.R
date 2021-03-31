#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Process NHSBSA dm+d data from TRUD to create costs for therapies
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - load packages, source folder locations ----

pkgs <- c('tidyverse', 'XML', 'here')
sapply(pkgs, require, character.only = TRUE)

#________________________________________

#Read xml data files and save as RDS ----

savedir <- here('Reference_data', 'NHSBSA_dm+d') #create path for saving dm+d files
dir.create(savedir, showWarnings = TRUE) #create folder if it doesn't exist

#NB: dm+d files listed below were downloaded from the NHSBSA TRUD website, unzipped and saved into savedir
dmd <- list(VMP = '/f_vmp2_3160420.xml', 
            VMPP = '/f_vmpp2_3160420.xml',
            AMP = '/f_amp2_3160420.xml', 
            AMPP = '/f_ampp2_3160420.xml') %>% #list file names
  map(~paste0(savedir, .)) %>% map(xmlParse) #read xml files into list

dmd$VMPP2 <- dmd[[2]] #copy VMPP so that we can use it to pull out the element_node: DTINFO

#Store paths to datasets
XPaths <- c('//VIRTUAL_MED_PRODUCTS//VMPS//VMP', '/VIRTUAL_MED_PRODUCT_PACK//VMPPS//VMPP', '//ACTUAL_MEDICINAL_PRODUCTS//AMPS//AMP', 
            '//ACTUAL_MEDICINAL_PROD_PACKS//AMPPS//AMPP', '//VIRTUAL_MED_PRODUCT_PACK//DRUG_TARIFF_INFO//DTINFO')

dmd_tibble <- map2(dmd, XPaths, ~ as_tibble(xmlToDataFrame(nodes = getNodeSet(.x, .y)))) #use XML package to parse XML data paths
View(dmd_tibble)

saveRDS(dmd_tibble, paste0(savedir, '/dmd.rds')) #save parsed data to file

#______________________________________________________________

#Check various types of ID to see which are listed in CPRD ----

dmd <- readRDS(paste0(savedir, '/dmd.rds')) #read in dm+d data
dmd[['VMPP2']] %>% mutate(priceflag = ifelse(is.na(PRICE), 'No price', 'Priced')) %>% group_by(priceflag) %>% tally() #check how many records are priced
#NB: only the DTINFO node contains pricing info, and this is at the VMPP level

vmp <- dmd[['VMPP2']] %>% left_join(dmd[['VMPP']]) %>% left_join(dmd[['VMP']]) #join the virtual pack data together
amp <- dmd[['VMPP2']] %>% left_join(dmd[['AMPP']]) %>% left_join(dmd[['AMPP']]) #join the actual pack data together


#Read in reference tables from CPRD (product) and Vision (gemscript)
product <- readRDS(here('../Data', 'product.rds'))
gemscript <- read_delim(here('Reference_data', 'gemscript_dmd_map_May20.txt'), delim = '|', col_types = 'ccc')
dmdpg <- product %>% left_join(gemscript, by = c('gemscriptcode' = 'gemscript_drug_code'))


#Join reference tables onto dm+d pricing data by any IDs possible
vmpdmd <- dmdpg %>% left_join(vmp, by = c('dmd_code'='VPID'))
vmppdmd <- dmdpg %>% left_join(vmp, by = c('dmd_code'='VPPID'))
ampdmd <- dmdpg %>% left_join(amp, by = c('dmd_code'='APID'))
amppdmd <- dmdpg %>% left_join(amp, by = c('dmd_code'='APPID'))


#Check how many records are priced for each table and ID type
#...only vmpdmd and ampdmd
vmpdmd %>% group_by(!is.na(PRICE)) %>% tally()
vmppdmd %>% group_by(!is.na(PRICE)) %>% tally()
ampdmd %>% group_by(!is.na(PRICE)) %>% tally()
amppdmd %>% group_by(!is.na(PRICE)) %>% tally()


#Create mean price per unit in both tables
#(Because VMP and AMP prices are not unit costs, need to divide by quantities in QTYVAL)

mean_vmp <- vmpdmd %>% mutate(unitprice = as.numeric(PRICE)/as.numeric(QTYVAL)) %>% #add unit price to virtual product table
  filter(!is.na(unitprice)) %>% group_by(prodcode, drugsubstance) %>% summarise(mean_unitprice = mean(unitprice, na.rm = TRUE))

mean_amp <- ampdmd %>% left_join(vmp %>% select(VPPID, QTYVAL), by = 'VPPID') %>% #need QTYVAL, which is in VMP table
  mutate(unitprice = as.numeric(PRICE)/as.numeric(QTYVAL)) %>% #add unit price to virtual product table
  filter(!is.na(unitprice)) %>% group_by(prodcode, drugsubstance) %>% summarise(mean_unitprice = mean(unitprice, na.rm = TRUE))


#Join together and save
dmdprices <- bind_rows(mean_vmp, mean_amp) %>% mutate(price = mean_unitprice/100) #prices originally in pence, convert to £s
saveRDS(dmdprices, here('Reference_data', 'dmd_prices.rds'))


