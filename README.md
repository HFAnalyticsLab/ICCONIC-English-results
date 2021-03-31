# International Collaborative on Costs, Outcomes and Needs in Care (ICCONIC) - Results for England 2020
## An analysis of specific cohorts of patients with high healthcare needs and associated costs

#### Project Status: Unpublished
This analysis has been submitted for publication in an academic journal and we'll provide links to the published articles when available.



## Project Description

ICCONIC explores cross-national variations in care trajectories, resource use and outcomes for selected high-cost high-need patient groups across 11 countries: 
Australia, Canada, England, France, Germany, the Netherlands, New Zealand, Spain, Sweden, Switzerland and the United States.

Making use of patient-level datasets linked across multiple care settings - spanning primary care, specialty services, acute hospital care and (in some countries) post-acute care - we explore the variations in utilisation and costs of health services across care settings and health systems in the year following index hospital admission.

This repository contains files related to the analysis of **English** data for two specific groups of high-cost high-need patients:
1. **Patients aged 65 and over admitted to hospital with a hip fracture and undergoing a hip replacement or repair procedure** - a tracer condition for frailty in older age
2. **Patients aged 65-90 with a primary diagnosis of congestive heart failure and diabetes** - representing an older person with complex multimorbidity

More information on the eligibility criteria for the cohorts can be found in the reference files and R script 3.

The code in this repository is used for data preparation, to define and describe the study cohorts (e.g. demographic details, clinical characteristics) and to quantify healthcare utilisation, spending and health outcomes.

We also carried out additional analyses of the English data to explore the following questions:
1. for the heart failure group - how does the presence or absence of different comorbidities affect healthcare spending and ulitisation in the year following hospital admission?
2. for the hip fracture group - how does healthcare spending and utlisation in the year prior to a hip fracture compare to in the year following a hip fracure?
3. for the hip fracture group - how does the location and cost of care in the 30, 90 and 180 days before death differ across countries?
4. for both personas - how much within-country variation exists in healthcare spending and utilisation and how this differs across care settings


## Data Sources

The main data source this project used was from the **Clinical Practice Research Datalink (CPRD) GOLD** (ISAC protocol number 18_124). CPRD GOLD is a primary care database from a set of GP practices that use INPS Vision software. The data that was sourced included linked Hospital Episode Statistics (HES) and ONS death registration data.

To increase sample size, patients were selected from multiple years (2014/15, 2015/16 and 2016/17), but in each case activity was calculated for a year following identification. This applies to both patient groups and all spending and utilisation categories.

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in the Health Foundation's Secure Data Environment (SDE), which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

Various code lists and reference files used in this analyis can be found in the 'Reference_data' folder. These include:

* [CPRD_cohort_definitions](Reference_data/CPRD_cohort_definitions) - a set of code lists used for identifying the various cohorts of interest. 
* [HRG_reference_costs](Reference_data/HRG_reference_costs) - spreadsheets of HRG unit costs from the NHS England [National Cost Collection](https://www.england.nhs.uk/national-cost-collection/).
* [NHSBSA_dm+d](Reference_data/NHSBSA_dm+d) - a database of prices for drugs and medical devices from the NHS Business Services Authority.
* [agegroups](Reference_data/agegroups.csv) - a lookup file to link ages in years to various age band definitions.
* [Comorbidities](Reference_data/Comorbidities.xlsx) - a spreadsheet listing ICD-10 codes for a variety of conditions that may be comorbid for the cohorts of interest. Used to develop the Elixhauser Comorbidity Index.
* [elix_order](Reference_data/elix_order.csv) - a list of conditions used to re-order Elixhauser conditions to the order that was required for the ICCONIC project results.
* [GDP_Deflators](Reference_data/GDP_Deflators_Budget_March_2020_update.xlsx) - a set of deflators used to adjust cost estimates to a single reference year.
* [hip_ops](Reference_data/hip_ops.csv) - a set of OPCS codes used to identify hip operations for the hip cohort.
* [Outpatient_specialty_buckets](Reference_data/Outpatient_specialty_buckets.xlsx) - a list of codes used to group main specialties for outpatient attendances into broad categories. 

We also use a reference file provided by INPS (the developer of the Vision primary care software): *gemscript_dmd_map_May20.txt*. This is a reference table used to link codes for drugs and medical devices from the Vision software system ('gemscript' codes) to the NHSBSA dm+d ('dmd' codes). This reference file is proprietary and so has not been shared here. Other researchers should contact INPS directly to request a copy of this file if aiming to reproduce the analysis of primary care therapies undertaken as part of this project.


## How does it work?

As the data used for this analysis is not publicly available, the code cannot be used to replicate the analysis on this dataset. However, with due care and attention the code can be adapted for use on other research projects which are based on patient-level CPRD GOLD extracts (with linkage to HES and ONS data, where applicable).


### Requirements

These scripts were written in R version 3.6.2 and RStudio version 1.2.5033. The following R packages (available on CRAN) are needed to run the scripts (the ones used in each script are loaded at the start of each script):

* [tidyverse](https://cran.r-project.org/package=tidyverse) - used for all data manipulation and processing
* [here](https://cran.r-project.org/package=here) - used to create relative filepaths from within an RStudio project
* [vroom](https://cran.r-project.org/package=vroom) - used to read raw/text data
* [utils](https://cran.r-project.org/package=utils) - used to manage zip folders and download files
* [readxl](https://cran.r-project.org/package=readxl) - used to read Excel files
* [xml](https://cran.r-project.org/package=xml2) - used to read the NHSBSA dm+d XML files


### Getting started

The 'R' folder contains:

* [01_Import_data.R](R/01_Import_data.R)  - due to the large amount of CPRD data needing to be read, this script reads, formats, compiles and saves the data to RDS files and then reports basic information about the datasets in the R console.
* [02_Prepare_APC_data.R](R/02_Prepare_APC_data.R) - this script is used to perform some basic checks on the Admitted Patient Care (APC) data from HES (diagnoses, procedures, critical care and episodes). It also creates within-provider spell identifiers and cross-provider Continuous In-Patient Spell (CIPS) identifiers.
* [03_Create_cohort_identifiers_using_CIPS.R](R/03_Create_cohort_identifiers_using_CIPS.R) - this script is used to identify the cohorts of interest in the ICCONIC study. It compiles basic demographic information on patients and uses the code lists in the [CPRD_cohort_definitions](Reference_data/CPRD_cohort_definitions) folder and the APC data to identify cohort members based on an index hospital admission. [A redundant section of code for identifying a cohort of patients with dementia is included. This cohort was not included in the current ICCONIC study, but the code is retained]. A set of eligibilty criteria were also applied based on patients' GP registration consistency and data quality. Due to potentially low numbers of eligible patients, three years of CPRD/HES data were used to identify cohort members.
* [04_Cohort_summary_outputs.R](R/04_Cohort_summary_outputs.R]) - this script creates some of the summary statistics required in the ICCONIC project to describe the charateristics of the cohorts. Outputs relate to: demographics; how many patients were transferred during their index CIPS; deaths in hospital and soon after admission; and days from admission to operation for the hip cohort. It is a misleadingly long and overly-complicated script. It simply creates a large number of summary tables in a wide variety of formats.
* [05_Download_HRG_reference_costs.R](R/05_Download_HRG_reference_costs.R) - this script was used to download and compile the unit costs from the NHS England National Cost Collection. It also contains some code for reformatting the [GDP_Deflators](Reference_data/GDP_Deflators_Budget_March_2020_update.xlsx). The results of this script have been stored in the [Reference_data](Reference_data/) and [HRG_reference_costs](Reference_data/HRG_reference_costs) folders.
* [06_Costing_CIPS.R](R/06_Costing_CIPS.R) - this script combines the APC data so that it can be grouped using the NHS Digital [HRG Reference Cost Groupers](https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools). Once grouped, the output is costed using the HRG reference costs from the NHS England National Cost Collection. Finally, the functions in the [Output_functions.R](R/Output_functions.R) script are used to create a variety of summary statistics.
* [07_Readmission_rates.R](R/07_Readmission_rates.R) - this script categorises the first hospital readmission for each patient after their index admission in order to identify how many patients were readmitted within certain time periods. It uses the functions in the [Output_functions.R](R/Output_functions.R) script to create a variety of summary statistics.
* [08_ICD_comorbidities.R](R/08_ICD_comorbidities.R) - this script uses the list of comorbidities in [Comorbidities.xlsx](Reference_data/Comorbidities.xlsx) and the APC diagnoses data to identify which patients have particular comorbid conditions recorded during their index admission CIPS. The conditions in the Elixhauser Comorbidity Index are used. A variety of summary statistics are then created for individual conditions and the overall index.
* [09_Costing_outpatient_visits.R](R/09_Costing_outpatient_visits.R) - this script combines the outpatient data so that it can be grouped using the NHS Digital [HRG Reference Cost Groupers](https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools). Once grouped, the output is costed using the HRG reference costs from the NHS England National Cost Collection. Finally, the functions in the [Output_functions.R](R/Output_functions.R) script are used to create a variety of summary statistics.
* [10_Costing_A&E_attendances.R](R/10_Costing_A&E_attendances.R) - this script combines the A&E data so that it can be grouped using the NHS Digital [HRG Reference Cost Groupers](https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools). Once grouped, the output is costed using the HRG reference costs from the NHS England National Cost Collection. Finally, the functions in the [Output_functions.R](R/Output_functions.R) script are used to create a variety of summary statistics.
* [11_Process_dm+d_costs.R](R/11_Process_dm+d_costs.R) - this script is used to process the NHSBSA dm+d XML data, downloaded from [TRUD](https://isd.digital.nhs.uk/trud3). The end result is a dataset with CPRD prodcodes, dmd codes and gemscript codes that contains average prices per unit for a variety of drugs and medical devices. The version last created is available in [Reference_data](Reference_data/) and named [dmd_prices.rds](Reference_data/dmd_prices.rds).
* [12_Costing_primary_care.R](R/12_Costing_primary_care.R) - this script is used to categorise and cost primary care consultations and drug therapies. The PSSRU [Unit Costs of Health and Social Care](https://www.pssru.ac.uk/project-pages/unit-costs/) are used to cost GP and nurse face-to-face and home visit consultation time, and the [dmd_prices.rds](Reference_data/dmd_prices.rds) dataset is used to cost the drug therapies. The functions in the [Output_functions.R](R/Output_functions.R) script are then used to create a variety of summary statistics.
* [13_Mortality_outcomes.R](R/13_Mortality_outcomes.R) - this script is used to identify which patients died in a variety of time periods, and whether the deaths occur in hospital or after discharge. The functions in the [Output_functions.R](R/Output_functions.R) script are used to create a variety of summary statistics.
* [14_End_of_life_analysis.R](R/14_End_of_life_analysis.R) - this script is used to calculate healthcare costs and utilisation for cohort members who died during follow-up. The estimates are calculated for three time periods: 30, 90 and 365 days prior to and up until death. The functions in the [Output_functions.R](R/Output_functions.R) script are used to create a variety of summary statistics. This script also contains code which creates survival tables in a way that is safe for release under the Health Foundation [Statistical Disclosure Control](https://figshare.com/articles/book/SDC_Handbook/9958520/1) guidelines.
* [Output_functions.R](R/Output_functions.R) - this script contains several functions that are used in the other scripts to summarise data. The main ones are *stats_func* and *decile_func*. The first of these is used to combine summarisations at various levels of aggregation to create the output format required by the ICCONIC project. The second is used to create cumulative sums by decile for measures of inequality that were part of additional work undertaken for ICCONIC.

Excluded R scripts:

* [file_paths.R]() - this script contains the file paths to raw CPRD/HES data and other resources on the Health Foundation's Secure Data Environment. The paths are contained in a separate file for security reasons and are not shared publicly.



## Useful links

* [Clinical Practice Research Datalink (CPRD)](https://www.cprd.com)
* [HES Data Dictionary](https://digital.nhs.uk/data-and-information/data-tools-and-services/hospital-episode-statistics/hospital-episode-statistics-data-dictionary)
* [National Cost Collection for the NHS](https://www.england.nhs.uk/national-cost-collection)
* [HRG Reference Cost Groupers](https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools)
* [Elixhauser Comorbidity Index](http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php)
* [PSSRU Unit Costs of Health and Social Care](https://www.pssru.ac.uk/project-pages/unit-costs)
* [NHSBSA Dictionary of Medicines and Devices (dm+d)](https://www.nhsbsa.nhs.uk/pharmacies-gp-practices-and-appliance-contractors/dictionary-medicines-and-devices-dmd)



## License

This project is licensed under the [MIT License](LICENSE).

The reference data in folder [NHSBSA_dm+d](Reference_data/NHSBSA_dm+d) contains public sector information licensed under the Open Government Licence v3.0.


## Team members - please feel free to get in touch

* Will Parry, PhD - [Twitter](https://twitter.com/DrWillParry) / [Website](https://willparry.net)
* Hannah Knight, PhD - [Twitter](https://twitter.com/HannahEllin)

<br><br><br>
