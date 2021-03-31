#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: English results for the International Collaborative on Costs, Outcomes & Needs in Care (ICCONIC)
# Purpose: Function for cost and utilisation summary statistics outputs in ICCONIC format
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)


#Function to add hip_cat to list of grouping variables if specific hip_cats are used ----
grouping_vars <- function(age_group, hip_cats){
  
  hip_cat_yn <- if(!is.null(hip_cats)){'hip_cat'} #if there are specific hip_cats, use var hip_cat
  
  return(c('gender', age_group, hip_cat_yn)) #return the grouping vars
}


#Function to create all required combinations of grouping variables ----
group_sets <- function(age_group, hip_cats, extra_cat = NULL){
  
  groups <- grouping_vars(age_group, hip_cats)
  
  group_sets <- map(1:length(groups), ~ combn(groups, .x)) %>% #create a list of all combinations of grouping variables
    map(~ asplit(.x, 2)) %>% #split each element into a list of vectors
    unlist(recursive = FALSE) %>% #unlist them one level so they are a single list
    map(~ c(., extra_cat)) %>% #append extra_cat grouping variable to each list item
    c('All', extra_cat, .) #add All and extra_cat grouping as group_by on their own
  #NB: extra_cat is used to add outpatient specialty categories to split all outpatient outputs
  
  return(group_sets) #return the list of sets of grouping vars
}


#Function to create cohort denominators by required groupings ----
denominators <- function(cohort_in, age_group, hip_cats){
  
  groups <- grouping_vars(age_group, hip_cats) #get groups to use
  groupsets <- group_sets(age_group, hip_cats) #get grouping sets
  
  cohort_in <- if(!is.null(hip_cats)){filter(cohort_in, hip_cat %in% hip_cats)} else {cohort_in} #if hip_cats is provided, filter the cohort data
  cohort_in <- mutate(cohort_in, All = NA) #add All grouping var (i.e. no grouping)
  
  denoms <- map(groupsets, function(.x) { #with each groupset
    cohort_in %>% group_by(across(!!.x)) %>% #group by the groups in that set
      tally(n = 'pats.') %>% ungroup() #summarise with a count and ungroup
    }) %>% bind_rows() #bind the results together

  return(denoms) #return the grouped denominators
}


#Function to create required statistics using various groupings ----
stats_func <- function(cohort_in, cost_data, cost_var, pre_post = NULL, age_group, hip_cats = NULL, extra_cat = NULL){
  
  ## Example arguments...
  # cohort_in = hipdeaths365
  # cost_data = costdat
  # cost_var = 'totcost'
  # pre_post = NULL
  # age_group = 'hip_agegroup'
  # hip_cats = c('hip_op_total','hip_op_partial','hip_op_osteopin')
  # extra_cat = 'category'
  
  groups <- grouping_vars(age_group, hip_cats) #get groups to use
  groupsets <- group_sets(age_group, hip_cats, extra_cat) #get grouping sets
  denoms <- denominators(cohort_in, age_group, hip_cats) #get denominators
  
  cohort_in <- select(cohort_in, patid, !!groups) #reduce cohort data to required columns
  
  cost_data <- if(!is.null(pre_post)){filter(cost_data, !!sym(pre_post))} else {cost_data} #filter costs to pre/post follow-up if required
  cost_data <- cost_data %>% ungroup() %>% select(patid, !!cost_var, !!extra_cat) %>% #ungroup and reduce cost data (keeping extra_cat if it is there)
    filter(!is.na(!!sym(cost_var))) %>% #filter to remove NA costs if any
    inner_join(cohort_in, by = 'patid') #join onto cohort to get only relevant patients
  
  main_stats <- cost_data %>% group_by(across(c(!!groups, !!extra_cat))) %>% #group by all of the grouping variables (including extra_cat)
    summarise(cost = sum(!!sym(cost_var), na.rm = TRUE), #sum the total cost 
              count = n(), .groups = 'drop') %>% #sum the number of records
    mutate(All = NA) #add All variable to represent no grouping
  
  grouped_stats <- map(groupsets, function(.x) { #with each groupset
      main_stats %>% group_by(across(!!.x)) %>% #group by the groups in that set
      summarise(across(c(cost, count), sum, na.rm = TRUE, .names = '{.col}.'), .groups = 'drop') #sum by each group set
    }) %>% 
    bind_rows() %>% #bind the results together
    right_join(denoms, by = c('All', groups)) %>% #join on the denominators
    mutate(across(c(cost., count.), ~ .x / pats., .names = 'mean{.col}'), #create means
           gender = recode(gender, 'Male', 'Female', .missing = 'All'), #recode the gender variable
           All = NULL) %>% #remove the All column
    pivot_wider(names_from = groups[groups != age_group], #pivot wider for all groups except the age groups
                values_from = c(pats., cost., count., meancost., meancount.)) %>%
    pivot_longer(cols = -c(!!age_group, !!extra_cat), names_to = c('type', '.value'), #pivot longer so that mean costs are separated from mean counts
                 names_pattern = '(.+)\\._(.+)') %>%
    select(type, !!extra_cat, !!age_group, contains('All'), contains('Female'), contains('Male')) %>% #arrange the columns in the correct order
    arrange(across(c(type, !!extra_cat, !!age_group))) #ensure the sort order is correct to separate the results
  
  if('hip_cat' %in% groups){ #if the analysis is broken down by hip_cat...
	grouped_stats <- grouped_stats %>% #arrange the columns in the correct order...
      select(type, !!extra_cat, !!age_group, contains('NA'), contains('total'), contains('partial'), contains('osteo'))
  }
  
  return(grouped_stats)
}


#____________________________________________________________________________

#Additional work: Function for returning deciles of cost and utilisation ----

decile_func <- function(cohort_in, data_in, prepost, var_in){
  
  ## Example arguments...
  # cohort_in <- hip_cohort
  # data_in <- costs
  # prepost <- 'hippost'
  # var_in <- 'totcost'
  
  data_nm <- deparse(substitute(data_in)) #get name of data used
  cohort_nm <- deparse(substitute(cohort_in)) #get name of cohort used
  
  d1 <- data_in %>% filter(!!sym(prepost)) %>% #filter cost/utilisation data by prepost
    right_join(cohort_in) %>%  #right join on the cohort data
    group_by(patid) %>% #group by patient
    summarise(cost_temp = sum(!!sym(var_in), na.rm = TRUE), #sum the total cost 
              count_temp = sum(!is.na(!!sym(var_in)))) #count the number of records

  c1 <- d1 %>% arrange(count_temp) %>% #sort by count
    mutate(count_decile = ntile(count_temp, 10)) %>% #create a decile group variable
    group_by(count_decile) %>% #group by the decile
    summarise(Ncount = sum(!is.na(count_temp)), #count sample size by decile
              sum_count = sum(count_temp)) %>% #sum the counts by decile
    mutate(cum_sum_count = cumsum(sum_count)) %>% #create a cumulative sum variable
    select(decile = count_decile, Ncount, cum_sum_count) #reduce to decile and cumsum cols
  
  c2 <- d1 %>% arrange(cost_temp) %>% #[repeat the above but for cost variable]
    mutate(cost_decile = ntile(cost_temp, 10)) %>% 
    group_by(cost_decile) %>% 
    summarise(Ncost = sum(!is.na(cost_temp)),
              sum_cost = sum(cost_temp)) %>% 
    mutate(cum_sum_cost = cumsum(sum_cost)) %>% 
    select(decile = cost_decile, Ncost, cum_sum_cost) 
  
  #Calculate the coefficient of variation values for the count and cost
  c3 <- d1 %>% summarise(CV_cost  = mean(cost_temp,  na.rm = TRUE) / sd(cost_temp,  na.rm = TRUE),
                         CV_count = mean(count_temp, na.rm = TRUE) / sd(count_temp, na.rm = TRUE))

  result <- inner_join(c1, c2) %>% #join the results together
    cbind(c3) %>% #bind coefficient of variation results on
    mutate(label = paste(cohort_nm, data_nm, var_in)) #add a label
  
  check <- d1 %>% summarise(sum_count = sum(count_temp), sum_cost = sum(cost_temp)) #create check sums
  cat('\nNumber of patients in cohort: ', nrow(d1), #print the number of patients in the summarised dataset
      '\nCheck total counts and costs:', check[['sum_count']], check[['sum_cost']], '\n') #and check sums
  
  return(result)

}



