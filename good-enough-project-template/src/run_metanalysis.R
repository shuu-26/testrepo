# Program Information  ----------------------------------------------------

# Program:      run_metanalysis
# Author:       Sophie Bots
# Description:  meta-analyse aggregated SCRI output data
# Requirements:
#               input:  data in csv with 1 row per analysis stratum
#               output: table 1, table 2, figures

# Housekeeping  -----------------------------------------------------------

## install and load packages
library(tidyverse)
library(meta)
library(stringi)

# LOADING IN THE DATASETS -------------------------------------------------------------
# Updated to output of step_12_2 that Svetlana finalised
# path probably needs adjusting to anDREa environment, but function should work
setwd("Z://inbox/")

# load in datasets and turn them into more easily manageable names
# the datasets you need (version 1.0.8) are:
# (1) alldata_brands = stratified by brand only
# (2) alldata_sex = stratified by brand & sex
# (3) alldata_age30 = stratified by brand & age (<30/30+)
# (4) alldata_sex_age30 = stratified by brand, sex & age (<30/30+)
# (5) alldata_no_split = not stratified
# (6) covidstart_no_split = excluding those with COVID-19 prior to control window (folder:no_covid_start_control_rw)

## first do the ones without issues that can be looped over
list_names <- c("brands", "sex", "age30", "sex_age30", "no_split")
nocovid <- "no_covid_start_control_rw"
load_list <- vector(mode="list", length = length(list_names))
dapnames <- c("CPRD", "SIDIAP", "ARS", "BIFAP") # add PHARMO and BIFAP when they have uploaded new data
nocovid_entry <- length(list_names) + 1

for (name in 1:length(dapnames)) {
  if(dapnames[name] == "CPRD") {
    inputfolder <- "transfer-2022-04-26-11-39-am"
    
    # make the list of file names
    for (i in 1:length(list_names)) {
      load_list[[i]] <- paste(inputfolder,"/g_export/scri/all_data/", dapnames[name], "_vax2prior_alldata_",list_names[i],"_report.RData", sep = "")
      
    }
    load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export/scri/", nocovid, "/", dapnames[name], "_vax2prior_covidstart_no_split_report.RData", sep = "")
    
    # load them in
    for (i in 1:length(load_list)) {
      load(load_list[[i]])
    }
    
  }
  else if(dapnames[name] == "PHARMO") {
    inputfolder <- "transfer-2022-03-21-09-26-am/Results of 1.0.4_18032022"
    
    # make the list of file names
    for (i in 1:length(list_names)) {
      load_list[[i]] <- paste(inputfolder,"/g_export/scri/all_data/", dapnames[name], "_vax2prior_alldata_",list_names[i],"_report.RData", sep = "")
      
    }
    load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export/scri/", nocovid, "/", dapnames[name], "_vax2prior_covidstart_no_split_report.RData", sep = "")
    
    # load them in
    for (i in 1:length(load_list)) {
      load(load_list[[i]])
    }
    
  }
  else if(dapnames[name] == "SIDIAP"){
    inputfolder <- "transfer-2022-04-26-11-17-am"
    
    # make the list of file names
    for (i in 1:length(list_names)) {
      load_list[[i]] <- paste(inputfolder,"/g_export/scri/all_data/", dapnames[name], "_vax2prior_alldata_",list_names[i],"_report.RData", sep = "")
      
    }
    load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export/scri/", nocovid, "/", dapnames[name], "_vax2prior_covidstart_no_split_report.RData", sep = "")
    
    # load them in
    for (i in 1:length(load_list)) {
      load(load_list[[i]])
    }
  }
  else if(dapnames[name] == "ARS"){
    inputfolder <- "transfer-2022-04-23-11-06-pm"
    
    # make the list of file names
    for (i in 1:length(list_names)) {
      load_list[[i]] <- paste(inputfolder,"/g_export/scri/all_data/", dapnames[name], "_vax2prior_alldata_",list_names[i],"_report.RData", sep = "")
      
    }
    load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export/scri/", nocovid, "/", dapnames[name], "_vax2prior_covidstart_no_split_report.RData", sep = "")
    
    # load them in
    for (i in 1:length(load_list)) {
      load(load_list[[i]])
    }
  }
  else if(dapnames[name] == "BIFAP"){
    inputfolder <- "transfer-2022-04-29-12-25-pm"
    
    # make the list of file names
    for (i in 1:length(list_names)) {
      load_list[[i]] <- paste(inputfolder,"/g_export_PC/scri/all_data/", dapnames[name], "_vax2prior_alldata_",list_names[i],"_report.RData", sep = "")
      
    }
    load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export_PC/scri/", nocovid, "/", dapnames[name], "_vax2prior_covidstart_no_split_report.RData", sep = "")
    
    # load them in
    for (i in 1:length(load_list)) {
      load(load_list[[i]])
    }
  }
}

# CREATE THE SCRI_DATA DATASET ---------------
# MAR2022 update (v1.0.8)
# Svetlana wrote new programme, so code needs to be updated too
# main analysis is 28-day risk window and 30-day period time adjustment
# 7-day risk window is sensitivity analysis

# write function to extract the information from the lists
extract_information <- function(data, 
                                riskwindow,
                                period) {
  #' @title Extract information 
  #' @description This function takes the raw SCRI output data
  #' from the different Data Access Providers (DAPs) and extracts
  #' only the information needed for the myocarditis paper meta-analysis.
  #' It returns a dataset with the required information 
  #' @param riskwindow The risk window length under evaluation (28 days or 7 days)
  #' @param period The granularity at which calender time is adjusted for (in period of how many days)
  #' @param data The dataset (a list) that serves as input
  #' @return A dataset with the required information
  
  set <- data[grepl(riskwindow, names(data))]
  subsets <- vector(mode = "list", length = length(set))
  
  # need to build ifelse in to remove sensitivity analysis Svetlana added where she adjusts for all events and not only those in risk window
  # recode those ("alltadj") to a NULL entry
  for (i in 1:length(set)) {
    if(grepl("tadj", names(set)[[i]])==F) {
      subset <- bind_rows(set[[i]][["all data"]][grepl(period, names(set[[i]][["all data"]]))])
      subsets[[i]] <- subset
    }
    else if(grepl("subgrtadj", names(set)[[i]])==T) {
      subset <- bind_rows(set[[i]][["all data"]][grepl(period, names(set[[i]][["all data"]]))])
      subsets[[i]] <- subset
    }

    else if (grepl("alltadj", names(set)[[i]]) == T) {
      subsets[[i]] <- NULL
    }
  }
  
  # bind_rows will ignore the NULL entries, so these are taken out
  # also add distinct because now there are duplicates in the sets for some reason?
  set2 <- bind_rows(subsets) %>%
    mutate(dap = stri_extract_first_regex(names[name], pattern = "[:alpha:]+"),
           selection = stri_extract_first_regex(names[name], pattern = "(?<=vax2prior_).{7,10}(?=_(brands|sex|age30|no_split))"),
           stratum = stri_extract_last_regex(names[name], pattern = "(?<=_(alldata|covidstart|covid30d)_).*(?=_report)"),
           analysis = ifelse(grepl("2v_28", riskwindow)==T, "main", "sensitivity"),
           caltime_adjustment = ifelse(grepl("30d", period)==T, "30d", period)) %>%
    select(-c(contains("cum_ev"), model)) %>%
    distinct()
  
  return(set2)

}

# create list of all the datasets you want to include in the main dataset
dap_datasets <- unlist(stri_extract_last_regex(load_list, "(?<=[:upper:])_.*(?=\\.RData)"))
dapnames <- c("CPRD", "SIDIAP", "ARS", "BIFAP") # add PHARMO when they are ready
list_names <- vector(mode = "list", length = length(dapnames))
names(list_names) <- dapnames

for (name in 1:length(dapnames)) {
  for (i in 1:length(dap_datasets)) {
    list_names[[name]][[i]] <- paste(dapnames[name], dap_datasets[i], sep="")
  }
}
names <- unlist(list_names)
# remove the BIFAPhosp you don't use
names <- names[which(names != "BIFAPhosp_vax2prior_alldata_no_split_report" & names != "BIFAPhosp_vax2prior_covidstart_no_split_report")]

# run the extract_information for all sets you want to include
main_sets <- vector(mode = "list", length = length(names))
sensitivity_sets <- vector(mode = "list", length = length(names))
main_noadj_sets <- vector(mode = "list", length = length(names))
names(main_sets) <- names
names(sensitivity_sets) <- names
names(main_noadj_sets) <- names

# run the function
for (name in 1:length(names)) {
  main_sets[[name]] <- extract_information(data = get(names[name]),
                                           riskwindow = "2v_28d_obsperc_0",
                                           period = "30d_start-234d")
  print(names[name])
  sensitivity_sets[[name]] <- extract_information(data = get(names[name]),
                                                  riskwindow = "2v_7d_obsperc_0",
                                                  period = "30d_start-234d")

  main_noadj_sets[[name]] <- extract_information(data = get(names[name]),
                                            riskwindow = "2v_28d_obsperc_0",
                                            period = "no_adj")
}

# make it into one dataset
main_set <- bind_rows(main_sets) %>% select(-contains("model"))
sensitivity_set <- bind_rows(sensitivity_sets) %>% select(-contains("model"))
noadj_set <- bind_rows(main_noadj_sets)
analysis_set <- rbind(main_set, sensitivity_set, noadj_set)

# clean up the analysis set so it can be fed into the meta-analysis functions
scri_data <- analysis_set %>%
  # take out all time period rows
  filter(!is.na(stri_match_all_regex(all_cat, pattern = "[:alpha:]+"))) %>%
  # clean up variable names
  rename(
    irr = RR,
    lci = `2.5%`,
    uci = `97.5%`,
    yi = coef,
    sei = `se(coef)`) %>%
  # reference category rows now have missing values for irr and yi 
  # fix this so you can plot/tabulate them;
  # mutate_at(.vars = vars(irr, lci, uci),
  #           .funs = ~ ifelse(is.na(.), 1, .)) %>%
  # mutate_at(.vars = vars(yi, sei),
  #           .funs = ~ ifelse(is.na(.), 0, .)) %>%
  # clean up event and DAP variable
  mutate(event = ifelse(event == "myocard", "myocarditis", "pericarditis"),
         event = ifelse(dap == "PHARMO", "myopericarditis", event),
         dap = recode(dap,
                      "ARS" = "ARS (Italy)",
                      "BIFAP" = "BIFAP-pc (Spain)",
                      "CPRD" = "CPRD (United Kingdom)",
                      "PHARMO" = "PHARMO (The Netherlands)",
                      "BIFAPhosp" = "BIFAP-hosp (Spain)",
                      "SIDIAP" = "SIDIAP (Spain)")) %>%
  # create necessary variables as columns:
  # (1) vaccine type
  mutate(vacctype = stri_extract_first_regex(all_cat, pattern = "(Astra|Pfize|J&J|Moder)"),
         vacctype = recode(vacctype, 
                           "Astra" = "AstraZeneca",
                           "Pfize" = "Pfizer",
                           "J&J" = "Janssen",
                           "Moder" = "Moderna"),
         # (2) sex
         sex = stri_extract_first_regex(pattern = "sex:(0|1)", all_cat),
         sex = recode(sex,
                      "sex:0" = "Women",
                      "sex:1" = "Men"),
         sex = ifelse(is.na(sex), "all sex", sex),
         # (3) age
         agegroup = stri_extract_first_regex(all_cat, pattern = "age.*(?=] &)"),
         agegroup = recode(agegroup,
                           "age(-1,30" = "under 30",
                           "age(30,Inf" = "over 30"),
         agegroup = ifelse(is.na(agegroup), "all age", agegroup),
         # (4) risk windows
         # importantly -  remove leading and trailing whitespace to make recode work
         riskwindow = stri_extract_first_regex(all_cat, pattern = "(?<=(Astra|Pfize|J&J|Moder)).*"),
         riskwindow = ifelse(grepl(c("-29;-1|0;0|1;28|1;7|8;14|15;28|-90;-30"), riskwindow) == TRUE,
                             trimws(riskwindow), NA),
         riskwindow = recode(riskwindow,
                             "buffer[-29;-1]" = "pre-exposure period",
                             "dose 1 [0;0]" = "dose 1 day 0",
                             "dose 1 [1;28]" = "dose 1 day 1-28",
                             "dose 1 [1;7]" = "dose 1 day 1-7",
                             "dose 1 [8;14]" = "dose 1 day 8-14",
                             "dose 1 [15;28]" = "dose 1 day 15-28",
                             "dose 1 pre-exposure[-90;-30]" = "dose 1 control window",
                             "dose 2 pre-exposure[-90;-30]" = "dose 2 control window",
                             "dose 2 [0;0]" = "dose 2 day 0",
                             "dose 2 [1;28]" = "dose 2 day 1-28",
                             "dose 2 [1;7]" = "dose 2 day 1-7",
                             "dose 2 [8;14]" = "dose 2 day 8-14",
                             "dose 2 [15;28]" = "dose 2 day 15-28")) %>%
  distinct()

# RUNNING THE META-ANALYSIS -----------------------------------------------

## PREPARATION
# there are many analysis strata, so make variable to denote stratum
# saves lots of loops
ma_input <- scri_data %>%
  filter(grepl("no", all_cat) == F) %>%
  filter(grepl("dose", riskwindow) == T) %>%
  filter(grepl("0|control", riskwindow) == F) %>%
  filter(dap != "BIFAP-hosp (Spain)") %>%
  filter(caltime_adjustment == "30d") %>%
  mutate(analysis_stratum = str_c(sex, agegroup, riskwindow, sep="_"))

## for Table 4 you need the number of cases in each control window and risk window
# make a table of this
controlw_cases <- scri_data %>%
  filter(grepl("control window", riskwindow) == T) %>%
  filter(caltime_adjustment == "30d" & analysis == "main" & selection == "alldata") %>%
  filter(dap != "BIFAP-hosp (Spain)") %>%
  mutate(analysis_stratum = str_c(sex, agegroup, riskwindow, sep="_")) %>%
  group_by(event, vacctype, analysis_stratum) %>%
  summarise(ntot_contr = sum(atrisk_ids), unexp_cases = sum(n_events)) %>%
  mutate(analysis_stratum = trimws(gsub(x = analysis_stratum, pattern = "control window", replacement = "")))

# also make a table of the number of cases in each risk window
riskw_cases <- ma_input %>%
  filter(caltime_adjustment == "30d" & analysis == "main" & selection == "alldata") %>%
  group_by(event, vacctype, analysis_stratum) %>%
  summarise(ntot_rw = sum(atrisk_ids), exp_cases = sum(n_events)) %>%
  arrange(event, vacctype, analysis_stratum) %>%
  mutate(analysis_stratum = trimws(gsub(x = analysis_stratum, pattern = "day 1-28", replacement = "")))


# combine the tables
table_cases <- controlw_cases %>%
  full_join(riskw_cases, by=c("event", "vacctype", "analysis_stratum"))


## Now do the actual meta-analysis
# Function that performs the meta-analysis
perform_ma <- function(data, 
                        outcome, 
                        analysis_selection,
                        population = "alldata") {
  #' @title Perform meta-analysis
  #' @description This function takes the merged SCRI output data
  #' from the different Data Access Providers (DAPs) and applies
  #' random effects meta-analysis stratified by vaccine brand. It returns a meta object with the
  #' meta-analysis results
  #' @param data The dataset that serves as input
  #' @param outcome The outcome under evaluation (myoperi, myo, or pericarditis)
  #' @param analysis_selection The analysis stratum defined based on sex, age, and risk window
  #' @param population The populations selection that should be meta-analysed (default = alldata, so no selection)
  #' @return A meta object with the meta-analysis results
  
  subset <- data %>%
    filter(event == outcome & 
             analysis_stratum == analysis_selection)
  
  # check for errors and return message when error occurs
  error <- tryCatch(
  meta_analysis <- metagen(subset,
                           TE = yi, seTE = sei, studlab = dap,
                           sm = "IRR", lower = lci, upper = uci,
                           random = T, fixed = F,
                           subgroup = vacctype,
                           n.e = n_events, n.c = atrisk_ids,
                           label.e = "events", label.c = "N"),
 
   error = function(e) e)
  
  return(error)
  }

create_table <- function(data,
                         nlists) {
  #' @title Create Table 
  #' @description This function takes the meta-analysis output and cleans it up for Table format (e.g. rounding, censoring n < 5)
  #' @param data The dataset that serves as input; must be list format
  #' @param nlists The number of lists that is in the dataset list
  #' @return A data frame with clean meta-analysis output results
  
  tab <- vector(mode = "list", length = nlists)
  
  for (n in 1:nlists) {
    if(is.null(data[[n]]) ==T) {
      tab[[n]] <- NULL
    }
    else {
    tab[[n]] <- data[[n]] %>%
      mutate_at(.vars = vars(irr, lci, uci),
                .funs = ~round(., digits = 2)) %>%
      mutate_at(.vars = vars(N_events, N_persons),
                list(c = ~ifelse(. < 5, "< 5", as.character(.))))
    }
    }
  
  table1 <- bind_rows(tab) 
  
  return(table1)
  }


# (1) -- MAIN ANALYSIS -------------
# for this only use the analysis strata that are part of "main" 
analysis_strata <- unique(ma_input[ma_input$analysis == "main",]$analysis_stratum)
myo_models <- vector(mode = "list", length = length(analysis_strata))
names(myo_models) <- analysis_strata

peri_models <- myo_models
myoperi_models <- myo_models
myo_output <- myo_models
peri_output <- myo_models
myoperi_output <- myo_models


# run the meta-analysis loop over all analysis strata
for (i in 1:length(analysis_strata)) {
  
  # myo first
  myo <- perform_ma(data = ma_input,
                     outcome = "myocarditis",
                     analysis_selection = analysis_strata[i])
  
  # peri second
  peri <- perform_ma(data = ma_input,
                      outcome = "pericarditis",
                      analysis_selection = analysis_strata[i])
  
  # myoperi third
  myoperi <- perform_ma(data = ma_input,
                        outcome = "myopericarditis",
                        analysis_selection = analysis_strata[i])


  # saving models
  myo_models[[i]] <- myo
  peri_models[[i]] <- peri
  myoperi_models[[i]] <- myoperi
  
  # creating tables with results
  if (length(myo) > 2) {
  tab1 <- data.frame(
    vacc = myo$bylevs,
    irr = exp(myo$TE.random.w),
    lci = exp(myo$lower.random.w),
    uci = exp(myo$upper.random.w),
    stratum = analysis_strata[i],
    N_persons = myo$n.c.w,
    N_events = myo$n.e.w)
  }
  else(tab1 <- NULL)
  
  if (length(peri) > 2) {
  tab2 <- data.frame(
    vacc = peri$bylevs,
    irr = exp(peri$TE.random.w),
    lci = exp(peri$lower.random.w),
    uci = exp(peri$upper.random.w),
    stratum = analysis_strata[i],
    N_persons = peri$n.c.w,
    N_events = peri$n.e.w)
  }
  else(tab2 <- NULL)
  
  if (length(myoperi) > 2) {
    tab3 <- data.frame(
      vacc = myoperi$bylevs,
      irr = exp(myoperi$TE.random.w),
      lci = exp(myoperi$lower.random.w),
      uci = exp(myoperi$upper.random.w),
      stratum = analysis_strata[i],
      N_persons = myoperi$n.c.w,
      N_events = myoperi$n.e.w)
  }
  else(tab3 <- NULL)

  # save tables
  myo_output[[i]] <- tab1
  peri_output[[i]] <- tab2
  myoperi_output[[i]] <- tab3
}

# create the tables
myo_table <- create_table(data = myo_output,
                          nlists = length(myo_output))

peri_table <- create_table (data = peri_output,
                            nlists = length(peri_output))

myoperi_table <- create_table (data = myoperi_output,
                            nlists = length(myoperi_output))

# save the tables
# write.csv(myo_table,
#           file = "Z:/inbox/scri/output/20220406_myocarditis 28d meta-analysis per stratum.csv",
#           row.names = F)
# 
# write.csv(peri_table,
#            file = "Z:/inbox/scri/output/20220429_pericarditis 28d meta-analysis per stratum.csv",
#            row.names = F)
# 
# write.csv(myoperi_table,
#           file = "Z:/inbox/scri/output/20220406_myopericarditis 28d meta-analysis per stratum.csv",
#           row.names = F)
# write.csv(table_cases,
#           file = "Z:/inbox/scri/output/20220414_table with numbers of unexposed and exposed cases per stratum.csv",
#           row.names = F)

# (2) SENSITIVITY ANALYSIS USING 7-DAY RISK WINDOW INSTEAD OF 28-DAY ---------------
# for this only use analysis strata that are part of "sensitivity"
analysis_strata2 <- unique(ma_input[ma_input$analysis == "sensitivity",]$analysis_stratum)
myo_models2 <- vector(mode = "list", length = length(analysis_strata2))
names(myo_models2) <- analysis_strata2

peri_models2 <- myo_models2
myoperi_models2 <- myo_models2
myo_output2 <- myo_models2
peri_output2 <- myo_models2
myoperi_output2 <- myo_models2

# run the meta-analysis loop over all analysis strata
for (i in 1:length(analysis_strata2)) {
  
  # myo first
  myo <- perform_ma(data = ma_input,
                    outcome = "myocarditis",
                    analysis_selection = analysis_strata2[i])
  
  # peri second
  peri <- perform_ma(data = ma_input,
                     outcome = "pericarditis",
                     analysis_selection = analysis_strata2[i])
  
  # myoperi third
  myoperi <- perform_ma(data = ma_input,
                     outcome = "myopericarditis",
                     analysis_selection = analysis_strata2[i])
  
  
  # saving models
  myo_models2[[i]] <- myo
  peri_models2[[i]] <- peri
  myoperi_models2[[i]] <- myoperi
  
  # creating tables with results
  if (length(myo) > 2) {
    tab1 <- data.frame(
      vacc = myo$bylevs,
      irr = exp(myo$TE.random.w),
      lci = exp(myo$lower.random.w),
      uci = exp(myo$upper.random.w),
      stratum = analysis_strata2[i],
      N_persons = myo$n.c.w,
      N_events = myo$n.e.w)
  }
  else(tab1 <- NULL)
  
  if (length(peri) > 2) {
    tab2 <- data.frame(
      vacc = peri$bylevs,
      irr = exp(peri$TE.random.w),
      lci = exp(peri$lower.random.w),
      uci = exp(peri$upper.random.w),
      stratum = analysis_strata2[i],
      N_persons = peri$n.c.w,
      N_events = peri$n.e.w)
  }
  else(tab2 <- NULL)
  
  if (length(myoperi) > 2) {
    tab3 <- data.frame(
      vacc = myoperi$bylevs,
      irr = exp(myoperi$TE.random.w),
      lci = exp(myoperi$lower.random.w),
      uci = exp(myoperi$upper.random.w),
      stratum = analysis_strata2[i],
      N_persons = myoperi$n.c.w,
      N_events = myoperi$n.e.w)
  }
  else(tab3 <- NULL)
  
  # save tables
  myo_output2[[i]] <- tab1
  peri_output2[[i]] <- tab2
  myoperi_output2[[i]] <- tab3
}

# create the tables
myo_table2 <- create_table(data = myo_output2,
                          nlists = length(myo_output2))

peri_table2 <- create_table (data = peri_output2,
                            nlists = length(peri_output2))

myoperi_table2 <- create_table (data = myoperi_output2,
                             nlists = length(myoperi_output2))

# save the tables
# write.csv(myo_table2,
#           file = "Z:/inbox/scri/output/20220406_myocarditis 7d meta-analysis per stratum.csv",
#           row.names = F)
# 
# write.csv(peri_table2,
#            file = "Z:/inbox/scri/output/20220406_pericarditis 7d meta-analysis per stratum.csv",
#            row.names = F)
# 
# write.csv(myoperi_table2,
#           file = "Z:/inbox/scri/output/20220406_myopericarditis 7d meta-analysis per stratum.csv",
#           row.names = F)

# (3) SENSITIVITY ANALYSIS COMPARING WHOLE POPULATION TO NO PRIOR COVID -----
# for this only use selection where stratum is no_split to compare "alldata" with "covidstart"
nocov_input <- scri_data %>%
  filter(stratum == "no_split") %>%
  filter(grepl("no|>|0", all_cat) == F) %>%
  filter(grepl("dose", all_cat) == T) %>%
  filter(dap != "BIFAP-hosp (Spain)") %>%
  filter(caltime_adjustment == "30d"& analysis == "main") %>%
  mutate(
    riskwindow = stri_extract_first_regex(all_cat, pattern = "dose (1|2)"),
    label = str_c(selection, event, riskwindow, sep = "_"),
    label2 = factor(label, levels = c("alldata_myocarditis_dose 1", "covidstart_myocarditis_dose 1",
                                      "alldata_myocarditis_dose 2", "covidstart_myocarditis_dose 2",
                                      "alldata_pericarditis_dose 1", "covidstart_pericarditis_dose 1",
                                      "alldata_pericarditis_dose 2", "covidstart_pericarditis_dose 2",
                                      "alldata_myopericarditis_dose 1", "covidstart_myopericarditis_dose 1",
                                      "alldata_myopericarditis_dose 2", "covidstart_myopericarditis_dose 2"),
                    labels = c("myocarditis dose 1 - whole population", "myocarditis dose 1 - no prior covid",
                               "myocarditis dose 2 - whole population", "myocarditis dose 2 - no prior covid",
                               "pericarditis dose 1 - whole population", "pericarditis dose 1 - no prior covid",
                               "pericarditis dose 2 - whole population", "pericarditis dose 2 - no prior covid",
                               "myopericarditis dose 1 - whole population", "myopericarditis dose 1 - no prior covid",
                               "myopericarditis dose 2 - whole population", "myopericarditis dose 2 - no prior covid")))

nocov_output <- metagen(nocov_input,
                      TE = yi, seTE = sei, studlab = dap,
                      sm = "IRR", lower = lci, upper = uci,
                      random = T, fixed = F,
                      subgroup = label2,
                      n.e = n_events, n.c = atrisk_ids,
                      label.e = "events", label.c = "N")

forest.meta(nocov_output,
            overall = F,
            overall.hetstat = F,
            test.subgroup = F,
            print.subgroup.name = F,
            text.subgroup.nohet = T,
            label.left = "lower risk", label.right = "higher risk",
            xlim = c(0.1, 5),
            leftcols = c("studlab"),
            rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
            just = "center",
            smlab = "",
            print.tau2 = F,
            fs.heading = 10,
            fontsize = 10,
            colgap.forest.left = unit(10, "mm"),
            sortvar = studlab)


# export as PDF in size 8inch x 17inch (probably little longer because SIDIAP needs to be included as well)

# FOREST PLOTS --------------------------------------------------------------------

# create forest plot that displays IRRs per DAP for each vaccine type
# you need to run the metagen and specifcy plot details before creating the forest plot
# we want a plot that shows the number of cases and events
# and we want to add some x-axis labels

create_plot <- function(data,
                        subgroup
){
  #' @title Create forest plots per subgroup and dose
  #' @description This function takes the create_tab1 output data
  #' It returns a forest plot
  #' @param subgroup The subgroup of interest
  #' @param data The dataset, which should be a list of meta-analysis results
  #' @return A forest plot
  
  plot <- forest.meta(data[[subgroup]],
                      overall = F,
                      overall.hetstat = F,
                      test.subgroup = F,
                      print.subgroup.name = F,
                      text.subgroup.nohet = T,
                      label.left = "lower risk", label.right = "higher risk",
                      xlim = c(0.1, 5),
                      leftcols = c("studlab"),
                      leftlabs = c("Vaccine brand"),
                      rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
                      just = "center",
                      smlab = "",
                      print.tau2 = F,
                      fs.heading = 10,
                      fontsize = 10,
                      colgap.forest.left = unit(10, "mm"),
                      sortvar = studlab)
  
}

## for the main models:
# 1/2 = all sex all age dose 1&2
# 3/4 = men dose 1&2
# 5/6 = women dose 1&2
# 7/8 = over 30 dose 1&2
# 9/10 = under 30 dose 1&2
# 11/12 = men over 30 dose 1&2
# 13/14 = women under 30 dose 1&2
# 15/16 = women over 30 dose 1&2
# 17/18 = men under30 dose 1&2

## for the sensitivity analyses models:
# 1/4 = all sex all age dose 1&2 (7 day risk window)

# myo
pdf(file = "Z:/inbox/scri/output/20220404_Forestplot_myo_28d_dose2.pdf", width = 9, height = 9)
create_plot(
  data = myo_models,
  subgroup = 3
)
dev.off()

pdf(file = "Z:/inbox/scri/output/20220401_Forestplot_peri_28d_dose2.pdf", width = 9, height = 9)
# peri
create_plot(
  data = peri_models,
  subgroup = 11
)
dev.off()

# check the 7-day plots

create_plot(
  data = myo_models2,
  subgroup = 3
)

# RESPONSE TO REVIEWER ADDITIONAL ANALYSES -----------------------

## EDITOR COMMENT 5  & REVIEWER 1 comment 5
# check effect in pre-exposure period
# to determine whether assumption of event-independent exposure holds
# for this you need the no_split files to be uploaded and included in scri_data

pre_input <- scri_data %>%
  filter(stratum == "no_split" & analysis == "main") %>%
  filter(grepl("buffer", all_cat) == T) %>%
  filter(caltime_adjustment == "30d" & selection == "alldata")

pre_output <- metagen(pre_input,
                      TE = yi, seTE = sei, studlab = dap,
                      sm = "IRR", lower = lci, upper = uci,
                      random = T, fixed = F,
                      subgroup = event,
                      n.e = n_events, n.c = atrisk_ids,
                      label.e = "events", label.c = "N")

forest.meta(pre_output,
            overall = F,
            overall.hetstat = F,
            test.subgroup = F,
            print.subgroup.name = F,
            text.subgroup.nohet = T,
            label.left = "lower risk", label.right = "higher risk",
            xlim = c(0.1, 5),
            leftcols = c("studlab"),
            rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
            just = "center",
            smlab = "",
            print.tau2 = F,
            fs.heading = 10,
            fontsize = 10,
            colgap.forest.left = unit(10, "mm"),
            sortvar = studlab)

## REVIER 1 comment 1
# is there a difference in effect between BIFAPpc and BIFAPhosp

bifap_input <- scri_data %>%
  filter(grepl("BIFAP", dap) == TRUE) %>%
  filter(stratum == "brands" & analysis == "main") %>%
  filter(caltime_adjustment == "30d" & vacctype == "Pfizer") %>%
  filter(grepl("28", riskwindow) == TRUE) %>%
  mutate(label = str_c(event, riskwindow, sep = "_"),
         label2 = factor(label,
                         levels = c("myocarditis_dose 1 day 1-28", "myocarditis_dose 2 day 1-28",
                                    "pericarditis_dose 1 day 1-28", "pericarditis_dose 2 day 1-28"),
                         labels = c("myocarditis - dose 1", "myocarditis - dose 2",
                                    "pericarditis - dose 1", "pericarditis - dose 2")))

bifap_output <- metagen(bifap_input,
                      TE = yi, seTE = sei, studlab = dap,
                      sm = "IRR", lower = lci, upper = uci,
                      random = F, fixed = F,
                      subgroup = label2,
                      n.e = n_events, n.c = atrisk_ids,
                      label.e = "events", label.c = "N")

forest.meta(bifap_output,
            overall = F,
            overall.hetstat = F,
            test.subgroup = F,
            print.subgroup.name = F,
            text.subgroup.nohet = T,
            label.left = "lower risk", label.right = "higher risk",
            xlim = c(0.1, 5),
            leftcols = c("studlab"),
            rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
            just = "center",
            smlab = "",
            print.tau2 = F,
            fs.heading = 10,
            fontsize = 10,
            colgap.forest.left = unit(10, "mm"),
            sortvar = studlab)

## REVIEWER 3 comments on discussion
# about the effect of COVID and it being a confounder
# should show difference in effect between unadjusted and adjusted models

unadj_input <- scri_data %>%
  filter(analysis == "main" & stratum == "no_split" & selection == "alldata") %>%
  mutate(dose = trimws(stri_extract_first_regex(all_cat, "dose .{1}")),
         caltime_adjustment = recode_factor(caltime_adjustment,
                                     "no_adj" = "unadjusted",
                                     "30d" = "adjusted"),
         label = str_c(event, caltime_adjustment, dose, sep = "_"),
         label2 = factor(label, levels = c("myocarditis_unadjusted_dose 1", "myocarditis_adjusted_dose 1",
                                           "myocarditis_unadjusted_dose 2", "myocarditis_adjusted_dose 2", 
                                           "pericarditis_unadjusted_dose 1", "pericarditis_adjusted_dose 1",
                                           "pericarditis_unadjusted_dose 2", "pericarditis_adjusted_dose 2",
                                           "myopericarditis_unadjusted_dose 1", "myopericarditis_adjusted_dose 1",
                                           "myopericarditis_unadjusted_dose 2", "myopericarditis_adjusted_dose 2"),
                         labels = c("myocarditis dose 1 - unadjusted", "myocarditis dose 1 - adjusted",
                                    "myocarditis dose 2 - unadjusted", "myocarditis dose 2 - adjusted",
                                    "pericarditis dose 1 - unadjusted", "pericarditis dose 1 - adjusted",
                                    "pericarditis dose 2 - unadjusted", "pericarditis dose 2 - adjusted",
                                    "myopericarditis dose 1 - unadjusted", "myopericarditis dose 1 - adjusted",
                                    "myopericarditis dose 2 - unadjusted", "myopericarditis dose 2 - adjusted"))) %>%
  filter(!is.na(dose)) %>%
  filter(grepl("0", all_cat) == F) %>%
  filter(grepl(">", all_cat) == F)

  
unadj_output <- metagen(unadj_input,
                      TE = yi, seTE = sei, studlab = dap,
                      sm = "IRR", lower = lci, upper = uci,
                      random = T, fixed = F,
                      subgroup = label2,
                      n.e = n_events, n.c = atrisk_ids,
                      label.e = "events", label.c = "N")

forest.meta(unadj_output,
            overall = F,
            overall.hetstat = F,
            test.subgroup = F,
            print.subgroup.name = F,
            text.subgroup.nohet = F,
            label.left = "lower risk", label.right = "higher risk",
            xlim = c(0.1, 5),
            leftcols = c("studlab"),
            rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
            just = "center",
            smlab = "",
            print.tau2 = F,
            fs.heading = 10,
            fontsize = 10,
            colgap.forest.left = unit(10, "mm"),
            sortvar = studlab)

# export as PDF in size 8inch x 17inch (probably little longer because SIDIAP needs to be included as well)

# OLD CODE -----------------------------------------------------------------------
# scri_data <- scri_data %>%
#   # variable names with nonalpha characters are hard to read in R so change this
#   rename(
#     irr = RR,
#     lci = `2.5%`,
#     uci = `97.5%`,
#     yi = coef,
#     sei = `se(coef)`) %>%
#   # reference category rows now have missing values for irr and yi 
#   # fix this so you can plot/tabulate them;
#   mutate_at(.vars = vars(irr, lci, uci),
#             .funs = ~ ifelse(is.na(.), 1, .)) %>%
#   mutate_at(.vars = vars(yi, sei),
#             .funs = ~ ifelse(is.na(.), 0, .)) %>%
#   # censor numbers < 5 but this breaks metagen so THINK ABOUT THIS
#   # mutate_at(.vars = vars(n_events, atrisk_persons),
#   #           .funs = ~ ifelse(. < 5, "<5", .)) %>%
#   # relabel vacctype to official names instead of brand names
#   mutate(vacctype = ifelse(vacctype == "J", "Janssen",
#                            ifelse(vacctype == "Astra", "AstraZeneca",
#                                   ifelse(vacctype == "Pfize", "Pfizer",
#                                          ifelse(vacctype == "Moder", "Moderna", vacctype)))),
#          dap_country = ifelse(dap == "ARS", 'ARS (Italy)',
#                               ifelse(dap == "BIFAP", "BIFAP-pc (Spain)",
#                                      ifelse(dap == "CPRD", "CPRD (United Kingdom)",
#                                             "PHARMO (the Netherlands)")))) %>%
#   # create a risk window label that is understandable and a variable for adjustment and outcome
#   # because these are needed for the create_tab1 function in meta-analysis
#   mutate(label = factor(period,
#                         levels = unique(scri_data$period),
#                         labels = c("control window", "pre-exposure period", "dose 1 day 0", "dose 1 day 1-28",
#                                    "in between doses", "dose 2 day 0", "dose 2 day 1-28", 
#                                    "after dose 2", "dose 1 day 1-7", "dose 1 day 8-14", "dose 1 day 15-28", 
#                                    "in between doses", "in between doses", "in between doses",
#                                    "dose 2 day 1-7", "dose 2 day 8-14", "dose 2 day 15-28",
#                                    "after dose 2", "after dose 2", "after dose 2"))) %>%
#   # clean up event variable
#   mutate(event = ifelse(event == "myocard", "myocarditis", "pericarditis"))
# 
# 
# 
# # so the datasets are structured per DAP, vaccine type, and stratum
# # stratified analyses all have their own set with the strata in the name
# # DAP name is given in the list name (e.g. CPRD_, ARS_, etc.)
# # vaccine type is given in the sublist name (e.g. AstraZeneca, Pfizer, etc.)
# # as is stratum for stratified analyses (e.g. age(-18,30], etc.)
# # create loop for all datasets that extracts this information into columns (e.g. variables)
# # and store each cleaned dataset in a list called type_sets
# 
# # loop will include all sets specified in 'names', so make sure this is complete
# names <- c("CPRD_scri_models_A", "CPRD_scri_models_A_age", "CPRD_scri_models_A_sex_age", "CPRD_scri_models_A_noCovid_in_study_period",
#            "ARS_scri_models_A", "ARS_scri_models_A_age", "ARS_scri_models_A_sex_age", "ARS_scri_models_A_noCovid_in_study_period",
#            "PHARMO_scri_models_A", "PHARMO_scri_models_A_age", "PHARMO_scri_models_A_sex_age", "PHARMO_scri_models_A_noCovid_in_study_period",
#            "BIFAP_scri_models_A", "BIFAP_scri_models_A_age", "BIFAP_scri_models_A_sex_age", "BIFAP_scri_models_A_noCovid_in_study_period")
# type_sets <- vector(mode = "list", length = length(names))
# names(type_sets) <- names
# 
# for (name in 1:length(names)) {
#   subset <- get(names[name])
#   
#   for (i in 1:length(subset)) {
#     if (subset[[i]] != "no data") {
#     # add the dap name, vaccine type and stratum as variables
#     # using regex based on
#     # name of dataset in case of dap
#     # name of subset name in case of vaccine type and stratum
#     # dap and vacctype both take first word (e.g. 'alpha') as value
#     # stratum takes text between _ and ] as value
#     # excl_covid19 is 1 if the name contains 'Covid'
#     subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
#       mutate(dap = stri_extract_first_regex(names[name], pattern = "[:alpha:]+"),
#              vacctype = stri_extract_first_regex(names(subset)[i], pattern = "[:alpha:]+"),
#              stratum = stri_extract_all_regex(names(subset)[i], pattern = "(?<=\\_).*\\]", simplify = T),
#              excl_covid19 = ifelse(grepl("Covid", names[name])==T, 1, 0))
#     }
#   }
#   
#   # select only the complete information model and then only the "tab" entry of that
#   subset <- subset[which(grepl("buf_betw", names(subset))==TRUE)]
#   subset <- unlist(subset, recursive = F)
#   subset <- subset[which(grepl(".tab", names(subset))==TRUE)]
#   subset <- subset[which(grepl(".tab_", names(subset))==FALSE)]
#   
#   
#   # combine the vaccine subsets into one type set
#   type_set <- bind_rows(subset)
#   
#   # remove columns from the Covid19 set
#   if(grepl("Covid", names[name]) == T){
#     type_set <- type_set %>%
#       rename(vd = period) %>%
#       select(-drug)
#   }
#   
#   # and then save the set in the type_sets list
#   type_sets[[name]] <- type_set
# }
#     
# # combine all dap subsets into one dataset
# #scri_data <- data.frame()
# for (i in 1:length(names)) {
#   scri_data <- rbind(scri_data, type_sets[[i]])
# }
# 
# # variable names do not match with the meta-analysis code and we miss some variables
# # fix this before moving on the the analysis step
# 
# #ukn <- scri_data %>%
# #  filter(vacctype == "UKN" & is.na(stratum))
# 
# #max(ukn$atrisk_persons)
# 
# scri_data <- scri_data %>%
#   # variable names with nonalpha characters are hard to read in R so change this
#   rename(
#     irr = RR,
#     lci = `2.5%`,
#     uci = `97.5%`,
#     yi = coef,
#     sei = `se(coef)`) %>%
#   # reference category rows now have missing values for irr and yi 
#   # fix this so you can plot/tabulate them;
#   mutate_at(.vars = vars(irr, lci, uci),
#             .funs = ~ ifelse(is.na(.), 1, .)) %>%
#   mutate_at(.vars = vars(yi, sei),
#             .funs = ~ ifelse(is.na(.), 0, .)) %>%
#   # censor numbers < 5 but this breaks metagen so THINK ABOUT THIS
#   # mutate_at(.vars = vars(n_events, atrisk_persons),
#   #           .funs = ~ ifelse(. < 5, "<5", .)) %>%
#   # relabel vacctype to official names instead of brand names
#   mutate(vacctype = ifelse(vacctype == "J", "Janssen",
#                            ifelse(vacctype == "Astra", "AstraZeneca",
#                                   ifelse(vacctype == "Pfize", "Pfizer",
#                                          ifelse(vacctype == "Moder", "Moderna", vacctype)))),
#          dap_country = ifelse(dap == "ARS", 'ARS (Italy)',
#                               ifelse(dap == "BIFAP", "BIFAP-pc (Spain)",
#                                      ifelse(dap == "CPRD", "CPRD (United Kingdom)",
#                                             "PHARMO (the Netherlands)")))) %>%
#   # create a risk window label that is understandable and a variable for adjustment and outcome
#   # because these are needed for the create_tab1 function in meta-analysis
#   mutate(label = factor(period,
#                         levels = unique(scri_data$period),
#                         labels = c("control window", "pre-exposure period", "dose 1 day 0", "dose 1 day 1-28",
#                                    "in between doses", "dose 2 day 0", "dose 2 day 1-28", 
#                                    "after dose 2", "dose 1 day 1-7", "dose 1 day 8-14", "dose 1 day 15-28", 
#                                    "in between doses", "in between doses", "in between doses",
#                                    "dose 2 day 1-7", "dose 2 day 8-14", "dose 2 day 15-28",
#                                    "after dose 2", "after dose 2", "after dose 2"))) %>%
#   # clean up event variable
#   mutate(event = ifelse(event == "myocard", "myocarditis", "pericarditis"))
#   
  

  # # clean up the stratum variable so the levels are meaningful
  # mutate(subgroup = ifelse(is.na(stratum), "all", stratum),
  #        subgroup = factor(subgroup,
  #                         levels = c("all", "age(-1,30]", "age(30,120]", "sex0_age(-1,30]", "sex0_age(30,120]",
  #                                    "sex1_age(-1,30]", "sex1_age(30,120]"),
  #                         labels = c("all", "age_under30", "age_30up", "women_under30", "women_30up", 
  #                                    "men_under30",  "men_30up"))) %>%
  # # remove unknown vacctype (n = 6)
  # filter(vacctype != "UKN")



