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

## SETTINGS THAT NEED UPDATING

# DAPs included in the analysis (e.g. to loop over)
dapnames <- c("ARS", "CPRD", "BIFAP", "SIDIAP", "FISABIO")

# outcomes included in the analysis (e.g. to loop over)
outcomenames <- c("myocarditis", "otitis_externa", "valvular_heart_disease")

# CREATING FUNCTIONS NEEDED IN THIS SCRIPT -------------------------------------------------------------
## NEW IN THIS VERSION (Dec2022)
# created function (match_inputfolder) to read in the files to make it easier to adapt code when input format changes

## match_inputfolder
# function to match DAP to inputfolder name
# note that this should be updated each time new input becomes available!
match_inputfolder <- function(dapname) {
  switch(dapname,
         "ARS" = "transfer-2022-12-19-10-11-pm",
         "CPRD" = "transfer-2022-12-22-08-46-am",
         "BIFAP" = "transfer-2022-12-28-02-33-pm",
         "SIDIAP" = "transfer-2022-12-26-11-17-pm",
         "FISABIO" = "transfer-2023-01-03-08-41-am",
         
         stop("Invalid dapname"))
}

## readin_files
# function to create list with all files that should be read in
# note that this should be updated each time input format changes
readin_files <- function(filename_list,
                         dapname,
                         outcome,
                         nocovid_option) {
  #' @title Read in files
  #' @description This function reads in all required files for the SCRI meta-analyses from the
  #' relevant data folders in the DRE inbox
  #' @param filename_list A character vector with all unique names for the files that need to be loaded in
  #' @param dapname A character vector of length 1 with the abbreviated DAP name
  #' @param outcome A character vector of length 1 with the outcome name (as used in folder structure)
  #' @param nocovid_option A character vector of length 1 that contains the name for the no_covid option
  #' @return a list filled with all datasets to load in
  
  inputfolder <- match_inputfolder(dapname = dapname)
  load_list <- vector(mode="list", length = length(filename_list))
  nocovid_entry <- length(filename_list) + 1
  
  for(i in seq_along(filename_list)) {
    load_list[[i]] <- paste(inputfolder,"/g_export/scri_small/", outcome, "/", dapname, "_", outcome, "_", filename_list[i], ".RData", sep = "")
  }
  load_list[[nocovid_entry]] <- paste(inputfolder,"/g_export/scri_small/", outcome, "/covid/", dapname, "_", outcome, "_nosplit_no_covid_", nocovid, ".RData", sep = "")
  
  return(load_list)
}

## extract_information
# write function to extract the information from the lists
# note that this should be updated each time input format changes
extract_information <- function(data, 
                                riskwindow,
                                period,
                                filename) {
  #' @title Extract information 
  #' @description This function takes the raw SCRI output data
  #' from the different Data Access Providers (DAPs) and extracts
  #' only the information needed for the myocarditis paper meta-analysis.
  #' It returns a dataset with the required information 
  #' @param riskwindow The risk window length under evaluation (28 days or 7 days)
  #' @param period The granularity at which calender time is adjusted for (in period of how many days)
  #' @param data The dataset (a list) that serves as input
  #' @param filename = The name of the datafile in the R environment (character)
  #' @return A dataset with the required information
  
  # in the new update, all lists have only 2 options (28d and 7d)
  # and three adjustments: no_adj and period30 starting minus 1d or 10d
  set <- data[grepl(riskwindow, names(data))]
  set <- set[[1]] # unlist once so you can use the names in the next statement
  subset <- set[grepl(period, names(set))]
  
  # bind rows to create dataset
  # you need to specify variables that are only in filename but not in table as a column
  # sex/age subgroup tables have strata variable; this is missing in others --> make as stratrum variable
  # you need to know if it is all data or covid only; create selection variable for this
  set2 <- bind_rows(subset) %>%
    mutate(
      selection = ifelse(grepl("no_covid", filename), "nocovid", "alldata"),
      stratum = ifelse("strata" %in% colnames(.), 
                       strata,
                       stri_extract_last_regex(filename, "(?<=_)[v|n].*")),
      analysis = ifelse(grepl("28d", riskwindow), "main", "sensitivity"),
      caltime_adjustment = ifelse(period=="1d", "30d", period)) %>%
    # i column duplicated in some subsets, requires removal by new name R creates for them
    select(-c(model, i, contains("..."))) %>%
    distinct()
  
  return(set2)
  
}

## perform_ma
# function that performs meta-analysis
# needs adaptation if column names change
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
                             n.e = events_rw, n.c = events_ref,
                             label.e = "events_rw", label.c = "events_cw"),
    
    error = function(e) e)
  
  return(error)
}

## create_table
# function that creates table with meta-analysed estimates, should run without needing adaptation
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
        mutate_at(.vars = vars(RW_events, CW_events),
                  list(c = ~ifelse(. < 5, "< 5", as.character(.))))
    }
  }
  
  table1 <- bind_rows(tab) 
  
  return(table1)
}


## create_plot
# create forest plot that displays IRRs per DAP for each vaccine type, should run without needing adaptation

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
# LOADING IN THE DATASETS -------------------------------------------------------------
# datasets are loaded from the DRE inbox, so set wd to there
setwd("Z://inbox/")

# use functions defined above
# load in datasets and turn them into more easily manageable names
# note that (6) is in a separate subfolder called 'covid'
# the datasets you need (version Dec2022) are:
# (1) vaxbrandshort = stratified by brand
# (2) ""_sex = stratified by brand & sex (note that _M and _F denote specific files for each sex)
# (3) ""_age = stratified by brand & age (note that (-1,30] and (30,Inf] denote specific files for age categories)
# (4) ""_sex age = stratified by brand, sex & age (see notes for sex/age on naming specifics)
# (5) nosplit = not stratified
# (6) no_covid = excluding those with prior COVID-19 (_observed = during observation period, _before_plus30d = up to 30 days after myocarditis)

# create lists for the file names so they can be read in within a loop
# this is now an issue because the names don't follow the same pattern and are combinations of each other
# create the combinations in a list and then unlist to get the character vector you need

options <- c("vaxbrandshort", "sex", "age(_1_30]", "age(30_Inf]", "nosplit") # unique options

# all combinations
names_create <- vector(mode = "list")
names_create[[1]] <- options[[1]]
names_create[[2]] <- paste(options[[1]], options[[2]], "F", sep = "_")
names_create[[3]] <- paste(options[[1]], options[[2]], "M", sep = "_")
names_create[[4]] <- paste(options[[1]], options[[3]], sep = "_")
names_create[[5]] <- paste(options[[1]], options[[4]], sep = "_")
names_create[[6]] <- paste(names_create[[2]], options[[3]], sep = " ")
names_create[[7]] <- paste(names_create[[2]], options[[4]], sep = " ")
names_create[[8]] <- paste(names_create[[3]], options[[3]], sep = " ")
names_create[[9]] <- paste(names_create[[3]], options[[4]], sep = " ")
names_create[[10]] <- options[[5]]

# create function input (dapnames is created at start script)
list_names <- unlist(names_create)
nocovid <- "observed"
nocovid_entry <- length(list_names) + 1
load_list <- vector(mode = "list", length = length(dapnames))

# create the file list for each DAP and read them in
for(name in seq_along(dapnames)) {
  for(outcome in seq_along(outcomenames)) {
  out <- readin_files(filename_list = list_names,
                      dapname = dapnames[[name]],
                      outcome = outcomenames[[outcome]],
                      nocovid_option = "observed")
  for(i in seq_along(out)) {
    load(out[[i]], verbose = T) # have it print out the names of the files loading in
  }
  load_list[[name]][[outcome]] <- out
  }
}

# CREATE THE SCRI_DATA DATASET ---------------
# main analysis is 28-day risk window and 30-day period time adjustment starting 1 day before
# 7-day risk window is sensitivity analysis

# create list of all the datasets you want to include in the main dataset
names <- stri_extract_first_regex(unlist(load_list), "[:upper:]{3,}_.*(?=.RData)")

# needs some hard fixes because file names don't match names of same objects in R environment
names <- str_replace(names, "_1_30", "-1,30")
names <- str_replace(names, "30_Inf", "30,Inf")
names <- str_replace(names, "_F", ":F")
names <- str_replace(names, "_M", ":M")

# create containers to save output of function in
main_sets <- vector(mode = "list", length = length(names))
sensitivity_sets <- vector(mode = "list", length = length(names))
main_noadj_sets <- vector(mode = "list", length = length(names))
names(main_sets) <- names
names(sensitivity_sets) <- names
names(main_noadj_sets) <- names

# run the extract_information for all sets you want to include
for (name in seq_along(names)) {
  print(names[name])
  main_sets[[name]] <- extract_information(data = get(names[name]),
                                           riskwindow = "28d",
                                           period = "1d",
                                           filename = names[name])
  print(names[name])
  sensitivity_sets[[name]] <- extract_information(data = get(names[name]),
                                                  riskwindow = "7d",
                                                  period = "1d",
                                                  filename = names[name])

  main_noadj_sets[[name]] <- extract_information(data = get(names[name]),
                                            riskwindow = "28d",
                                            period = "no_adj",
                                            filename = names[name])
}

# make it into one dataset
main_set <- bind_rows(main_sets)
sensitivity_set <- bind_rows(sensitivity_sets)
noadj_set <- bind_rows(main_noadj_sets)
analysis_set <- rbind(main_set, sensitivity_set, noadj_set)

# clean up the analysis set so it can be fed into the meta-analysis functions
scri_data <- analysis_set %>%
  # take out rows that are not part of the pre-defined risk windows (e.g. just time or after 28d)
  filter(!is.na(stri_match_all_regex(all_cat, pattern = "[:alpha:]+"))) %>%
  filter(grepl(pattern = paste(c("61", "62"), collapse = "|"), x = all_cat)==F) %>%
  # remove strata
  select(-strata) %>%
  # clean up variable names
  rename(
    dap = data,
    irr = RR,
    yi = coef,
    sei = se_coef) %>%
  # reference category rows now have missing values for irr and yi 
  # fix this so you can plot/tabulate them;
  # mutate_at(.vars = vars(irr, lci, uci),
  #           .funs = ~ ifelse(is.na(.), 1, .)) %>%
  # mutate_at(.vars = vars(yi, sei),
  #           .funs = ~ ifelse(is.na(.), 0, .)) %>%
  # clean up event and DAP variable; NOTE THIS SHOULD BE UPDATED WHEN EVENTS ARE ADDED
  mutate(event = recode(event,
                        "pericar" = "pericarditis",
                        "myocard" = "myocarditis",
                        "otitis_" = "otitis externa",
                        "valvula" = "valvular heart disease"),
         dap = recode(dap,
                      "ARS" = "ARS (Italy)",
                      "BIFAP" = "BIFAP-pc (Spain)",
                      "CPRD" = "CPRD (United Kingdom)",
                      "FISABIO" = "FISABIO (Spain)",
                      "BIFAPhosp" = "BIFAP-hosp (Spain)",
                      "SIDIAP" = "SIDIAP (Spain)")) %>%
  # create necessary variables as columns:
  # (1) vaccine type
  mutate(vacctype = stri_extract_first_regex(all_cat, pattern = "(astra|pfize|janss|moder)"),
         vacctype = recode(vacctype, 
                           "astra" = "AstraZeneca",
                           "pfize" = "Pfizer",
                           "janss" = "Janssen",
                           "moder" = "Moderna"),
         # (2) sex
         sex = stri_extract_first_regex(pattern = "sex:(F|M)", stratum),
         sex = recode(sex,
                      "sex:F" = "Women",
                      "sex:M" = "Men"),
         sex = ifelse(is.na(sex), "all sex", sex),
         # (3) age
         agegroup = stri_extract_first_regex(stratum, pattern = "age.*"),
         agegroup = recode(agegroup,
                           "age(-1,30]" = "under 30",
                           "age(30,Inf]" = "over 30"),
         agegroup = ifelse(is.na(agegroup), "all age", agegroup),
         # (4) risk windows
         # importantly -  remove leading and trailing whitespace to make recode work
         riskwindow = stri_extract_first_regex(all_cat, pattern = "(?<=(astra|pfize|janss|moder)).*"),
         riskwindow = ifelse(grepl(c("-29;-1|0;0|1;28|1;7|8;14|15;21|22;28|-90;-30"), riskwindow) == TRUE,
                             trimws(riskwindow), NA),
         riskwindow = recode(riskwindow,
                             "dose 1 reference[-90;-30]" = "dose 1 control window",
                             "dose 2 reference[-90;-30]" = "dose 2 control window",
                             "dose 3 reference[-90;-30]" = "dose 3 control window",
                             "dose 1 buffer[-29;-1]" = "pre-exposure period 1",
                             "dose 2 buffer[-29;-1]" = "pre-exposure period 2",
                             "dose 3 buffer[-29;-1]" = "pre-exposure period 3",
                             "dose 1 [0;0]" = "dose 1 day 0",
                             "dose 1 [1;28]" = "dose 1 day 1-28",
                             "dose 1 [1;7]" = "dose 1 day 1-7",
                             "dose 1 [8;14]" = "dose 1 day 8-14",
                             "dose 1 [15;21]" = "dose 1 day 15-21",
                             "dose 1 [22;28]" = "dose 1 day 22-28",
                             "dose 2 [0;0]" = "dose 2 day 0",
                             "dose 2 [1;28]" = "dose 2 day 1-28",
                             "dose 2 [1;7]" = "dose 2 day 1-7",
                             "dose 2 [8;14]" = "dose 2 day 8-14",
                             "dose 2 [15;21]" = "dose 2 day 15-21",
                             "dose 2 [22;28]" = "dose 2 day 22-28",
                             "dose 3 [0;0]" = "dose 3 day 0",
                             "dose 3 [1;28]" = "dose 3 day 1-28",
                             "dose 3 [1;7]" = "dose 3 day 1-7",
                             "dose 3 [8;14]" = "dose 3 day 8-14",
                             "dose 3 [15;21]" = "dose 3 day 15-21",
                             "dose 3 [22;28]" = "dose 3 day 22-28")) %>%
  distinct()

# RUNNING THE META-ANALYSIS -----------------------------------------------

## PREPARATION
# there are many analysis strata, so make variable to denote stratum
# saves lots of loops
ma_input <- scri_data %>%
  filter(grepl("dose", riskwindow) == T) %>%
  filter(grepl("dose 4", riskwindow) == F) %>% # remove dose 4 for now because it is empty
  filter(grepl("0|control", riskwindow) == F) %>%
  filter(dap != "BIFAP-hosp (Spain)") %>%
  filter(caltime_adjustment == "30d") %>%
  mutate(analysis_stratum = str_c(sex, agegroup, riskwindow, sep="_"))

## Now do the actual meta-analysis
# (1) -- MAIN ANALYSIS -------------
# for this only use the analysis strata that are part of "main" 
analysis_strata <- unique(ma_input[ma_input$analysis == "main",]$analysis_stratum)
myo_models <- vector(mode = "list", length = length(analysis_strata))
names(myo_models) <- analysis_strata
myo_output <- myo_models

# other outcomes
peri_models <- peri_output <- myo_models
otex_models <- otex_output <- myo_models
valv_models <- valv_output <- myo_models

# run the meta-analysis loop over all analysis strata
for (i in seq_along(analysis_strata)) {
  
  # myo first
  myo <- perform_ma(data = ma_input,
                     outcome = "myocarditis",
                     analysis_selection = analysis_strata[i])

 # peri second
  peri <- perform_ma(data = ma_input,
                      outcome = "pericarditis",
                      analysis_selection = analysis_strata[i])
  
  # other outcomes
  otex <- perform_ma(data = ma_input,
                    outcome = "otitis externa",
                    analysis_selection = analysis_strata[i])
  valv <- perform_ma(data = ma_input,
                    outcome = "valvular heart disease",
                    analysis_selection = analysis_strata[i])



  # saving models
  myo_models[[i]] <- myo
  peri_models[[i]] <- peri
  otex_models[[i]] <- otex
  valv_models[[i]] <- valv
  
  # creating tables with results
  if (length(myo) > 2) {
  tab1 <- data.frame(
    vacc = myo$bylevs,
    irr = exp(myo$TE.random.w),
    lci = exp(myo$lower.random.w),
    uci = exp(myo$upper.random.w),
    stratum = analysis_strata[i],
    CW_events = myo$n.c.w,
    RW_events = myo$n.e.w)
  }
  else(tab1 <- NULL)

  if (length(peri) > 2) {
  tab2 <- data.frame(
    vacc = peri$bylevs,
    irr = exp(peri$TE.random.w),
    lci = exp(peri$lower.random.w),
    uci = exp(peri$upper.random.w),
    stratum = analysis_strata[i],
    CW_events = peri$n.c.w,
    RW_events = peri$n.e.w)
  }
  else(tab2 <- NULL)
  
  if (length(otex) > 2) {
    tab3 <- data.frame(
      vacc = otex$bylevs,
      irr = exp(otex$TE.random.w),
      lci = exp(otex$lower.random.w),
      uci = exp(otex$upper.random.w),
      stratum = analysis_strata[i],
      CW_events = otex$n.c.w,
      RW_events = otex$n.e.w)
  }
  else(tab3 <- NULL)
  
  if (length(valv) > 2) {
    tab4 <- data.frame(
      vacc = valv$bylevs,
      irr = exp(valv$TE.random.w),
      lci = exp(valv$lower.random.w),
      uci = exp(valv$upper.random.w),
      stratum = analysis_strata[i],
      CW_events = valv$n.c.w,
      RW_events = valv$n.e.w)
  }
  else(tab4 <- NULL)

  # save tables
  myo_output[[i]] <- tab1
  peri_output[[i]] <- tab2
  otex_output[[i]] <- tab3
  valv_output[[i]] <- tab4
}

# create the tables
myo_table <- create_table(data = myo_output,
                          nlists = length(myo_output))

peri_table <- create_table (data = peri_output,
                            nlists = length(peri_output))

otex_table <- create_table (data = otex_output,
                            nlists = length(otex_output))

valv_table <- create_table (data = valv_output,
                            nlists = length(valv_output))

# save the tables
write.csv(myo_table,
          file = "Z:/inbox/scri/output/jan23_output/20230123_myocarditis 28d meta-analysis per stratum.csv",
          row.names = F)

# write.csv(peri_table,
#            file = "Z:/inbox/scri/output/jan23_output/20221222_pericarditis 28d meta-analysis per stratum.csv",
#            row.names = F)

write.csv(otex_table,
          file = "Z:/inbox/scri/output/jan23_output/20230123_otitis externa 28d meta-analysis per stratum.csv",
          row.names = F)

write.csv(valv_table,
          file = "Z:/inbox/scri/output/jan23_output/20230123_valvular heart disease 28d meta-analysis per stratum.csv",
          row.names = F)

# (2) SENSITIVITY ANALYSIS USING 7-DAY RISK WINDOW INSTEAD OF 28-DAY ---------------
# for this only use analysis strata that are part of "sensitivity"
analysis_strata2 <- unique(ma_input[ma_input$analysis == "sensitivity",]$analysis_stratum)
myo_models2 <- vector(mode = "list", length = length(analysis_strata2))
names(myo_models2) <- analysis_strata2

peri_models2 <- myo_models2
myo_output2 <- myo_models2
peri_output2 <- myo_models2

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

  # saving models
  myo_models2[[i]] <- myo
  peri_models2[[i]] <- peri
  
  # creating tables with results
  if (length(myo) > 2) {
    tab1 <- data.frame(
      vacc = myo$bylevs,
      irr = exp(myo$TE.random.w),
      lci = exp(myo$lower.random.w),
      uci = exp(myo$upper.random.w),
      stratum = analysis_strata2[i],
      CW_events = myo$n.c.w,
      RW_events = myo$n.e.w)
  }
  else(tab1 <- NULL)
  
  if (length(peri) > 2) {
    tab2 <- data.frame(
      vacc = peri$bylevs,
      irr = exp(peri$TE.random.w),
      lci = exp(peri$lower.random.w),
      uci = exp(peri$upper.random.w),
      stratum = analysis_strata2[i],
      CW_events = peri$n.c.w,
      RW_events = peri$n.e.w)
  }
  else(tab2 <- NULL)
  
  # save tables
  myo_output2[[i]] <- tab1
  peri_output2[[i]] <- tab2
}

# create the tables
myo_table2 <- create_table(data = myo_output2,
                          nlists = length(myo_output2)) %>%
  arrange(vacc, stratum)

peri_table2 <- create_table (data = peri_output2,
                            nlists = length(peri_output2)) %>%
  arrange(vacc, stratum)

# save the tables
write.csv(myo_table2,
          file = "Z:/inbox/scri/output/jan23_output/20221222_myocarditis 7d meta-analysis per stratum.csv",
          row.names = F)

write.csv(peri_table2,
           file = "Z:/inbox/scri/output/jan23_output/20221222_pericarditis 7d meta-analysis per stratum.csv",
           row.names = F)


## DEC VERSION: DOES NOT RUN THIS SENSITIVITY ANALYSIS
# (3) SENSITIVITY ANALYSIS COMPARING WHOLE POPULATION TO NO PRIOR COVID -----
# for this only use selection where stratum is no_split to compare "alldata" with "covidstart"
## DEC version does not run this so if we want it this needs updating
# nocov_input <- scri_data %>%
#   filter(stratum == "nosplit") %>%
#   filter(grepl("no|>|0", all_cat) == F) %>%
#   filter(grepl("dose", all_cat) == T) %>%
#   filter(grepl("dose 4", all_cat) == F) %>%
#   filter(dap != "BIFAP-hosp (Spain)") %>%
#   filter(caltime_adjustment == "30d"& analysis == "main") %>%
#   mutate(
#     riskwindow = stri_extract_first_regex(all_cat, pattern = "dose (1|2|3)"),
#     label = str_c(selection, event, riskwindow, sep = "_"),
#     label2 = factor(label, levels = c("alldata_myocarditis_dose 1", "covidstart_myocarditis_dose 1",
#                                       "alldata_myocarditis_dose 2", "covidstart_myocarditis_dose 2",
#                                       "alldata_pericarditis_dose 1", "covidstart_pericarditis_dose 1",
#                                       "alldata_pericarditis_dose 2", "covidstart_pericarditis_dose 2",
#                                       "alldata_myopericarditis_dose 1", "covidstart_myopericarditis_dose 1",
#                                       "alldata_myopericarditis_dose 2", "covidstart_myopericarditis_dose 2"),
#                     labels = c("myocarditis dose 1 - whole population", "myocarditis dose 1 - no prior covid",
#                                "myocarditis dose 2 - whole population", "myocarditis dose 2 - no prior covid",
#                                "pericarditis dose 1 - whole population", "pericarditis dose 1 - no prior covid",
#                                "pericarditis dose 2 - whole population", "pericarditis dose 2 - no prior covid")))
# 
# nocov_output <- metagen(nocov_input,
#                       TE = yi, seTE = sei, studlab = dap,
#                       sm = "IRR", lower = lci, upper = uci,
#                       random = T, fixed = F,
#                       subgroup = label2,
#                       n.e = n_events, n.c = atrisk_ids,
#                       label.e = "events", label.c = "N")
# 
# forest.meta(nocov_output,
#             overall = F,
#             overall.hetstat = F,
#             test.subgroup = F,
#             print.subgroup.name = F,
#             text.subgroup.nohet = T,
#             label.left = "lower risk", label.right = "higher risk",
#             xlim = c(0.1, 5),
#             leftcols = c("studlab"),
#             rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
#             just = "center",
#             smlab = "",
#             print.tau2 = F,
#             fs.heading = 10,
#             fontsize = 10,
#             colgap.forest.left = unit(10, "mm"),
#             sortvar = studlab)


# export as PDF in size 8inch x 17inch (probably little longer because SIDIAP needs to be included as well)

# FOREST PLOTS --------------------------------------------------------------------
## for the main models:
# 1/2/3 = all sex all age dose 1&2&3


# myo
pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_myo_28d_dose1.pdf", width = 8, height = 9)
create_plot(
  data = myo_models,
  subgroup = 1
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_myo_28d_dose2.pdf", width = 8, height = 9)
create_plot(
  data = myo_models,
  subgroup = 2
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_myo_28d_dose3.pdf", width = 8, height = 9)
create_plot(
  data = myo_models,
  subgroup = 3
)
dev.off()

# peri
pdf(file = "Z:/inbox/scri/output/jan23_output/20221222_Forestplot_peri_28d_dose1.pdf", width = 8, height = 9)
create_plot(
  data = peri_models,
  subgroup = 1
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20221222_Forestplot_peri_28d_dose2.pdf", width = 8, height = 9)
create_plot(
  data = peri_models,
  subgroup = 2
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20221222_Forestplot_peri_28d_dose3.pdf", width = 8, height = 9)
create_plot(
  data = peri_models,
  subgroup = 3
)
dev.off()

# otitis
pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_otitis_28d_dose1.pdf", width = 8, height = 9)
create_plot(
  data = otex_models,
  subgroup = 1
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_otitis_28d_dose2.pdf", width = 8, height = 9)
create_plot(
  data = otex_models,
  subgroup = 2
)
dev.off()

# valvular heart disease
pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_valvular_28d_dose1.pdf", width = 8, height = 9)
create_plot(
  data = valv_models,
  subgroup = 1
)
dev.off()

pdf(file = "Z:/inbox/scri/output/jan23_output/20230123_Forestplot_valvular_28d_dose2.pdf", width = 8, height = 9)
create_plot(
  data = valv_models,
  subgroup = 2
)
dev.off()

## DEC VERSION: CODE STOPS RUNNING HERE
# RESPONSE TO REVIEWER ADDITIONAL ANALYSES -----------------------

## EDITOR COMMENT 5  & REVIEWER 1 comment 5
# check effect in pre-exposure period
# to determine whether assumption of event-independent exposure holds
# for this you need the no_split files to be uploaded and included in scri_data

# pre_input <- scri_data %>%
#   filter(stratum == "no_split" & analysis == "main") %>%
#   filter(grepl("buffer", all_cat) == T) %>%
#   filter(caltime_adjustment == "30d" & selection == "alldata")
# 
# pre_output <- metagen(pre_input,
#                       TE = yi, seTE = sei, studlab = dap,
#                       sm = "IRR", lower = lci, upper = uci,
#                       random = T, fixed = F,
#                       subgroup = event,
#                       n.e = n_events, n.c = atrisk_ids,
#                       label.e = "events", label.c = "N")
# 
# forest.meta(pre_output,
#             overall = F,
#             overall.hetstat = F,
#             test.subgroup = F,
#             print.subgroup.name = F,
#             text.subgroup.nohet = T,
#             label.left = "lower risk", label.right = "higher risk",
#             xlim = c(0.1, 5),
#             leftcols = c("studlab"),
#             rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
#             just = "center",
#             smlab = "",
#             print.tau2 = F,
#             fs.heading = 10,
#             fontsize = 10,
#             colgap.forest.left = unit(10, "mm"),
#             sortvar = studlab)
# 
# ## REVIER 1 comment 1
# # is there a difference in effect between BIFAPpc and BIFAPhosp
# 
# bifap_input <- scri_data %>%
#   filter(grepl("BIFAP", dap) == TRUE) %>%
#   filter(stratum == "brands" & analysis == "main") %>%
#   filter(caltime_adjustment == "30d" & vacctype == "Pfizer") %>%
#   filter(grepl("28", riskwindow) == TRUE) %>%
#   mutate(label = str_c(event, riskwindow, sep = "_"),
#          label2 = factor(label,
#                          levels = c("myocarditis_dose 1 day 1-28", "myocarditis_dose 2 day 1-28",
#                                     "pericarditis_dose 1 day 1-28", "pericarditis_dose 2 day 1-28"),
#                          labels = c("myocarditis - dose 1", "myocarditis - dose 2",
#                                     "pericarditis - dose 1", "pericarditis - dose 2")))
# 
# bifap_output <- metagen(bifap_input,
#                       TE = yi, seTE = sei, studlab = dap,
#                       sm = "IRR", lower = lci, upper = uci,
#                       random = F, fixed = F,
#                       subgroup = label2,
#                       n.e = n_events, n.c = atrisk_ids,
#                       label.e = "events", label.c = "N")
# 
# forest.meta(bifap_output,
#             overall = F,
#             overall.hetstat = F,
#             test.subgroup = F,
#             print.subgroup.name = F,
#             text.subgroup.nohet = T,
#             label.left = "lower risk", label.right = "higher risk",
#             xlim = c(0.1, 5),
#             leftcols = c("studlab"),
#             rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
#             just = "center",
#             smlab = "",
#             print.tau2 = F,
#             fs.heading = 10,
#             fontsize = 10,
#             colgap.forest.left = unit(10, "mm"),
#             sortvar = studlab)
# 
# ## REVIEWER 3 comments on discussion
# # about the effect of COVID and it being a confounder
# # should show difference in effect between unadjusted and adjusted models
# 
# unadj_input <- scri_data %>%
#   filter(analysis == "main" & stratum == "no_split" & selection == "alldata") %>%
#   mutate(dose = trimws(stri_extract_first_regex(all_cat, "dose .{1}")),
#          caltime_adjustment = recode_factor(caltime_adjustment,
#                                      "no_adj" = "unadjusted",
#                                      "30d" = "adjusted"),
#          label = str_c(event, caltime_adjustment, dose, sep = "_"),
#          label2 = factor(label, levels = c("myocarditis_unadjusted_dose 1", "myocarditis_adjusted_dose 1",
#                                            "myocarditis_unadjusted_dose 2", "myocarditis_adjusted_dose 2", 
#                                            "pericarditis_unadjusted_dose 1", "pericarditis_adjusted_dose 1",
#                                            "pericarditis_unadjusted_dose 2", "pericarditis_adjusted_dose 2",
#                                            "myopericarditis_unadjusted_dose 1", "myopericarditis_adjusted_dose 1",
#                                            "myopericarditis_unadjusted_dose 2", "myopericarditis_adjusted_dose 2"),
#                          labels = c("myocarditis dose 1 - unadjusted", "myocarditis dose 1 - adjusted",
#                                     "myocarditis dose 2 - unadjusted", "myocarditis dose 2 - adjusted",
#                                     "pericarditis dose 1 - unadjusted", "pericarditis dose 1 - adjusted",
#                                     "pericarditis dose 2 - unadjusted", "pericarditis dose 2 - adjusted",
#                                     "myopericarditis dose 1 - unadjusted", "myopericarditis dose 1 - adjusted",
#                                     "myopericarditis dose 2 - unadjusted", "myopericarditis dose 2 - adjusted"))) %>%
#   filter(!is.na(dose)) %>%
#   filter(grepl("0", all_cat) == F) %>%
#   filter(grepl(">", all_cat) == F)
# 
#   
# unadj_output <- metagen(unadj_input,
#                       TE = yi, seTE = sei, studlab = dap,
#                       sm = "IRR", lower = lci, upper = uci,
#                       random = T, fixed = F,
#                       subgroup = label2,
#                       n.e = n_events, n.c = atrisk_ids,
#                       label.e = "events", label.c = "N")
# 
# forest.meta(unadj_output,
#             overall = F,
#             overall.hetstat = F,
#             test.subgroup = F,
#             print.subgroup.name = F,
#             text.subgroup.nohet = F,
#             label.left = "lower risk", label.right = "higher risk",
#             xlim = c(0.1, 5),
#             leftcols = c("studlab"),
#             rightlabs = c("Incidence\n Rate Ratio", "95%\n Confidence Interval"),
#             just = "center",
#             smlab = "",
#             print.tau2 = F,
#             fs.heading = 10,
#             fontsize = 10,
#             colgap.forest.left = unit(10, "mm"),
#             sortvar = studlab)

# export as PDF in size 8inch x 17inch (probably little longer because SIDIAP needs to be included as well)
