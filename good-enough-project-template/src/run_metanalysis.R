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

# Import and Format Data-------------------------------------------------------------
# Updated to output of step_12_2 that Svetlana finalised
# path probably needs adjusting to anDREa environment, but function should work


# load in datasets and turn them into more easily manageable names
# CPRD
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A_age.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A_sex_age.RData")

# ARS
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/ARS_scri_models_A.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/ARS_scri_models_A_age.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/ARS_scri_models_A_sex_age.RData")

# BIFAP

# PHARMO

# so the datasets are structured per DAP, vaccine type, and stratum
# stratified analyses all have their own set with the strata in the name
# DAP name is given in the list name (e.g. CPRD_, ARS_, etc.)
# vaccine type is given in the sublist name (e.g. AstraZeneca, Pfizer, etc.)
# as is stratum for stratified analyses (e.g. age(-18,30], etc.)
# create loop for all datasets that extracts this information into columns (e.g. variables)
# and store each cleaned dataset in a list called type_sets

# loop will include all sets specified in 'names', so make sure this is complete
names <- c("CPRD_scri_models_A", "CPRD_scri_models_A_age", "CPRD_scri_models_A_sex_age",
           "ARS_scri_models_A", "ARS_scri_models_A_age", "ARS_scri_models_A_sex_age")
type_sets <- vector(mode = "list", length = length(names))
names(type_sets) <- names

for (name in 1:length(names)) {
  subset <- get(names[name])
  
  for (i in 1:length(subset)) {
    # add the dap name, vaccine type and stratum as variables
    # using regex based on
    # name of dataset in case of dap
    # name of subset name in case of vaccine type and stratum
    # dap and vacctype both take first word (e.g. 'alpha') as value
    # stratum takes text between _ and ] as value
    subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
      mutate(dap = stri_extract_first_regex(names[name], pattern = "[:alpha:]+"),
             vacctype = stri_extract_first_regex(names(subset)[i], pattern = "[:alpha:]+"),
             stratum = stri_extract_all_regex(names(subset)[i], pattern = "(?<=\\_).*\\]", simplify = T))
  }
  
  # select only the complete information model and then only the "tab" entry of that
  subset <- subset[which(grepl("buf_betw", names(subset))==TRUE)]
  subset <- unlist(subset, recursive = F)
  subset <- subset[which(grepl(".tab", names(subset))==TRUE)]
  subset <- subset[which(grepl(".tab_", names(subset))==FALSE)]
  
  # combine the vaccine subsets into one type set
  type_set <- bind_rows(subset)
  
  # and then save the set in the type_sets list
  type_sets[[name]] <- type_set
}
    
# combine all dap subsets into one dataset
scri_data <- data.frame()
for (i in 1:length(names)) {
  scri_data <- rbind(scri_data, type_sets[[i]])
}

# variable names do not match with the meta-analysis code and we miss some variables
# fix this before moving on the the analysis step

scri_data <- scri_data %>%
  # variable names with nonalpha characters are hard to read in R so change this
  rename(
    irr = RR,
    lci = `2.5%`,
    uci = `97.5%`,
    yi = coef,
    sei = `se(coef)`,
    pval = `Pr(>|z|)`) %>%
  # reference category rows now have missing values for irr and yi 
  # fix this so you can plot/tabulate them;
  mutate_at(.vars = vars(irr, lci, uci),
            .funs = ~ ifelse(is.na(.), 1, .)) %>%
  mutate_at(.vars = vars(yi, sei),
            .funs = ~ ifelse(is.na(.), 0, .)) %>%
  # censor numbers < 5 but this breaks metagen so THINK ABOUT THIS
  # mutate_at(.vars = vars(n_events, atrisk_persons),
  #           .funs = ~ ifelse(. < 5, "<5", .)) %>%
  # create a risk window label that is understandable and a variable for adjustment and outcome
  # because these are needed for the create_tab1 function in meta-analysis
  mutate(label = factor(vd, levels = c(1,2,3,4,5,6,7),
                        labels = c("control window", "buffer period",
                                   "dose 1 day 0", "dose 1 risk window",
                                   "in between doses", "dose 2 day 0",
                                   "dose 2 risk window")),
         analysis = "unadjusted",
         eventtype = "myopericarditis") %>%
  # clean up the stratum variable so the levels are meaningful
  mutate(subgroup = ifelse(is.na(stratum), "all", stratum),
         subgroup = factor(subgroup,
                          levels = c("all", "age(-1,30]", "age(30,120]", "sex0_age(-1,30]", "sex0_age(30,120]",
                                     "sex1_age(-1,30]", "sex1_age(30,120]"),
                          labels = c("all", "age_under30", "age_30up", "women_under30", "women_30up", 
                                     "men_under30",  "men_30up"))) %>%
  # relabel vacctype "J" to "J&J"
  mutate(vacctype = ifelse(vacctype == "J", "J&J", vacctype))

# Running the meta-analysis -----------------------------------------------

# Creating Table 1 function
create_tab1 <- function(adjustment = "unadjusted", 
                        riskwindow, 
                        outcome = "myopericarditis", 
                        data
                        ) {
  #' @title Create Table 1 
  #' @description This function takes the merged SCRI output data
  #' from the different Data Access Providers (DAPs) and applies
  #' random effects meta-analysis stratified by vaccine brand. It returns a meta object with the
  #' meta-analysis results
  #' @param adjustment The model type (adjusted or unadjusted); default set to unadjusted
  #' @param riskwindow The risk window under evaluation (dose 1 or dose 2)
  #' @param outcome The outcome under evaluation (myoperi, myo, or pericarditis); default set to myopericarditis
  #' @param data The dataset that serves as input
  #' @return A meta object with the meta-analysis results

  meta_analysis <- metagen(data = data %>% filter(analysis == adjustment &
                                                  eventtype == outcome &
                                                  label == riskwindow),
                           TE = yi, seTE = sei, studlab = dap,
                           sm = "IRR", lower = lci, upper = uci,
                           random = T, fixed = F,
                           subgroup = vacctype,
                           n.e = n_events, n.c = atrisk_persons,
                           label.e = "events", label.c = "N")
  return(meta_analysis)
}

# can create tables per many different things (outcomes, strata, adjustment models, risk windows)
# for now we only have 1 outcome and 1 adjustment model
# so let's loop over strata
strata <- c("all", "age_under30", "age_30up", "women_under30", "women_30up", 
            "men_under30",  "men_30up")
tab_strata <- vector(mode = "list", length = length(strata))
tab_ma <- vector(mode = "list", length = length(strata))
names(tab_strata) <- strata
names(tab_ma) <- strata

for (i in 1:length(strata)) {
  
  # select the correct stratum
  data_noall <- scri_data %>% filter(vacctype != "all")
  selection <- data_noall %>% filter(subgroup == strata[i])
  
  # run analyses
  u_dose1 <- create_tab1(data = selection, riskwindow = "dose 1 risk window")
  u_dose2 <- create_tab1(data = selection, riskwindow = "dose 2 risk window")
  
  # create table
  tab1 <- data.frame(
    vacc = u_dose1$bylevs,
    irr_1 = exp(u_dose1$TE.random.w),
    lci_1 = exp(u_dose1$lower.random.w),
    uci_1 = exp(u_dose1$upper.random.w),
    stratum = strata[i],
    N_persons = u_dose1$n.c.w,
    N_events = u_dose1$n.e.w)
  
  tab2 <- data.frame(
    vacc = u_dose2$bylevs,
    irr_2 = exp(u_dose2$TE.random.w),
    lci_2 = exp(u_dose2$lower.random.w),
    uci_2 = exp(u_dose2$upper.random.w),
    stratum = strata[i],
    N_persons = u_dose2$n.c.w,
    N_events = u_dose2$n.e.w)
  
  # save meta-analyses in vector
  tab_ma[[i]][["dose1"]] <- u_dose1
  tab_ma[[i]][["dose2"]] <- u_dose2
  
  # save in vector
  tab_strata[[i]][["dose1"]] <- tab1
  tab_strata[[i]][["dose2"]] <- tab2
}

# create one table with results for all strata together
# separate tables for dose1 and dose2 because they differ in column names
myoperi_strata <- unlist(tab_strata, recursive = F)

# round off the risk estimates and censor numbers < 5
for (i in 1:length(myoperi_strata)) {
  myoperi_strata[[i]] <- myoperi_strata[[i]] %>%
    mutate_at(.vars = vars(-c(vacc, stratum, N_persons, N_events)),
              .funs = ~round(., digits = 2)) %>%
    mutate_at(.vars = vars(N_events, N_persons),
              .funs = ~ifelse(. < 5, "< 5", as.character(.)))
}

# and extract the dose1 and dose2 tables
myoperi_dose1 <- bind_rows(myoperi_strata[grepl("dose1", names(myoperi_strata))])
myoperi_dose2 <- bind_rows(myoperi_strata[grepl("dose2", names(myoperi_strata))])


# Note to self: file specification should be updated for real analyses
write.csv2(myoperi_dose1,
          file = "//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_figure/Table_metanalysed_per_stratum_dose1.csv",
          row.names = F)

write.csv2(myoperi_dose2,
           file = "//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_figure/Table_metanalysed_per_stratum_dose2.csv",
           row.names = F)


## FOREST PLOTS --------------------------------------------------------------------

# create forest plot that displays IRRs per DAP for each vaccine type
# you need to run the metagen and specifcy plot details before creating the forest plot
# we want a plot that shows the number of cases and events
# and we want to add some x-axis labels

create_plot <- function(subgroup,
                        dose,
                        data
){
  #' @title Create forest plots per subgroup and dose
  #' @description This function takes the create_tab1 output data
  #' It returns a forest plot
  #' @param subgroup The subgroup of interest
  #' @param dose The dose of interest
  #' @param data The dataset, which should be a list of meta-analysis results
  #' @return A forest plot
  
  plot <- forest.meta(data[[subgroup]][[dose]],
              overall = F,
              overall.hetstat = F,
              test.subgroup = F,
              label.left = "lower risk", label.right = "higher risk")

}

# somehow we cannot loop over plots because it doesn't generate objects
# also cannot ggsave, annoying
# saving as pdf with 6x8 inches seems to work
# save each one by hand *sigh*
# naming convention: Plot_subgroup_dose

# whole population
create_plot(subgroup = "all",
            dose = "dose1",
            data = tab_ma)

create_plot(subgroup = "all",
            dose = "dose2",
            data = tab_ma)

# under 30
create_plot(subgroup = "age_under30",
            dose = "dose1",
            data = tab_ma)

create_plot(subgroup = "age_under30",
            dose = "dose2",
            data = tab_ma)

# over 30
create_plot(subgroup = "age_30up",
            dose = "dose1",
            data = tab_ma)

create_plot(subgroup = "age_under30",
            dose = "dose2",
            data = tab_ma)

# women / age
create_plot(subgroup = "women_under30",
            dose = "dose1",
            data = tab_ma)
create_plot(subgroup = "women_under30",
            dose = "dose2",
            data = tab_ma)
create_plot(subgroup = "women_30up",
            dose = "dose1",
            data = tab_ma)
create_plot(subgroup = "women_30up",
            dose = "dose2",
            data = tab_ma)

# men / age
create_plot(subgroup = "men_under30",
            dose = "dose1",
            data = tab_ma)
create_plot(subgroup = "men_under30",
            dose = "dose2",
            data = tab_ma)
create_plot(subgroup = "men_30up",
            dose = "dose1",
            data = tab_ma)
create_plot(subgroup = "men_30up",
            dose = "dose2",
            data = tab_ma)


# Sensitivity Analyses ----------------------------------------------------