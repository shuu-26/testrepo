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

# Import and Format Data-------------------------------------------------------------
# Updated to output of step_12_2 that Svetlana finalised
# path probably needs adjusting to anDREa environment, but function should work
# the R object name does not contain DAP name, so cannot load them all in one
# load each one separately and give it a name
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/scri_models_A.RData")
CPRD <- scri_models_A

# added this to test code but needs to be updated after data actually become available!
# ARS <- scri_models_A
# BIFAP <- scri_models_A
# PHARMO <- scri_models_A

# We need a dataset with all information from the DAPs together because meta-analysis will be across DAPs
# to do this without creating confusion, we need to add variables that denote the source DAP and vaccine type per DAP dataset
# so per DAP (as denoted in dlab), we create a dataset with all analysis output plus 2 variables namely 
# 'DAP' to denote source and 'vacctype' to denote vaccine type
# we then select only the models with all exposure windows (called "buf_betwmodel_A_all_with_day0")
# and create a dataframe containing all these model outcomes, which we format a little bit (e.g. update variable names etc.)
# the result of the loop is the object dap_sets, a list with a properly formatted dataset for each DAP

# set-up; can be adapted by changing 'dlab'
dlab <- c("CPRD", "ARS", "BIFAP", "PHARMO")
dap_sets <- vector(mode = "list", length = length(dlab))
names(dap_sets) <- dlab

# start of the for-loop
for (name in 1:length(dlab)) {

  subset <- get(dlab[name])
  for (i in 1:length(subset)) {
    # add the dap name as a variable
    subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
      mutate(dap = dlab[name])
    
    # add variable with correct vaccine brand (using the if-statement)
    if(grepl("Astra", names(subset)[[i]])) {
      subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
        mutate(vacctype = "AstraZeneca")
      }
    
    if(grepl("Pfizer", names(subset)[[i]])) {
      subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
        mutate(vacctype = "Pfizer")
    }
    
    if(grepl("Moderna", names(subset)[[i]])) {
      subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
        mutate(vacctype = "Moderna")
    }
    
    if(grepl("J&J", names(subset)[[i]])) {
      subset[[i]][["tab"]] <- subset[[i]][["tab"]] %>%
        mutate(vacctype = "J&J")
    }
  }
  
  # select only the complete information model and then only the "tab" entry of that
  subset <- subset[which(grepl("buf_betwmodel", names(subset))==TRUE)]
  subset <- unlist(subset, recursive = F)
  subset <- subset[which(grepl(".tab", names(subset))==TRUE)]
  subset <- subset[which(grepl(".tab_", names(subset))==FALSE)]
  
  # combine the vaccine subsets into one dap set
  dap_set <- bind_rows(subset) %>%
    select(-c(all_cat2)) 
  
  # and then save the set in the dap_sets list
  dap_sets[[name]] <- dap_set
  }

# combine all dap subsets into one dataset
scri_data <- data.frame()
for (i in 1:length(dlab)) {
  scri_data <- rbind(scri_data, dap_sets[[i]])
}

# variable names do not match with the meta-analysis code and we miss some variables
# fix this before moving on the the analysis step

scri_data <- scri_data %>%
  # variable names with nonalpha characters are hard to read in R so change this
  rename(
    irr = RR,
    lci = `2.5 %`,
    uci = `97.5 %`,
    yi = coef,
    sei = `se(coef)`,
    pval = `Pr(>|z|)`) %>%
  # reference category rows now have missing values for irr and yi 
  # fix this so you can plot/tabulate them;
  mutate_at(.vars = vars(irr, lci, uci),
            .funs = ~ ifelse(is.na(.), 1, .)) %>%
  mutate_at(.vars = vars(yi, sei),
            .funs = ~ ifelse(is.na(.), 0, .)) %>%
  # create a risk window label that is understandable and a variable for adjustment and outcome
  # because these are needed for the create_tab1 function in meta-analysis
  mutate(label = factor(vd, levels = c(1,2,3,4,5,6,7),
                        labels = c("control window", "buffer period",
                                   "dose 1 day 0", "dose 1 risk window",
                                   "in between doses", "dose 2 day 0",
                                   "dose 2 risk window")),
         analysis = "unadjusted",
         eventtype = "Myopericarditis")


# Running the meta-analysis -----------------------------------------------

# Creating Table 1 
create_tab1 <- function(adjustment, riskwindow, outcome) {
  #' @title Create Table 1 
  #' @description This function takes the merged SCRI output data
  #' from the different Data Access Providers (DAPs) and applies
  #' random effects meta-analysis. It returns a meta object with the
  #' meta-analysis results
  #' @param adjustment The model type (adjusted or unadjusted)
  #' @param riskwindow The risk window under evaluation (dose 1 or dose 2)
  #' @param outcome The outcome under evaluation (myocarditis, pericarditis, or first of either)
  #' @return A meta object with the meta-analysis results
  meta_analysis <- metagen(data = scri_data %>% filter(analysis == adjustment & label == riskwindow &
                                                    eventtype == outcome),
                           TE = yi, seTE = sei, studlab = dap,
                           sm = "IRR", lower = lci, upper = uci,
                           random = T, fixed = F,
                           subgroup = vacctype)
  return(meta_analysis)
}

# Table 1 requires results per outcome, so the code loops over those
# to create a results table per outcome in the vector tab1
# The table contains unadjusted and adjusted results for each risk window
# and is saved per outcome as a csv output file
# Number of outcomes can be adapted by changing the outcomes variable
outcomes <- c("Myopericarditis", "Myocarditis", "Pericarditis")
tab1 <- vector(mode = "list", length = length(outcomes))
names(tab1) <- outcomes

for (i in 1:length(outcomes)) {
  # run analyses
  u_dose1 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  u_dose2 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])
  # a_dose1 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  # a_dose2 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])
  
  # create table
  tab <- data.frame(
    vacc = u_dose1$bylevs,
    irr_1 = exp(u_dose1$TE.random.w),
    lci_1 = exp(u_dose1$lower.random.w),
    uci_1 = exp(u_dose1$upper.random.w),
    irr_2 = exp(u_dose2$TE.random.w),
    lci_2 = exp(u_dose2$lower.random.w),
    uci_2 = exp(u_dose2$upper.random.w))
  
  # save in vector
  tab1[[i]] <- tab
  }

# Note to self: file specification should be updated for real analyses
write.csv(tab1[["Myopericarditis"]],
          file = "../results/output/myocarditis_table.csv")

write.csv(tab1[["Myocarditis"]],
          file = "../results/output/myocarditis_table.csv")

write.csv(tab1[["Pericarditis"]],
          file = "../results/output/pericarditis_table.csv")


## FOREST PLOTS --------------------------------------------------------------------

# create forest plot that displays IRRs per DAP for each vaccine type
# you need to run the metagen and specifcy plot details before creating the forest plot
# we want a plot that shows the number of cases and events
# and we want to add some x-axis labels

# remove the day0 rows because they mess up the plot
plot_data <- scri_data %>%
  filter(label != "dose 1 day 0" & label != "dose 2 day 0")


# creating the set for the forest plot (in this case CPRD only, all risk windows, per vaccine)
plot_set <- metagen(data = plot_data %>% filter(dap == "CPRD"),
                    TE = yi, seTE = sei, studlab = label, sm ="IRR",
                    lower = lci, upper = uci, random=F, fixed=F,
                    subgroup = vacctype,
                    n.c = atrisk_persons, n.e = n_events,
                    label.e = "events", label.c = "cases",
                    label.left = "lower risk", label.right = "higher risk")


# then plot
forest.meta(plot_set,
            #sortvar = TE,
            smlab = "Incidence Rate Ratio",
            pooled.events = F,
            pooled.times = F,
            pooled.totals = F,
            print.subgroup.name = F,
            fs.heading = 10
            # print.tau2 = F,
            # leftcols = c("dap", "atrisk_persons", "n_events"),
            # leftlabs = c("Data source", "Cases", "Events"),
            # just.addcols = "left"
            )




# Sensitivity Analyses ----------------------------------------------------