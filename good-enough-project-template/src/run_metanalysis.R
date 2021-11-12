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
library(docstring)

# Import Data -------------------------------------------------------------
# Note to self: this is based on current GitHub structure
# needs to be updated to anDREa structure when actual data are available
all_results <- read.csv("../data/temp/results_per_dap.csv")

#- Data format check -#
# Input files may be structured as comma or semicolon separated, which use
# read.csv() and read.csv2() respectively.
# Mismatching these results in a dataset with only 1 row, which cannot be used
# This code checks whether that happens and fixes it if it does.
# If the fix doesn't work, it stops the code and output a meaningful error,
# so the user can start troubleshooting immediately.

if (ncol(all_results) == 1) {
  print("Warning: data file only has 1 row, code will attempt different read.csv() option to solve")
  all_results <- read.csv2("../data/temp/results_per_dap.csv")
  if (ncol(all_results) == 1) {
    stop("Data issue remains: troubleshoot required before continuing")
  }
  }

## Format data ---------------------------------------------------------
# Note to self: this is only required for current dummy data.
# needs to be updated based on structure of actual results data

# The dummy data does not contain the data label column needed for meta-analysis
# This code creates that column (dlab) for each DAP subset
# before combining subsets into the analysis dataset scri_data
# The number of DAPs can easily be adjusted by changing the dlab variable
dlab <- c("BIFAP", "PHARMO", "ARS", "CPRD")
dap_sets <- vector(mode = "list", length = length(dlab))
names(dap_sets) <- dlab

for (i in 1:length(dlab)) {
  subset <- all_results %>%
    select(-X) %>%
    mutate(dlab = dlab[i])
  
  dap_sets[[i]] <- subset
}

scri_data <- data.frame()
for (i in 1:length(dlab)) {
  scri_data <- rbind(scri_data, dap_sets[[i]])
}

## CURRENTLY WORKING ON THIS - NOT YET OPERATIONAL #
# check if data are in correct formats
# numvars <- c("irr", "lci", "uci", "sei", "yi")
# for (i in 1:length(numvars)) {
#   if(is.numeric(scri_data[, numvars[[i]]]) == FALSE) {
#     print(c(numvars[[i]], "Warning: not numeric, will attempt fix"))
#     scri_data[, numvars[[i]]] <- as.numeric(as.character(scri_data[, numvars[[i]]]))
#   }
#   if(abs(max(scri_data[, numvars[[i]]])) > 10) {
#      print(c(numvars[[i]],"Warning: max value larger than 10"))
#   }
# }

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
                           TE = yi, seTE = sei, studlab = dlab,
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
outcomes <- c("Myocarditis", "Pericarditis")
tab1 <- vector(mode = "list", length = length(outcomes))
names(tab1) <- outcomes

for (i in 1:length(outcomes)) {
  
  u_dose1 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  u_dose2 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])
  a_dose1 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  a_dose2 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])

  tab <- data.frame(
    vacc = u_dose1$bylevs,
    irr_1 = exp(u_dose1$TE.random.w),
    lci_1 = exp(u_dose1$lower.random.w),
    uci_1 = exp(u_dose1$upper.random.w),
    irr_2 = exp(u_dose2$TE.random.w),
    lci_2 = exp(u_dose2$lower.random.w),
    uci_2 = exp(u_dose2$upper.random.w))

  tab1[[i]] <- tab
  }

# Note to self: file specification should be updated for real analyses
write.csv(tab1[["Myocarditis"]],
          file = "../results/output/myocarditis_table.csv")

write.csv(tab1[["Pericarditis"]],
          file = "../results/output/pericarditis_table.csv")


## FOREST PLOTS --------------------------------------------------------------------
# placeholder now -- needs to be updated to actual data and functions
# this is example code and it does not yet work


# first turn data into required format for plotting function
meta_analysis <- metagen(data = results, TE = yi, seTE = sei, studlab = dlab, sm = "IRR",
                          lower = lci, upper = uci,
                          random = T, fixed = F, n.c = ncase, n.e = nevent,
                          label.e = "", label.c = "",
                          label.left = "lower risk", label.right = "higher risk")

# then plot
forest.meta(meta_analysis2,
            sortvar = TE,
            smlab = "Incidence Rate Ratio",
            print.tau2 = F,
            leftcols = c("dlab", "ncase", "nevent"),
            leftlabs = c("Data source", "Cases", "Events"),
            just.addcols = "left")

# Sensitivity Analyses ----------------------------------------------------