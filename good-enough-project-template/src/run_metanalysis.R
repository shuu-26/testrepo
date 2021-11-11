# Program Information  ----------------------------------------------------

# Program:      run_metanalysis
# Author:       Sophie Bots
# Description:  meta-analyse aggregated scri output data
# Requirements:
#               input:  data in csv with 1 row per analysis stratum
#               output: table 1, table 2, figures

# Housekeeping  -----------------------------------------------------------

## install and load packages
library(tidyverse)
library(meta)

# Import Data -------------------------------------------------------------
# need to confirm folder structure on the anDREa platform
# I also imagine each DAP uploads their CSV separately
# so I will need to mix and match a bit.
# I can write a suggestion, but I have no clue if this is how
# it'll work so probably will need to adapt when the actual data are there

all_results <- read.csv("../data/temp/results_per_dap.csv")

# check if reading the CSV has worked correctly and fix if it hasn't
# this can go wrong if the CSV format doesn't match the read.csv settings
if (ncol(all_results) == 1) {
  print("Warning: data file only has 1 row, code will attempt different read.csv() option to solve")
  all_results <- read.csv2("../data/temp/results_per_dap.csv")
  if (ncol(all_results) == 1) {
    stop("Data issue remains: troubleshoot required before continuing")
  }
  }

## Format data ---------------------------------------------------------
#-- code needed for dummy data to create datasets from different sources 
dlab <- c("BIFAP", "PHARMO", "ARS", "CPRD")
dap_sets <- vector(mode = "list", length = length(dlab))
names(dap_sets) <- dlab

# create dlab variable for each subset and save
for (i in 1:length(dlab)) {
  subset <- all_results %>%
    select(-X) %>%
    mutate(dlab = dlab[i])
  
  dap_sets[[i]] <- subset
}

# add all subsets together to make one set with all results
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
# meta-analysis should be done:
# (a) for each main analysis subtype
# (b) for each sensitivity analysis subtype
# for both: meta-analyse across DAPs separately for each
# - vaccine type
# - risk window
# - adjustment type (unadjusted / adjusted for seasonality)

## TABLE 1 ------------------------------------------------

# write function so it's easy to change settings without having to copy-paste code
create_tab1 <- function(adjustment, riskwindow, outcome) {
  meta_analysis <- metagen(data = scri_data %>% filter(analysis == adjustment & label == riskwindow &
                                                    eventtype == outcome),
                           TE = yi, seTE = sei, studlab = dlab,
                           sm = "IRR", lower = lci, upper = uci,
                           random = T, fixed = F,
                           subgroup = vacctype)
  return(meta_analysis)
}

# loop over the different outcomes
# this can also easily be adjusted to accommodate more outcomes
outcomes <- c("Myocarditis", "Pericarditis")
tab1 <- vector(mode = "list", length = length(outcomes))
names(tab1) <- outcomes

for (i in 1:seq_len(outcomes)) {
  # Run analysis
  u_dose1 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  u_dose2 <- create_tab1(adjustment = "unadjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])
  a_dose1 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 1 risk window", outcome = outcomes[i])
  a_dose2 <- create_tab1(adjustment = "adjusted", riskwindow = "dose 2 risk window", outcome = outcomes[i])
  # make the table
  tab <- data.frame(
    vacc = u_dose1$bylevs,
    irr_1 = exp(u_dose1$TE.random.w),
    lci_1 = exp(u_dose1$lower.random.w),
    uci_1 = exp(u_dose1$upper.random.w),
    irr_2 = exp(u_dose2$TE.random.w),
    lci_2 = exp(u_dose2$lower.random.w),
    uci_2 = exp(u_dose2$upper.random.w))

  # save table
  tab1[[i]] <- tab
  }

# save tables to folder
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