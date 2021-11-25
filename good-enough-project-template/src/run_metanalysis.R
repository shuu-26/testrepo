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

# CPRD
# load in datasets and turn them into more easily manageable names
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A_age.RData")
load("//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_output/scri/scri/CPRD_scri_models_A_sex_age.RData")


# so the datasets are structured per DAP, vaccine type, and stratum
# stratified analyses all have their own set with the strata in the name
# DAP name is given in the list name (e.g. CPRD_, ARS_, etc.)
# vaccine type is given in the sublist name (e.g. AstraZeneca, Pfizer, etc.)
# as is stratum for stratified analyses (e.g. age(-18,30], etc.)
# create loop for all datasets that extracts this information into columns (e.g. variables)
# and store each cleaned dataset in a list called type_sets

# loop will include all sets specified in 'names', so make sure this is complete
names <- c("CPRD_scri_models_A", "CPRD_scri_models_A_age", "CPRD_scri_models_A_sex_age")
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
             stratum = stri_extract_all_regex(names(subset)[i], pattern = "(?<=\\_).*\\]"))
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
  mutate(stratum = ifelse(is.na(stratum), "all", stratum),
         stratum = factor(stratum,
                          levels = c("all", "age(-1,30]", "age(30,120]", "sex0_age(-1,30]", "sex0_age(30,120]",
                                     "sex1_age(-1,30]", "sex1_age(30,120]"),
                          labels = c("all", "age_under30", "age_30up", "women_under30", "women_30up", 
                                     "men_under30",  "men_30up")),
         subgroup = as.character(stratum))

# Running the meta-analysis -----------------------------------------------

# Creating Table 1 function
# please note that dataset is hardcoded in the function and is set to scri_data as created in the data formatting above
create_tab1 <- function(adjustment = "unadjusted", 
                        riskwindow, 
                        outcome = "myopericarditis", 
                        stratum = "all") {
  #' @title Create Table 1 
  #' @description This function takes the merged SCRI output data
  #' from the different Data Access Providers (DAPs) and applies
  #' random effects meta-analysis stratified by vaccine brand. It returns a meta object with the
  #' meta-analysis results
  #' @param adjustment The model type (adjusted or unadjusted); default set to unadjusted
  #' @param riskwindow The risk window under evaluation (dose 1 or dose 2)
  #' @param outcome The outcome under evaluation (myoperi, myo, or pericarditis); default set to myopericarditis
  #' @param stratum The subgroup (stratum) under evaluation; default is all
  #' @return A meta object with the meta-analysis results
  meta_analysis <- metagen(data = scri_data %>% filter(analysis == adjustment & 
                                                         label == riskwindow &
                                                         eventtype == outcome & 
                                                         stratum == stratum),
                           TE = yi, seTE = sei, studlab = dap,
                           sm = "IRR", lower = lci, upper = uci,
                           random = T, fixed = F,
                           subgroup = vacctype)
  return(meta_analysis)
}

### NB SEEMS NOT TO PERFORM SELECTION ON STRATUM PROPERLY SO LOOK INTO THIS ###

# can create tables per many different things (outcomes, strata, adjustment models, risk windows)
# for now we only have 1 outcome and 1 adjustment model
# so let's loop over strata

# take strata levels from the 'stratum' variable in the scri_data (so it is adapted if more strata become available)
strata <- unique(scri_data$stratum)
vaccine <- unique(scri_data$vacctype)

tab_strata <- vector(mode = "list", length = length(strata))
names(tab_strata) <- strata

for (i in 1:length(strata)) {
  
  # run analyses
  u_dose1 <- create_tab1(riskwindow = "dose 1 risk window")
  u_dose2 <- create_tab1(riskwindow = "dose 2 risk window")
  
  # create table
  tab <- data.frame(
    vacc = u_dose1$bylevs,
    irr_1 = exp(u_dose1$TE.random.w),
    lci_1 = exp(u_dose1$lower.random.w),
    uci_1 = exp(u_dose1$upper.random.w),
    irr_2 = exp(u_dose2$TE.random.w),
    lci_2 = exp(u_dose2$lower.random.w),
    uci_2 = exp(u_dose2$upper.random.w),
    stratum = strata[i])
  
  # save in vector
  tab_strata[[i]] <- tab
}

# create one table with results for all strata together
myoperi_strata <- bind_rows(tab_strata) %>%
  mutate_at(.vars = vars(-c(vacc, stratum)),
            .funs = ~round(., digits = 2))

# Note to self: file specification should be updated for real analyses
write.csv2(myoperi_strata,
          file = "//soliscom.uu.nl/users/3911195/My Documents/GitHub/CVM/g_figure/Table_metanalysed_per_stratum.csv",
          row.names = F)


## FOREST PLOTS --------------------------------------------------------------------

# create forest plot that displays IRRs per DAP for each vaccine type
# you need to run the metagen and specifcy plot details before creating the forest plot
# we want a plot that shows the number of cases and events
# and we want to add some x-axis labels

# remove the day0 rows because they mess up the plot
plot_data <- scri_data %>%
  filter(label != "dose 1 day 0" & label != "dose 2 day 0")


# creating the set for the forest plot (in this case CPRD only, all risk windows, per vaccine)
plot_set <- metagen(data = plot_data %>% filter(subgroup == "all"),
                    TE = yi, seTE = sei, studlab = label, sm ="IRR",
                    lower = lci, upper = uci, random=F, fixed=F,
                    subgroup = vacctype,
                    n.c = atrisk_persons, n.e = n_events,
                    label.e = "myoperi", label.c = "N",
                    label.left = "lower risk", label.right = "higher risk")


# then plot
forest.meta(plot_set,
            #sortvar = TE,
            smlab = "Incidence Rate Ratio",
            pooled.events = F,
            pooled.times = F,
            pooled.totals = F,
            print.subgroup.name = F,
            fs.heading = 8,
            xlim = c(0.5, 5)
            # print.tau2 = F,
            # leftcols = c("dap", "atrisk_persons", "n_events"),
            # leftlabs = c("Data source", "Cases", "Events"),
            # just.addcols = "left"
            )




# Sensitivity Analyses ----------------------------------------------------