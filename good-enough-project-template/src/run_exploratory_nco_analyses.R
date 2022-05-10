#### 
### Negative controls sensitivity analysis
###

## loading packages

library(tidyverse)
library(haven)
library(lubridate)


# loading data
pat <- read_sas("F:/Projects/EMA-tender/CVM/Gout/persons_subset.sas7bdat")
vax <- read_sas("F:/Projects/EMA-tender/CVM/Gout/vaccines_subset.sas7bdat")
obs <- read_sas("F:/Projects/EMA-tender/CVM/Gout/observation_periods_subset.sas7bdat")
event <- read_sas("F:/Projects/EMA-tender/CVM/Gout/events_subset.sas7bdat")
#med <- read_sas("F:/Projects/EMA-tender/CVM/Gout/medicines_subset.sas7bdat")
nco_codes <- read.delim("F:/Projects/EMA-tender/CVM/Gout/codelist_CVM_gout.txt",
                        colClasses = "character", 
                       col.names = c("coding_system", "code", "code_name", "concept", "concept_name",
                                     "tags", "variable_name", "system", "event_abbreviation", 
                                     "type_event"))


## TURN THE RAW DATA INTO D3 FORMAT
# STEP 1 - make subsets of all gout cases and all vaccinated individuals

# first get all gout cases based on the code list
gout <- nco_codes %>%
  filter(event_abbreviation == "GOUT")
df_gout <- event %>%
  filter(event_code %in% gout$code) %>%
  select(person_id, start_date_record, event_code, text_linked_to_event_code) %>%
  # removing weird entries with year 9999, those are probably unknown year entries
  filter(year(start_date_record) < 2023) %>%
  # check if first event is inside study period starting 01Sep2020
  arrange(person_id, as.Date(start_date_record)) %>%
  group_by(person_id) %>%
  mutate(gout_instance = row_number()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(event_type = case_when(
    gout_instance == 1 & as.Date(start_date_record) >= as.Date("2020-09-01") ~ "first - in study period",
    gout_instance == 1 & as.Date(start_date_record) < as.Date("2020-09-01") ~ "first - before study start",
    gout_instance > 1 ~ "recurrent flare"
  ))


ggplot(df_gout) +
  geom_histogram(aes(x = start_date_record, fill = event_type), binwidth = 15, position = "dodge") +
  coord_cartesian(xlim=c(as.Date("2019-09-01"), as.Date("2021-05-09")))

# analysis will serve as NCO to myocarditis, where we only use first event 
# from people without events in previous year --> select only those

df_incidentgout <- df_gout %>%
  filter(event_type == "first - in study period")

# then reformat vaccine dataset so each row represents one person
vax_wide <- vax %>%
  select(person_id, vx_record_date, vx_admin_date, vx_manufacturer, vx_dose) %>%
  pivot_wider(
    id_cols = person_id,
    names_from = vx_dose,
    values_from = c(vx_manufacturer, vx_record_date, vx_admin_date)) %>%
  rename(date_vax1 = vx_admin_date_1,
         date_vax2 = vx_admin_date_2,
         type_vax1 = vx_manufacturer_1,
         type_vax2 = vx_manufacturer_2) %>%
  mutate(date_vax2 = ifelse(!is.na(date_vax2) &
                              as.Date(date_vax2) - as.Date(date_vax1) < 21, 
                            as.character(vx_admin_date_3), 
                            as.character(date_vax2)),
         type_vax2 = ifelse(!is.na(type_vax2) &
                               as.Date(date_vax2) - as.Date(date_vax1) < 21, 
                             vx_manufacturer_3, type_vax2)) %>%
  select(-contains("vx")) %>%
  distinct()

# STEP 2 - make subset with data availability information
df_availability <- obs %>%
  mutate(fu_1y = ifelse(as.Date("2020-09-01") - as.Date(op_start_date) > 365, 1, 0)) %>%
  select(person_id, op_start_date, op_end_date, fu_1y) %>%
  rename(date_entry = op_start_date,
         date_exit = op_end_date) %>%
  distinct()

# STEP 3 - make subset with sex, date of birth, date of death
df_pat <- pat %>%
  mutate_at(.vars = vars(contains("death")),
            .funs = ~trimws(.)) %>%
  mutate_at(.vars = vars(contains("death")),
            .funs = ~ifelse(. == ".", NA, .)) %>%
  unite(date_death, c(day_of_death, month_of_death, year_of_death), sep = "-", remove=T,
        na.rm=T) %>%
  mutate(date_death = parse_date_time(date_death, orders = "dmy"),
         year_of_birth = as.numeric(as.character(year_of_birth))) %>%
  select(person_id, sex_at_instance_creation, year_of_birth, date_death)

# STEP 4 - make subset with COVID disease information
df_covid <- event %>%
  filter(grepl("cov", text_linked_to_event_code, ignore.case = T)) %>%
  rename(covid_19_date = start_date_record) %>%
  select(person_id, covid_19_date)

# FINAL STEP - combine subsets to get study population
df_population <- df_incidentgout %>%
  inner_join(vax_wide, by = "person_id") %>%
  left_join(df_availability, by = "person_id") %>%
  left_join(df_pat, by = "person_id") %>%
  left_join(df_covid, by = "person_id") %>%
  # exclude those with less than 1 year of follow-up (not due to being born in 2020)
  mutate(excl = ifelse(year(date_entry) != year_of_birth & fu_1y == 0, 1, 0)) %>%
  filter(excl == 0) %>%
  mutate(
    study_entry_date = max(as.Date("2020-09-01"), as.Date(date_entry)),
    study_exit_date = min(as.Date(date_death), as.Date(date_exit)),
    age_at_study_entry = 2020 - year_of_birth,
    DAP = "CPRD") %>%
  distinct() %>%
  select(-c(excl, event_type, gout_instance, fu_1y, event_code, year_of_birth)) %>%
  rename(
    date_gout = start_date_record,
    sex = sex_at_instance_creation)

# save the dataset
write.csv(df_population,
          file = "F:/Projects/EMA-tender/CVM/Gout/20220510_Gout analysis population file.csv")
