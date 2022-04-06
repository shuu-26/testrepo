#### 
### Negative controls sensitivity analysis
###

## loading packages

library(tidyverse)
library(haven)
library(lubridate)


## loading data
pat <- read_sas("F:/Projects/EMA-tender/CVM/Gout/persons_subset.sas7bdat")
vax <- read_sas("F:/Projects/EMA-tender/CVM/Gout/vaccines_subset.sas7bdat")
obs <- read_sas("F:/Projects/EMA-tender/CVM/Gout/observation_periods_subset.sas7bdat")
event <- read_sas("F:/Projects/EMA-tender/CVM/Gout/events_subset.sas7bdat")
med <- read_sas("F:/Projects/EMA-tender/CVM/Gout/medicines_subset.sas7bdat")
nco_codes <- read_tsv("F:/Projects/EMA-tender/CVM/Gout/codelist_CVM_gout.txt",
                       col_names = c("coding_system", "code", "code_name", "concept", "concept_name",
                                     "tags", "variable_name", "system", "event_abbreviation", 
                                     "type_event"))

# fixing stuff
# like making vax wide instead of long
vax_wide <- vax %>%
  select(person_id, vx_record_date, vx_admin_date, vx_manufacturer, vx_dose) %>%
  pivot_wider(
    id_cols = person_id,
    names_from = vx_dose,
    values_from = c(vx_manufacturer, vx_record_date, vx_admin_date)) %>%
  rename(date_vax1 = vx_admin_date_1,
         date_vax2 = vx_admin_date_2,
         brand_vax1 = vx_manufacturer_1,
         brand_vax2 = vx_manufacturer_2) %>%
  mutate(date_vax2 = ifelse(!is.na(date_vax2) &
                              as.Date(date_vax2) - as.Date(date_vax1) < 21, 
                            as.character(vx_admin_date_3), 
                            as.character(date_vax2)),
         brand_vax2 = ifelse(!is.na(date_vax2) &
                               as.Date(date_vax2) - as.Date(date_vax1) < 21, 
                             vx_manufacturer_3, brand_vax2)) %>%
  select(-contains("vx")) %>%
  distinct()

# and making date of death one variable
pat <- pat %>%
  mutate_at(.vars = vars(contains("death")),
            .funs = ~trimws(.)) %>%
  mutate_at(.vars = vars(contains("death")),
            .funs = ~ifelse(. == ".", NA, .)) %>%
  unite(date_of_death, c(day_of_death, month_of_death, year_of_death), sep = "-", remove=T,
        na.rm=T) %>%
  mutate(date_of_death = parse_date_time(date_of_death, orders = "dmy"))

## making selections

# NCO codes subsets
otitisext <- nco_codes %>%
  filter(event_abbreviation == "OTITISEXT")
gout <- nco_codes %>%
  filter(event_abbreviation == "GOUT")
trign <- nco_codes %>%
  filter(event_abbreviation == "TRIGEMINALNEURALGIA")

# data subsets
df_otitis <- event %>%
  filter(event_code %in% otitisext$code)
df_gout <- event %>%
  filter(event_code %in% gout$code)
df_trign <- event %>%
  filter(event_code %in% trign$code)
df_covid <- event %>%
  filter(grepl("cov", text_linked_to_event_code, ignore.case = T)) %>%
  rename(covid_date = start_date_record) %>%
  select(person_id, covid_date) %>%
  mutate(prior_covid = ifelse(between(as.Date(covid_date), 
                                      as.Date("2019-09-01"), 
                                      as.Date("2020-09-01")), 1, 0))
df_availability <- obs %>%
  mutate(fu_1y = ifelse(as.Date("2020-09-01") - as.Date(op_start_date) > 365, 1, 0)) %>%
  select(person_id, op_start_date, op_end_date, fu_1y) %>%
  rename(date_entry = op_start_date,
         date_exit = op_end_date) %>%
  distinct()

# let's go with otitis first
# make the set
data_otitis <- df_otitis %>%
  left_join(pat, by = "person_id") %>%
  left_join(vax_wide, by = "person_id") %>%
  select(person_id, start_date_record, text_linked_to_event_code, sex_at_instance_creation,
         year_of_birth, date_of_death, contains("vax")) %>%
  distinct() %>%
  filter(!is.na(date_vax1)) %>%
  rename(date_otitis = start_date_record) %>%
# counting the number of events
# there are only 34 that occur after the start of the control window
  rowwise() %>%
  mutate(
    control_event = ifelse(between(as.Date(date_otitis), 
                                   as.Date(date_vax1) - 90, 
                                   as.Date(date_vax1) - 30), 1, 0),
    rw1_event = ifelse(between(as.Date(date_otitis),
                               as.Date(date_vax1) + 1,
                               as.Date(date_vax1) + 28), 1, 0),
    rw2_event = ifelse(!is.na(date_vax2) & between(as.Date(date_otitis),
                               as.Date(date_vax2) + 1,
                               as.Date(date_vax2) + 28), 1, 0),
    between_event = ifelse(!is.na(date_vax2) & between(as.Date(date_otitis),
                                   as.Date(date_vax1) + 28,
                                   as.Date(date_vax2)), 1, 0),
    before_event = ifelse(as.Date(date_otitis) < as.Date(date_vax1) - 90, 1, 0)
  ) %>%
  ungroup()

data_otitis_unique <- data_otitis %>%
  arrange(date_otitis) %>%
  distinct(person_id, .keep_all = T) %>%
  arrange(person_id)

# getting the numbers
plyr::count(data_otitis_unique$before_event) # 27 after start control window
plyr::count(data_otitis_unique$control_event) # 10 in control window
plyr::count(data_otitis_unique$rw1_event) # 5 in risk window 1
plyr::count(data_otitis_unique$rw2_event) # 2 in risk window 2

# same for gout
# make the set
data_gout <- df_gout %>%
  left_join(pat, by = "person_id") %>%
  left_join(vax_wide, by = "person_id") %>%
  select(person_id, start_date_record, text_linked_to_event_code, sex_at_instance_creation,
         year_of_birth, date_of_death, contains("vax")) %>%
  distinct() %>%
  filter(!is.na(date_vax1)) %>%
  rename(date_gout = start_date_record) %>%
  rowwise() %>%
  mutate(
    control_event = ifelse(between(as.Date(date_gout), 
                                   as.Date(date_vax1) - 90, 
                                   as.Date(date_vax1) - 30), 1, 0),
    rw1_event = ifelse(between(as.Date(date_gout),
                               as.Date(date_vax1) + 1,
                               as.Date(date_vax1) + 28), 1, 0),
    rw2_event = ifelse(!is.na(date_vax2) & between(as.Date(date_gout),
                                                   as.Date(date_vax2) + 1,
                                                   as.Date(date_vax2) + 28), 1, 0),
    between_event = ifelse(!is.na(date_vax2) & between(as.Date(date_gout),
                                                       as.Date(date_vax1) + 28,
                                                       as.Date(date_vax2)), 1, 0),
    before_event = ifelse(as.Date(date_gout) < as.Date(date_vax1) - 90, 1, 0)
  ) %>%
  ungroup()


data_gout_unique <- data_gout %>%
  arrange(date_gout) %>%
  distinct(person_id, .keep_all = T) %>%
  arrange(person_id)

# getting the numbers
plyr::count(data_gout_unique$before_event) # 12083 after start control window
plyr::count(data_gout_unique$control_event) # 4264 in control window
plyr::count(data_gout_unique$rw1_event) # 1979 in risk window 1
plyr::count(data_gout_unique$rw2_event) # 708 in risk window 2
