# code for determining and evaluating exam-based relapse recovery

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the relapse data
relapse_data_orig <- data.frame(read_excel(data_file_name, sheet="relapse"))

data <- relapse_data_orig %>%
    # select only relevant columns
    select(c(PatientName, FormGroup, visit_date, ra_new_symptoms, ra_onset_date)) %>%
    # get only patients with a confirmed relapse
    filter(ra_new_symptoms == "Yes") %>%
    # change the string "u/k" to a missing value
    mutate(ra_onset_date = ifelse(ra_onset_date == "u/k", NA, ra_onset_date)) %>%
    # change the string to a numeric value
    mutate(ra_onset_date = as.numeric(ra_onset_date)) %>%
    # change the numeric value to a date with the origin being
    # the special origin date for microsoft excel
    mutate(ra_onset_date = as.Date(ra_onset_date, origin = "1899-12-30"))

data_missing <- data %>%
    # get patients who have a missing value for the onset date
    filter(is.na(ra_onset_date))

data_missing %>% slice_head(n=10) %>% print()

write.csv(data_missing, "data_missing.csv", row.names=FALSE)

future_relapse_data <- data %>%
    # get patients who have a symptom onset date later than
    # their visit date
    filter(ra_onset_date > visit_date)

future_relapse_data %>% slice_head(n=10) %>% print()

write.csv(future_relapse_data, "future_relapse_data.csv", row.names=FALSE)

future_relapse_data <- data %>%
    # get patients who have a symptom onset date later than
    # their visit date
    filter(ra_onset_date + 365 < visit_date)

future_relapse_data %>% slice_head(n=10) %>% print()


