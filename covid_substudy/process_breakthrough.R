# import the global variables file
source("global_variables.R")

# read the data with breakthrough disease data
data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="Sheet1")) %>%
    select(c(patient_id, contains("any_breakthrough_disease"), contains("visit_date_M"))) %>%
                 # pivot all columns except for patient_id
    pivot_longer(cols = -patient_id,
                 # when the column name is split into two parts, the first part of the column
                 # name should be the value inside the cell, and the second part should be the month number
                 names_to = c(".value", "month"),
                 # this pattern tells us how to split the column name
                 # (.*) means any symbol any number of times
                 # [0-9]+ means any digit at least one time
                 # the regex portions tell us which parts to keep us the value name and month name
                 names_pattern = "(.*)_M([0-9]+)") %>%
    # get only observed months
    filter(!is.na(month)) %>%
    # get only observed breakthrough disease yes/no
    filter(!is.na(any_breakthrough_disease)) %>%
    # make sure visit date is within the substudy range
    filter(visit_date >= substudy_start & visit_date <= substudy_end) %>%
    # keep only rows where the string "Yes" is present
    filter(str_detect(any_breakthrough_disease, "Yes"))

# get the patients that experienced breakthrough disease in the substudy timeframe
yes_breakthrough <- unique(data$patient_id)

# use the patient_id data to get data on whether each patient experienced
# breakthrough disease
breakthrough_data <- patient_id_data %>%
    mutate(breakthrough_disease = ifelse(patient_id %in% yes_breakthrough, 1, 0))

print(breakthrough_data)

# save the data to an RDS file
saveRDS(breakthrough_data, "breakthrough_data.RDS")
