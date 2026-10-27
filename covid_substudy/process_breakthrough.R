# import the global variables file
source("global_variables.R")

# get a list of patients corresponding to a binary yes/no of whether they
# experienced breakthrough disease

# read the data with breakthrough disease data
data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="Sheet1")) %>%
    select(c(patient_id, contains("any_breakthrough_disease"), contains("visit_date_M"))) %>%
                 # pivot all columns except for patient_id
    pivot_longer(cols = -patient_id,
                 # when the column name is split into two parts, the first group of the original column
                 # name is the column name with the cell value; the second group of the original 
                 # column name should be stored in a column called month
                 names_to = c(".value", "timepoint"),
                 # this pattern tells us how to split the column name
                 # (.*) means any symbol any number of times as the first group
                 # [0-9]+ means any digit at least one time as the second group
                 names_pattern = "(.*)_(Interim|M[0-9]+)") %>%
    # get only observed months
    filter(!is.na(month)) %>%
    # get only observed breakthrough disease yes/no
    filter(!is.na(any_breakthrough_disease)) %>%
    # make sure visit date is within the substudy range
    filter(visit_date >= substudy_start & visit_date <= substudy_end) %>%
    # keep only rows where the string "Yes" is present
    filter(str_detect(any_breakthrough_disease, "Yes"))

data %>% slice_head(n=10) %>% print()

# read the raw breakthrough disease data 
raw_data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="updated_breakthru disease")) %>%
    # select relevant columns
    select(c(PatientName, FormGroup, visit_date, any_breakthrough_disease)) %>%
    # group by patient identifiers
    group_by(PatientName) %>%
    # create a new column that is 1 if the string any_breakthrough_disease
    # contains "Yes" and 0 otherwise
    mutate(disease = ifelse(str_detect(any_breakthrough_disease, "Yes"), 1, 0)) %>%
    # summarize each patient by creating a new column that basically is 1 if a patient
    # has breakthrough disease at least once and is 0 otherwise
    summarize(
        breakthrough_disease = ifelse(sum(disease, na.rm=TRUE) > 0, 1, 0)
    ) %>%
    # rename column
    rename(patient_id = PatientName) 

raw_data %>% slice_head(n=10) %>% print()

raw_data %>% inner_join(data, by="patient_id") %>% filter(breakthrough_disease == 0) %>% print()

# get the patients that experienced breakthrough disease in the substudy timeframe
yes_breakthrough <- unique(data$patient_id)

# use the patient_id data to get data on whether each patient experienced
# breakthrough disease
breakthrough_data <- patient_id_data %>%
    mutate(breakthrough_disease = ifelse(patient_id %in% yes_breakthrough, 1, 0))

breakthrough_data %>% slice_head(n=5) %>% print()

breakthrough_data_raw <- patient_id_data %>%
    left_join(raw_data, by="patient_id") %>%
    filter(is.na(breakthrough_disease))

print(breakthrough_data_raw)
q()

# save the data to an RDS file
saveRDS(breakthrough_data, "breakthrough_data.RDS")
