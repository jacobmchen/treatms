# import the global variables file
source("global_variables.R")

# read the data with breakthrough disease data
data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="Sheet1")) %>%
    select(c(patient_id, contains("postpone_cancel_visit"), visit_date_Baseline, contains("visit_date_W"))) %>%
                 # pivot all columns except for patient_id
    pivot_longer(cols = -patient_id,
                 # when the column name is split into two parts, the first group of the original column
                 # name is the column name with the cell value; the second group of the original 
                 # column name should be stored in a column called timepoint
                 names_to = c(".value", "timepoint"),
                 # this pattern tells us how to split the original column name
                 # (.*) means any symbol any number of times as the first group
                 # (Baseline|Week\\d*) means the string "Baseline" or "Week" followed by any 
                 # number of digits
                 names_pattern = "(.*)_(Baseline|Week\\d*)") %>%
    # get only observed timepoints
    filter(!is.na(timepoint)) %>%
    # get only observed breakthrough disease yes/no
    filter(!is.na(postpone_cancel_visit)) %>%
    # make sure visit date is within the substudy range
    filter(visit_date >= substudy_start & visit_date <= substudy_end) %>%
    # keep only rows where the string "Yes" is present
    filter(str_detect(postpone_cancel_visit, "Yes"))

data %>% slice_head(n=10) %>% print()

# count how many times each patient appears in the data
postpone_count <- data %>% count(patient_id) %>%
    rename(num_postpone=n)

print(head(postpone_count))

# save the number of times each patient postpones or cancels into an RDS file
saveRDS(postpone_count, "postpone_count.RDS")

# get the patients that experienced breakthrough disease in the substudy timeframe
yes_postpone <- unique(data$patient_id)

# use the patient_id data to get data on whether each patient experienced
# breakthrough disease
postpone_data <- patient_id_data %>%
    mutate(yes_postpone = ifelse(patient_id %in% yes_postpone, 1, 0))

print(head(postpone_data))

# save the data to an RDS file
saveRDS(postpone_data, "postpone_data.RDS")
