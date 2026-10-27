# import the global variables file
source("global_variables.R")

# process data on whether patients had new or worsening symptoms

# read the data with breakthrough disease data
data <- data.frame(read_excel(new_worse, sheet="Sheet1")) %>%
    # select relevant columns
    select(c(patient_id, ms_date_reported, symp_newly_rpt_followup, symp_worsened)) %>%
    # get rid of missing values for date reported
    filter(!is.na(ms_date_reported)) %>%
    # change missing values to no
    mutate(symp_newly_rpt_followup = ifelse(is.na(symp_newly_rpt_followup), "no", symp_newly_rpt_followup)) %>%
    mutate(symp_worsened = ifelse(is.na(symp_worsened), "no", symp_worsened)) %>%
    # if string is "on", then change the value to 1, otherwise keep it 0
    mutate(symp_newly_rpt_followup = ifelse(symp_newly_rpt_followup == "on", 1, 0)) %>%
    mutate(symp_worsened = ifelse(symp_worsened == "on", 1, 0)) %>%
    # new column that is 1 if either new or worsening symptom is recorded
    mutate(new_or_worsen = ifelse(symp_newly_rpt_followup == 1 | symp_worsened == 1, 1, 0)) %>%
    # keep only the patients that had new or worsening symptoms
    filter(new_or_worsen == 1)
 
data %>% slice_head(n=10) %>% print()

# read the raw new/worsening symptoms data 
raw_data <- data.frame(read_excel(new_worse, sheet="updated_new or worse sx")) %>%
    # select relevant columns
    select(c(PatientName, symp_newly_rpt_followup, symp_worsened)) %>%
    # change missing values in both columns to "no"
    mutate(symp_newly_rpt_followup = ifelse(is.na(symp_newly_rpt_followup), "no", symp_newly_rpt_followup)) %>%
    mutate(symp_worsened = ifelse(is.na(symp_worsened), "no", symp_worsened)) %>%
    # change columns to be 1 if the string is "on" and 0 otherwise
    mutate(symp_newly_rpt_followup = ifelse(symp_newly_rpt_followup == "on", 1, 0)) %>%
    mutate(symp_worsened = ifelse(symp_worsened == "on", 1, 0)) %>%
    # create a new column that is the >0 if there is a new or worsening symptom at a
    # certain time point
    mutate(new_or_worsen = symp_newly_rpt_followup + symp_worsened) %>%
    # group by patient identifiers
    group_by(PatientName) %>%
    # summarize each patient by creating a new column that basically is 1 if a patient
    # has new or worsening symptoms at least once and is 0 otherwise
    summarize(
        new_or_worsen_raw = ifelse(sum(new_or_worsen, na.rm=TRUE) > 0, 1, 0)
    ) %>%
    # rename column
    rename(patient_id = PatientName) 

raw_data %>% slice_head(n=10) %>% print()

# get the patients that experienced breakthrough disease in the substudy timeframe
new_worse <- unique(data$patient_id)

# use the patient_id data to get data on whether each patient had new or
# worsening symptoms
new_worse_data <- patient_id_data %>%
    mutate(new_or_worsen = ifelse(patient_id %in% new_worse, 1, 0))

print(new_worse_data)

raw_data %>% inner_join(new_worse_data, by="patient_id") %>% filter(new_or_worsen != new_or_worsen_raw) %>%
    print()
q()

# save the data to an RDS file
saveRDS(new_worse_data, "new_worse_data.RDS")
