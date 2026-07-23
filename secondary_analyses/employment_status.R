# code for evaluating the secondary outcome SDMT, which
# will be a time to event analysis

# read the global variables
source("../primary_analysis/global_variables.R")

# import the functions written by Diaz et al.
source("../estimator_functions.R")

# package for real excel files
library(readxl)

# library for imputing missing values
library(mice)

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

data <- data.frame(read_excel(data_file_name, sheet="social status")) %>%
    filter(FormGroup == "Baseline") %>%
    filter(is.na(completion_date)) 

missing_baseline <- data$PatientName

data <- data.frame(read_excel(data_file_name, sheet="social status")) %>%
    filter(PatientName %in% missing_baseline) %>%
    filter(FormGroup == "Month 9") %>%
    filter(!is.na(completion_date))

# data %>% slice_head(n=10) %>% print()
# print(nrow(data))

# write.csv(data, "csv_files/no_baseline_yes_month9.csv", row.names=FALSE)

trial_start <- data.frame(read_excel(data_file_name, sheet="visit windows")) %>%
    select(c(Patient, open6)) %>%
    rename(PatientName=Patient, trial_start=open6)

data <- data.frame(read_excel(data_file_name, sheet="social status")) %>%
    filter(PatientName %in% missing_baseline) %>%
    group_by(PatientName) %>%
    left_join(trial_start, by="PatientName") %>%
    ungroup() %>%
    filter(FormGroup == "Interim ePro" | FormGroup == "Supplemental") %>%
    filter(!is.na(completion_date)) %>%
    filter(as.numeric(difftime(completion_date, trial_start, units = "days")) <= 365)

data %>% slice_head(n=10) %>% print(width=Inf)

write.csv(data, "csv_files/no_baseline_yes_interim_supp.csv", row.names=FALSE)

q()

# read the social status data
data <- data.frame(read_excel(data_file_name, sheet="social status")) %>%
    # select relevant columns
    select(c(PatientName, FormGroup,
             employ_status_disabled, employ_status_unemployed,
             employ_status_working))

# get a dataframe of patients who are working at baseline
working_at_baseline <- data %>%
    filter(FormGroup == "Baseline") %>%
    filter(employ_status_working == "Working now")

# get the list of patients that are working at baseline
working_at_baseline <- working_at_baseline$PatientName

# process the data
data <- data %>%
    # subset data to only those that are working at baseline
    filter(PatientName %in% working_at_baseline) %>%
    # rename columns to more simple things
    rename(disabled=employ_status_disabled, unemployed=employ_status_unemployed,
           working=employ_status_working) %>%
    # keep only rows that correspond to a month measurement
    filter(str_detect(FormGroup, "Month")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # deselect the FormGroup column
    select(-FormGroup) %>%
    # change measurements in disabled, unemployed, and working to 0 and 1
    # instead of string and missing values
    mutate(disabled=ifelse(is.na(disabled), 0, 1)) %>%
    mutate(unemployed=ifelse(is.na(unemployed), 0, 1)) %>%
    mutate(working=ifelse(is.na(working), 0, 1)) %>%
    mutate(disabled_or_unemployed=disabled+unemployed) %>%
    select(-c(disabled, unemployed, working))

# get the censoring time, which turns out to be 72 for every patient!
censor <- data %>%
    # group data by patient name
    group_by(PatientName) %>%
    # keep the month with the last observed value
    slice_max(month) %>%
    # rename the month as the censoring time
    rename(censor=month)

# define a function that computes the event time for employment status
compute_event_time <- function(months, values) {
    # see when the event happens, with the event being a 
    # disabled or unemployed value of greater than 1
    for (i in 1:length(months)) {
        if (values[i] > 0) {
            return(months[i])
        }
    }

    # if the event does not happen, return infinity
    return(Inf)
}

# compute the event time for sdmt
event_time <- data %>% 
    group_by(PatientName) %>%
    summarise(event=compute_event_time(month, disabled_or_unemployed))

set.seed(0)

# create the combined data
combined_data <- baseline_data %>%
    # join the censor time
    inner_join(censor, by="PatientName") %>%
    # join the event time
    inner_join(event_time, by="PatientName") %>%
    # create randomized treatment
    group_by(PatientName) %>%
    mutate(treatment=rbinom(1, size=1, prob=0.5)) %>%
    ungroup() %>%
    # get rid of rows for whom the event is a missing value
    filter(!is.na(event))

# redefine the id to just be 1 through number of rows
combined_data <- combined_data %>%
    rename(id=PatientName) %>%
    mutate(id=1:nrow(combined_data)) %>%
    # rename the columns so that they match with what is needed
    # for the dlong function
    rename(c(event_time=event, treatment_group=treatment))

combined_data %>% slice_head(n=10) %>% print()

# define cut-off point, which is the censoring time for 
# all patients
tau <- 72

# create long form data using the data with all clusters
dlong <- create_dlong(combined_data)

# execute the TMLE analysis
print(tmle(dlong, tau))

