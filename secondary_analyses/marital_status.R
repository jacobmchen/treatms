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

# read the social status data
data <- data.frame(read_excel(data_file_name, sheet="social status")) %>%
    # select relevant columns
    select(c(PatientName, FormGroup,
             marital_status))

print(unique(data$marital_status))

# get a dataframe of patients who are working at baseline
partnered_at_baseline <- data %>%
    filter(FormGroup == "Baseline") %>%
    filter(marital_status == "Married" | marital_status == "Domestic Partnership")

# get the list of patients that are working at baseline
partnered_at_baseline <- partnered_at_baseline$PatientName

# process the data
data <- data %>%
    # subset data to only those that are working at baseline
    filter(PatientName %in% partnered_at_baseline) %>%
    # keep only rows that correspond to a month measurement
    filter(str_detect(FormGroup, "Month")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # deselect the FormGroup column
    select(-FormGroup) %>%
    mutate(divorced_or_separated=ifelse(marital_status=="Divorced" | marital_status=="Separated", 1, 0)) %>%
    mutate(divorced_or_separated=ifelse(is.na(divorced_or_separated), 0, divorced_or_separated)) %>%
    select(-marital_status)

data %>% slice_head(n=10) %>% print()

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
        if (values[i] == 1) {
            return(months[i])
        }
    }

    # if the event does not happen, return infinity
    return(Inf)
}

# compute the event time for sdmt
event_time <- data %>% 
    group_by(PatientName) %>%
    summarise(event=compute_event_time(month, divorced_or_separated))

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

