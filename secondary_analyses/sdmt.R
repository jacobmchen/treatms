# code for evaluating the secondary outcome SDMT, which
# will be a time to event analysis

# read the global variables
source("../primary_analysis/global_variables.R")

# import the functions written by Diaz et al.
source("../estimator_functions.R")

# package for real excel files
library(readxl)

# read the data for baseline characteristics, which was computed separately
baseline_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# get the censoring time for SDMT
censor <- data.frame(read_excel(data_file_name, sheet="sdmt")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only relevant columns
    select(c(PatientName, month, sdmt_score)) %>%
    # group data by patient name
    group_by(PatientName) %>%
    # remove all rows where sdmt is missing
    filter(!is.na(sdmt_score)) %>%
    # keep the month with the last observed value
    slice_max(month) %>%
    # remove the column for the score
    select(-sdmt_score) %>%
    # rename the month as the censoring time
    rename(censor=month)
 
# process the data to eventually compute the event time for sdmt
event_time <- data.frame(read_excel(data_file_name, sheet="sdmt")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only relevant columns
    select(c(PatientName, month, sdmt_score)) %>%
    group_by(PatientName) %>%
    # sort the patient names by month
    arrange(PatientName, month) %>%
    # merge the censoring time
    full_join(censor, by="PatientName") %>%
    # keep only the observations that are before the censoring time
    filter(month <= censor) %>%
    # delete the censor column
    select(-censor)

# define a function that computes the event time for SDMT
compute_event_time <- function(months, values) {
    # if only one observed value, we cannot compute the event time
    if (length(months) <= 1) {
        return(NA)
    }

    # get the baseline value, which is defined as just the first
    # observed value
    baseline <- values[1]

    # see when the event happens, with the event being a 
    # 4 point or more increase in value from baseline
    for (i in 2:length(months)) {
        if (values[i] <= baseline-4) {
            return(months[i])
        }
    }

    # if the event does not happen, return infinity
    return(Inf)
}

# compute the event time for each patient
event_time <- event_time %>%
    select(c(PatientName, month, sdmt_score)) %>%
    group_by(PatientName) %>%
    summarise(event=compute_event_time(month, sdmt_score))

event_time %>% slice_head(n=10) %>% print()

# append the previously generated treatments to the data
treatment_data <- readRDS("../primary_analysis/treatment_data.RDS")

# create the combined data
combined_data <- baseline_data %>%
    # join the censor time
    inner_join(censor, by="PatientName") %>%
    # join the event time
    inner_join(event_time, by="PatientName") %>%
    # create randomized treatment
    group_by(PatientName) %>%
    # join the treatment data
    inner_join(treatment_data, by="PatientName") %>%
    # rename the column for treatment group
    rename(treatment=treatment_group) %>%
    ungroup() %>%
    # get rid of rows for whom the event is a missing value
    filter(!is.na(event))

combined_data %>% slice_head(n=10) %>% print()

# print the columns for debugging
print(colnames(combined_data))
# print the number of rows of data
print(nrow(combined_data))

# redefine the id to just be 1 through number of rows
combined_data <- combined_data %>%
    rename(id=PatientName) %>%
    mutate(id=1:nrow(combined_data)) %>%
    # rename the columns so that they match with what is needed
    # for the dlong function
    rename(c(event_time=event, treatment_group=treatment))

# define cut-off point
tau <- 84

# create long form data using the data with all clusters
dlong <- create_dlong(combined_data)

# execute the TMLE analysis
print(tmle(dlong, tau))
