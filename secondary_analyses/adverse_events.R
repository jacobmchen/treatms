# code for evaluating the outcome Adverse events leading
# to dose reduction or change in therapy

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)
library(lubridate)

# read the adverse events data data
adverse_events <- data.frame(read_excel(data_file_name, sheet="AEs"))
serious_adverse_events <- data.frame(read_excel(data_file_name, sheet="SAEs"))

print("total number of rows in dataset")
print(nrow(adverse_events))

print("possible dmt actions taken")
print(unique(adverse_events$dmt_action_taken))

print("number of NAs for action taken")
print(sum(is.na(adverse_events$dmt_action_taken)))

print("number of NAs for describe event")
print(sum(is.na(adverse_events$ae_describe_event)))

print("number of unique patients in dataset")
print(length(unique(adverse_events$PatientName)))

# get the patients for whom there is an adverse event described
# but a missing value for action taken
no_action_patients <- adverse_events %>%
    filter(!is.na(ae_describe_event)) %>%
    filter(is.na(dmt_action_taken))

print(nrow(no_action_patients))
print(head(no_action_patients))
# write.csv(no_action_patients, "no_action_patients.csv", row.names=FALSE)

# get patients with serious adverse event
adverse_events_serious_only <- adverse_events %>%
    filter(!is.na(ae_describe_event)) %>%
    filter(sae_yes_no == "Yes") 
    # filter(is.na(ae_onset_date))

# write.csv(adverse_events_serious_only, "sae_no_onset_date.csv", row.names=FALSE)

print(head(adverse_events_serious_only))

# make sure each serious adverse event has a corresponding entry in 
# the SAEs tab by iterating over patients with sae
for (i in 1:nrow(adverse_events_serious_only)) {
    # get patient id and ae onset date
    id <- adverse_events_serious_only$PatientName[i]
    month <- month(adverse_events_serious_only$ae_onset_date[i])
    year <- year(adverse_events_serious_only$ae_onset_date[i])

    # first make sure values are not missing
    if (is.na(id) || is.na(month)) {
        print("missing values")
        print(id)
        print(month)
        print(year)
        next
    }

    # see if there is a row in the sae tab that matches up by
    # patient name and onset month and year
    subset <- serious_adverse_events %>%
        filter(month(sae_date) == month & year(sae_date) == year & PatientName == id)

    if (nrow(subset) == 0) {
        print("no match")
        print(id)
        print(month)
        print(year)
    }
}

# temporarily exit program early
q()

# data processing for adverse events
adverse_events <- adverse_events %>%
    # keep only rows of data where a clear action regarding
    # disease modifying therapy was taken
    filter(!is.na(dmt_action_taken)) %>%
    # select only relevant columns
    select(c(PatientName, ae_describe_event, dmt_action_taken))

print(sum(adverse_events$dmt_action_taken == "None"))

adverse_events <- adverse_events %>%
    # remove all rows where the action taken is none
    filter(dmt_action_taken != "None")

# convert the adverse event data to count data of how
# many times each patient experiences an adverse event
# that affects the disease modifying therapy
adverse_events <- adverse_events %>%
    count(PatientName) %>%
    rename(num_aes = n)

print(head(adverse_events))

# read the covariate data
baseline_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

print(head(baseline_data))

adverse_events <- baseline_data %>%
    # merge the adverse events count to the baseline data
    left_join(adverse_events, by="PatientName") %>%
    # replace all NAs with 0
    mutate(num_aes = ifelse(is.na(num_aes), 0, num_aes))

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
adverse_events <- adverse_events %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(adverse_events))

formulas <- create_formulas("treatment", "", "PatientName", "num_aes", adverse_events)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(adverse_events, formula_red, formula_full, "count"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(adverse_events, 2, formula_red, formula_full, "num_aes", "count"))


