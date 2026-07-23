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

data <- adverse_events %>%
    filter(!is.na(ae_describe_event)) %>%
    filter(dmt_action_taken == "None") %>%
    filter(sae_yes_no == "No") %>%
    mutate(pregnancy = ifelse(grepl("pregnancy", ae_describe_event, ignore.case=TRUE), 1, 0)) %>%
    filter(pregnancy == 0) %>%
    select(-pregnancy)

data %>% slice_head(n=10) %>% print()
write.csv(data, "csv_files/no_action_no_sae.csv", row.names=FALSE)

q()

# print("total number of rows in dataset")
# print(nrow(adverse_events))
#
# print("possible dmt actions taken")
# print(unique(adverse_events$dmt_action_taken))
#
# print("number of NAs for action taken")
# print(sum(is.na(adverse_events$dmt_action_taken)))
#
# print("number of NAs for describe event")
# print(sum(is.na(adverse_events$ae_describe_event)))
#
# print("number of unique patients in dataset")
# print(length(unique(adverse_events$PatientName)))
#
# # get the patients for whom there is an adverse event described
# # but a missing value for action taken
# no_action_patients <- adverse_events %>%
#     filter(!is.na(ae_describe_event)) %>%
#     filter(is.na(dmt_action_taken))
#
# print(nrow(no_action_patients))
# print(head(no_action_patients))
# # write.csv(no_action_patients, "no_action_patients.csv", row.names=FALSE)
#
# # get patients with serious adverse event
# adverse_events_serious_only <- adverse_events %>%
#     filter(!is.na(ae_describe_event)) %>%
#     filter(sae_yes_no == "Yes") 
#     # filter(is.na(ae_onset_date))
#
# # write.csv(adverse_events_serious_only, "sae_no_onset_date.csv", row.names=FALSE)
#
# print(head(adverse_events_serious_only))
#
# # make sure each serious adverse event has a corresponding entry in 
# # the SAEs tab by iterating over patients with sae
# for (i in 1:nrow(adverse_events_serious_only)) {
#     # get patient id and ae onset date
#     id <- adverse_events_serious_only$PatientName[i]
#     month <- month(adverse_events_serious_only$ae_onset_date[i])
#     year <- year(adverse_events_serious_only$ae_onset_date[i])
#
#     # first make sure values are not missing
#     if (is.na(id) || is.na(month)) {
#         print("missing values")
#         print(id)
#         print(month)
#         print(year)
#         next
#     }
#
#     # see if there is a row in the sae tab that matches up by
#     # patient name and onset month and year
#     subset <- serious_adverse_events %>%
#         filter(month(sae_date) == month & year(sae_date) == year & PatientName == id)
#
#     if (nrow(subset) == 0) {
#         print("no match")
#         print(id)
#         print(month)
#         print(year)
#     }
# }
#
# # temporarily exit program early
# q()

# if the event is a pregnancy and the action taken is NA, replace
# by None
adverse_events <- adverse_events %>%
    mutate(dmt_action_taken = ifelse(grepl("pregnancy", ae_describe_event, ignore.case=TRUE) & is.na(dmt_action_taken), "None", dmt_action_taken))

# data processing for adverse events
adverse_events <- adverse_events %>%
    # keep only rows of data where a clear action regarding
    # disease modifying therapy was taken
    filter(!is.na(dmt_action_taken)) %>%
    # select only relevant columns
    select(c(PatientName, ae_describe_event, dmt_action_taken))

adverse_events <- adverse_events %>%
    # remove all rows where the action taken is none
    filter(dmt_action_taken != "None")

# convert the adverse event data to count data of how
# many times each patient experiences an adverse event
# that affects the disease modifying therapy
adverse_events <- adverse_events %>%
    count(PatientName) %>%
    rename(num_aes = n)

# read the covariate data
baseline_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# read the censoring data so we know the censoring time for each patient
censoring_times <- readRDS("../primary_analysis/censoring_times.RDS") %>%
    select(c(PatientName, censor))

# read the imputed edss and pdds data so that we know how many follow-ups
# each patient has
imputed_edss_pdds <- readRDS("../primary_analysis/imputed_edss_pdds_data.RDS") %>%
    count(PatientName) %>%
    rename(num_followup = n) %>%
    select(c(PatientName, num_followup))

adverse_events <- baseline_data %>%
    # merge the adverse events count to the baseline data
    left_join(adverse_events, by="PatientName") %>%
    # replace all NAs with 0
    mutate(num_aes = ifelse(is.na(num_aes), 0, num_aes)) %>%
    # add the censoring times to the data
    left_join(censoring_times, by="PatientName") %>%
    # replace all NAs with 0
    mutate(censor = ifelse(is.na(censor), 0, censor)) %>%
    # add the number of followups to the data
    left_join(imputed_edss_pdds, by="PatientName") %>%
    # replace all NAs with 0
    mutate(num_followup = ifelse(is.na(num_followup), 0, num_followup))

adverse_events %>% slice_head(n=10) %>% print()

# data processing for serious adverse events
serious_adverse_events <- serious_adverse_events %>%
    select(c(PatientName, primary_ae)) %>%
    # count the number of times each patient appears in the serious
    # adverse events spreadsheet
    count(PatientName) %>%
    rename(num_saes = n)

serious_adverse_events <- baseline_data %>%
    # merge the count of serious adverse events with the baseline data
    left_join(serious_adverse_events, by="PatientName") %>%
    # if there's a missing value, that corresponds to no saes
    mutate(num_saes = ifelse(is.na(num_saes), 0, num_saes)) %>%
    # add the censoring times to the data
    left_join(censoring_times, by="PatientName") %>%
    # replace all NAs with 0
    mutate(censor = ifelse(is.na(censor), 0, censor)) %>%
    # add the number of followups to the data
    left_join(imputed_edss_pdds, by="PatientName") %>%
    # replace all NAs with 0
    mutate(num_followup = ifelse(is.na(num_followup), 0, num_followup))

serious_adverse_events %>% slice_head(n=10) %>% print()

# set the seed so that the experiments are reproducible
set.seed(0)

print("analyze adverse events")

# add a randomly generated treatment to the data for simulation
adverse_events <- adverse_events %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(adverse_events))

formulas <- create_formulas("treatment", "", "PatientName", "num_aes", adverse_events, "treatment*censor + treatment*num_followup")
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(adverse_events, formula_red, formula_full, "count"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(adverse_events, 2, formula_red, formula_full, "num_aes", "count"))

print("analyze serious adverse events")

# add a randomly generated treatment to the data for simulation
serious_adverse_events <- serious_adverse_events %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(serious_adverse_events))

formulas <- create_formulas("treatment", "", "PatientName", "num_saes", serious_adverse_events, "treatment*censor + treatment*num_followup")
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(serious_adverse_events, formula_red, formula_full, "count"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(serious_adverse_events, 2, formula_red, formula_full, "num_saes", "count"))

