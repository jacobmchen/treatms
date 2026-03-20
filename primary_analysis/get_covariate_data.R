# Get the covariate data for each individual. We need to one-
# hot encode the categorical variables and fill in missing
# values with MICE.
# We create 3 versions of the covariate data depending on
# how we handle clusters. The first version one-hot encodes
# all clusters. The second version includes no clusters, and
# the third version groups some clusters together.

# read global variables
source("global_variables.R")

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# package for one-hot encoding variables
library(fastDummies)

# create a function that takes all data processing steps from the
# baseline data
process_data <- function(data) {
    return_data <- data %>%
        # remove the treatment group data and patient status data
        select(-c(treatment_group, patient_status)) %>%
        # convert the risk stratification data to 0 and 1 since it is binary
        mutate(risk_stratification = as.numeric(factor(risk_stratification)) - 1) %>%
        # convert the ethnicity data to 0 and 1 since it is binary
        mutate(ethnicity = ifelse(ethnicity == "Hispanic or Latino", 1, 0)) %>%
        # remove the spaces from the values of the race_calculated and SiteName
        # columns
        mutate(race_calculated = gsub(" ", "", race_calculated)) %>%
        mutate(SiteName = gsub(" ", "", SiteName)) %>%
        # change the - and / characters to an empty string to avoid string problems
        # later on for the columns SiteName and gender
        mutate(SiteName = gsub("-", "", SiteName)) %>%
        mutate(SiteName = gsub("/", "", SiteName)) %>%
        mutate(gender = gsub("-", "", gender)) %>%
        # binarize whether the patient is African American
        mutate(race = ifelse(race_calculated == "BLACK OR AFRICAN AMERICAN", 1, 0)) %>%
        # binarize whether the patient is male
        mutate(gender = ifelse(gender == "Male", 1, 0)) %>%
        # one hot encode site name since these are unordered categorical variables;
        # remove first dummy to prevent collinearity issues
        dummy_cols(select_columns=c("SiteName"), remove_first_dummy=TRUE) %>%
        # compute the age at consent as the difference between the consent
        # date and the birth date
        mutate(birth_date = as.Date(birth_date, format="%m/%d/%Y")) %>%
        mutate(consent_date = as.Date(consent_date, format="%m/%d/%Y")) %>%
        mutate(age = as.numeric(consent_date - birth_date) %/% 365) %>%
        # remove columns we cleaned and no longer need,
        # and remove the column gender non-binary
        select(-c(race_calculated, SiteName, birth_date, consent_date))

    return(return_data)
}

# create a function that takes all data processing steps from the
# baseline covariates data
process_data_covars <- function(data) {
    # TO-DO: replace UNKNOWN values in the data with what clinicians
    # inputted into the risk stratification data, which should have
    # no missing values

    return_data <- data %>%
        # deselect all of the variables that we are not using from the
        # baseline covars data
        select(-c(site_full_name, patient_status, male, older.or.very.young,
                  Hispanic, AfrAmerican)) %>%
        # across all of the columns indicated, convert to NA any
        # string that says Unknown, and then change it to a 
        # numeric value
        mutate(across(c(early.second.relapse,
                        frequent.relapses, incomplete.recovery,
                        high.lesion.burden, new.T2.lesions,
                        enhancing.lesions, BS_cerebellum_SC), ~ as.numeric(factor(na_if(.x, "Unknown"))) - 1)) %>%
        rename(PatientName = patient_id)

    # code for evaluating missing data in the covariate data
    # print("AfrAmerican missing")
    # print(return_data$patient_id[which(is.na(return_data$AfrAmerican))])
    # print(sum(is.na(return_data$AfrAmerican)))
    # print("early second relapse missing")
    # print(sum(is.na(return_data$early.second.relapse)))
    # print("frequent relapses")
    # print(sum(is.na(return_data$frequent.relapses)))
    # print("incomplete recovery missing")
    # print(sum(is.na(return_data$incomplete.recovery)))
    # print("high lesion burden missing")
    # print(sum(is.na(return_data$high.lesion.burden)))
    # print("new T2 lesions missing")
    # print(sum(is.na(return_data$new.T2.lesions)))
    # print("enchancing lesions missing")
    # print(sum(is.na(return_data$enhancing.lesions)))
    # print("BS cerebellum SC missing")
    # print(sum(is.na(return_data$BS_cerebellum_SC)))

    return(return_data)
}

# create a function that merges the baseline characteristics
# and baseline covariates data
merge_char_covar_data <- function(char_data, covar_data) {
    return_data <- merge(char_data, covar_data, 
                                    by="PatientName", all.x=TRUE) %>%
        # remove the race columns since we are just using whether the patient
        # is AfrAmerican
        select(-starts_with("race_calculated"))

    return(return_data)
}

###
# finished defining functions, actual code is below
###

# read the data for baseline characteristics
baseline_data <- data.frame(read_excel(data_file_name, sheet="baseline chars"))

# remove this patient since we have no data for them 
baseline_data <- baseline_data %>%
    filter(PatientName != no_data_patient)

# clean the baseline data using all clusters
baseline_data_all_clusters <- process_data(baseline_data)

# read the data for baseline covariates (variables that are indicative
# of disease progression)
baseline_covars <- data.frame(read_excel(data_file_name, sheet="baseline covars"))

# remove the patient for whom we have no data for
baseline_covars <- baseline_covars %>%
    filter(patient_id != no_data_patient)

# clean the baseline covars data
baseline_covars <- process_data_covars(baseline_covars)

# merge the baseline characteristic data with the baseline
# covariate data
baseline_data_all_clusters <- merge_char_covar_data(baseline_data_all_clusters, baseline_covars)

# start of code block
###
# below is code used to identify discrepancies between baseline covars and
# baseline char data
#
# baseline_covars <- baseline_covars %>%
#     select(c(patient_id, male, AfrAmerican, Hispanic)) %>%
#     rename(PatientName = patient_id) %>%
#     mutate(male = as.numeric(factor(male)) - 1) %>%
#     mutate(AfrAmerican = ifelse(AfrAmerican == "Unknown", NA, AfrAmerican)) %>%
#     mutate(AfrAmerican = as.numeric(factor(AfrAmerican)) - 1) %>%
#     mutate(Hispanic = ifelse(Hispanic == "Unknown", NA, Hispanic)) %>%
#     mutate(Hispanic = as.numeric(factor(Hispanic)) - 1)
#
# merged_baseline <- merge(baseline_data_all_clusters, baseline_covars, by="PatientName") %>%
#     select(c(PatientName, male, gender_Male, AfrAmerican, race_calculated_BLACKORAFRICANAMERICAN,
#              Hispanic, ethnicity))
#
# print(head(merged_baseline))
# print("gender")
# print(which(merged_baseline$male != merged_baseline$gender_Male))
# print(merged_baseline[which(merged_baseline$male != merged_baseline$gender_Male), ])
# print("african american")
# print(which(merged_baseline$AfrAmerican != merged_baseline$race_calculated_BLACKORAFRICANAMERICAN))
# print(merged_baseline[which(merged_baseline$AfrAmerican != merged_baseline$race_calculated_BLACKORAFRICANAMERICAN), ])
# print("hispanic")
# print(which(merged_baseline$Hispanic == merged_baseline$ethnicity))
# print(merged_baseline[which(merged_baseline$Hispanic == merged_baseline$ethnicity), ])
# print(merged_baseline$PatientName[which(merged_baseline$Hispanic == merged_baseline$ethnicity)])
###
# end of code block

# save the baseline data as an RDS file
saveRDS(baseline_data_all_clusters, file="baseline_data.RDS")

# clean the baseline data using no clusters
# NOTE: we don't use the predefined function here because we are not one
# hot encoding the the site names here
baseline_data_no_clusters <- baseline_data %>%
    # remove the treatment group data and patient status data
    select(-c(treatment_group, patient_status)) %>%
    # convert the risk stratification data to 0 and 1 since it is binary
    mutate(risk_stratification = as.numeric(factor(risk_stratification)) - 1) %>%
    # convert the ethnicity data to 0 and 1 since it is binary
    mutate(ethnicity = as.numeric(factor(ethnicity)) - 1) %>%
    # remove the spaces from the values of the race_calculated and SiteName
    # columns
    mutate(race_calculated = gsub(" ", "", race_calculated)) %>%
    # change the - and / characters to an empty string to avoid string problems
    # later on for the columns SiteName and gender
    mutate(gender = gsub("-", "", gender)) %>%
    # one hot encode gender and race since these are unordered
    # categorical variables
    dummy_cols(select_columns=c("gender", "race_calculated")) %>%
    # compute the age at consent as the difference between the consent
    # date and the birth date
    mutate(birth_date = as.Date(birth_date, format="%m/%d/%Y")) %>%
    mutate(consent_date = as.Date(consent_date, format="%m/%d/%Y")) %>%
    mutate(age = as.numeric(consent_date - birth_date) %/% 365) %>%
    # remove columns we cleaned and no longer need
    select(-c(gender, race_calculated, SiteName, birth_date, consent_date))

# merge with the baseline covariate data
baseline_data_no_clusters <- merge_char_covar_data(baseline_data_no_clusters, baseline_covars)

# save the baseline data as an RDS file
saveRDS(baseline_data_no_clusters, file="baseline_data_no_clusters.RDS")

# print out the number of units in each cluster
rare_clusters <- baseline_data %>%
    count(SiteName) %>%
    filter(n < 10)

baseline_data_merge_rare_clusters <- baseline_data %>%
    mutate(SiteName = ifelse(SiteName %in% rare_clusters$SiteName, "Other", SiteName))

# clean the baseline data using all clusters
baseline_data_merge_rare_clusters <- process_data(baseline_data_merge_rare_clusters)

# merge with the baseline covariates data
baseline_data_merge_rare_clusters <- merge_char_covar_data(baseline_data_merge_rare_clusters, baseline_covars)

# save the baseline data as an RDS file
saveRDS(baseline_data_merge_rare_clusters, file="baseline_data_merge_rare_clusters.RDS")

site_to_state_dict <- c(
    `Advanced Neuro Spc-0427` = "MD",
    `Allegheny-0265` = "PA",
    `Barrow-0301` = "AZ",
    `Baylor Dallas-0400` = "TX",
    `Billings Clinic-0425` = "MT",
    `Blacksburg-0448` = "VA",
    `Cedars Sinai-0262` = "CA",
    `CenTx Neuro-0402` = "TX",
    `Christiana Care-0401` = "DE",
    `Columbia Presby-0104` = "NY",
    `Dignity Sacramento-0403` = "CA",
    `Geisinger-0106` = "PA",
    `Georgetown DC-0217` = "DC",
    `Hackensack-0405` = "NJ",
    `Icahn at Mount Sinai-0101` = "NY",
    `JHU Remote-9999` = "MD",
    `JHU-0100` = "MD",
    `MCR/Tidewater-0411` = "SC",
    `MCW-0153` = "SC",
    `MassGen-0156` = "MA",
    `Mayo Clinic-0407` = "MN",
    `NYU-0412` = "NY",
    `Norton-0410` = "KY",
    `OMRF-0125` = "OK",
    `OhioHealth-0413` = "OH",
    `Providence-0270` = "OR",
    `Rush-0223` = "IL",
    `Swedish-0231` = "WA",
    `UAB-0216` = "AL",
    `UCLA-0225` = "CA",
    `UCSD-0289` = "CA",
    `UCSF-0233` = "CA",
    `UCincinnati-0134` = "OH",
    `UFL Gainesville-0416` = "FL",
    `UKansas Med Ctr-0300` = "KS",
    `UMB-0173` = "MD",
    `UMass Worcester-0420` = "MA",
    `UMiami-0256` = "FL",
    `UMichigan-0313` = "MI",
    `UNMC-0444` = "NE",
    `USouth Alabama-0449` = "AL",
    `USouth FL Health-0267` = "FL",
    `UTexas SW-0243` = "TX",
    `UUtah-0238` = "UT",
    `UVermont-0423` = "VT",
    `UWashington-0424` = "WA",
    `Vanderbilt-0241` = "TN",
    `Wayne State-0302` = "MI"
)

baseline_data_merge_states <- baseline_data %>%
    mutate(SiteName = site_to_state_dict[SiteName])

# process the data after merging sites to states
baseline_data_merge_states <- process_data(baseline_data_merge_states)

# merge with the baseline covariates data
baseline_data_merge_states <- merge_char_covar_data(baseline_data_merge_states, baseline_covars)

# save the baseline data as an RDS file
saveRDS(baseline_data_merge_states, file="baseline_data_merge_states.RDS")

###
# rough draft of code that checks for collinearity
###
# # check for collinearity
# # remove the PatientName column so it doesn't get
# # one-hot encoded
# check_collinear <- baseline_data_merge_states %>%
#     select(-c(PatientName, new.T2.lesions))
# # note that the model.matrix function only keeps
# # rows that are completely observed
# X <- model.matrix(~ ., data=check_collinear)[, -1]
# print(nrow(X))
# sds <- apply(X, 2, sd, na.rm=TRUE)
# X2 <- X[, sds > 0 & !is.na(sds), drop = FALSE]
# print("merge states")
# print(kappa(scale(X2)))
#
# check_collinear <- baseline_data_merge_rare_clusters %>%
#     select(-c(PatientName, new.T2.lesions))
# # note that the model.matrix function only keeps
# # rows that are completely observed
# X <- model.matrix(~ ., data=check_collinear)[, -1]
# print(nrow(X))
# sds <- apply(X, 2, sd, na.rm=TRUE)
# X2 <- X[, sds > 0 & !is.na(sds), drop = FALSE]
# print("merge rare clusters")
# print(kappa(scale(X2)))
#
# check_collinear <- baseline_data_all_clusters %>%
#     select(-c(PatientName, new.T2.lesions))
# # note that the model.matrix function only keeps
# # rows that are completely observed
# X <- model.matrix(~ ., data=check_collinear)[, -1]
# print(nrow(X))
# sds <- apply(X, 2, sd, na.rm=TRUE)
# X2 <- X[, sds > 0 & !is.na(sds), drop = FALSE]
# print("all clusters")
# print(kappa(scale(X2)))
#
# check_collinear <- baseline_data_no_clusters %>%
#     select(-c(PatientName, new.T2.lesions))
# # note that the model.matrix function only keeps
# # rows that are completely observed
# X <- model.matrix(~ ., data=check_collinear)[, -1]
# print(nrow(X))
# sds <- apply(X, 2, sd, na.rm=TRUE)
# X2 <- X[, sds > 0 & !is.na(sds), drop = FALSE]
# print("no clusters")
# print(kappa(scale(X2)))

