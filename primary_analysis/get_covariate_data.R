# Get the covariate data for each individual. We need to one-
# hot encode the categorical variables and fill in missing
# values with MICE.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# package for one-hot encoding variables
library(fastDummies)

# read the data for EDSS
baseline_data <- data.frame(read_excel("../preliminary_longitudinal_data.xlsx", sheet="baseline chars"))

# remove this patient since we have no data for them 
baseline_data <- baseline_data %>%
    filter(PatientName != "0225-016")

# clean the baseline data
baseline_data <- baseline_data %>%
    # remove the treatment group data and patient status data
    select(-c(treatment_group, patient_status)) %>%
    # convert the risk stratification data to 0 and 1 since it is binary
    mutate(risk_stratification = as.numeric(factor(risk_stratification)) - 1) %>%
    # convert the ethnicity data to 0 and 1 since it is binary
    mutate(ethnicity = as.numeric(factor(ethnicity)) - 1) %>%
    # remove the spaces from the values of the race_calculated and SiteName
    # columns
    mutate(race_calculated = gsub(" ", "", race_calculated)) %>%
    mutate(SiteName = gsub(" ", "", SiteName)) %>%
    # change the - and / characters to an empty string to avoid string problems
    # later on for the columns SiteName and gender
    mutate(SiteName = gsub("-", "", SiteName)) %>%
    mutate(SiteName = gsub("/", "", SiteName)) %>%
    mutate(gender = gsub("-", "", gender)) %>%
    # one hot encode gender, race, and site name since these are unordered
    # categorical variables
    dummy_cols(select_columns=c("gender", "race_calculated", "SiteName")) %>%
    # compute the age at consent as the difference between the consent
    # date and the birth date
    mutate(birth_date = as.Date(birth_date, format="%m/%d/%Y")) %>%
    mutate(consent_date = as.Date(consent_date, format="%m/%d/%Y")) %>%
    mutate(age = as.numeric(consent_date - birth_date) %/% 365) %>%
    # remove columns we cleaned and no longer need
    select(-c(gender, race_calculated, SiteName, birth_date, consent_date))

# we have verified that there are no missing values in the baseline data
# print(anyNA(baseline_data))

# save the baseline data as an RDS file
saveRDS(baseline_data, file="baseline_data.RDS")
