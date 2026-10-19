# code for evaluating the secondary outcome msfc
# this outcome will be treated as continuous

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the imputed t25fw data
t25fw <- readRDS("../primary_analysis/t25fw_data.RDS")

# read the imputed nhpt data
nhpt <- readRDS("../primary_analysis/nhpt_data.RDS") %>%
    select(c(PatientName, month, hand_average_seconds))

# read the observed pasat data, which is observed only every year
# this RDS file is created in pasat.R
pasat <- readRDS("pasat_data.RDS") %>%
    select(c(PatientName, month, pasat))

data <- t25fw %>%
    # join the three metrics needed to compute msfc
    full_join(nhpt, by=c("PatientName", "month")) %>%
    full_join(pasat, by=c("PatientName", "month")) %>%
    # compute the average of the inverse of nhpt
    mutate(nhpt_inverse = 1/hand_average_seconds) %>%
    # get rid of rows we don't need
    select(-c(hand_average_seconds)) %>%
    # keep only rows where all three values are observed
    filter(!is.na(trial_average_seconds) & !is.na(nhpt_inverse) & !is.na(pasat))

data %>% slice_head(n=10) %>% select(c(PatientName, month, trial_average_seconds, nhpt_inverse, pasat)) %>% print()

# compute means and stds for nhpt, t25fw, and pasat
nhpt_inverse_mean <- mean(data$nhpt_inverse)
t25fw_mean <- mean(data$trial_average_seconds)
pasat_mean <- mean(data$pasat)
nhpt_inverse_std <- sd(data$nhpt_inverse)
t25fw_std <- sd(data$trial_average_seconds)
pasat_std <- sd(data$pasat)

# compute the msfc score
data <- data %>%
    mutate(msfc = ((nhpt_inverse - nhpt_inverse_mean) / nhpt_inverse_std - (trial_average_seconds - t25fw_mean) / t25fw_std + (pasat - pasat_mean) / pasat_std) / 3) %>%
    select(-c(nhpt_inverse, trial_average_seconds, pasat))

# set the seed so that the experiments are reproducible
set.seed(0)

# append the previously generated treatments to the data
treatment_data <- readRDS("../primary_analysis/treatment_data.RDS")

data <- data %>%
    # group by patient name so that each patient gets the same
    # treatment
    group_by(PatientName) %>%
    # get the treatment data
    # note that we have to do an inner join here since not every
    # patient had enough data to compute MSFC values for
    inner_join(treatment_data, by="PatientName") %>%
    # rename the column for treatment group
    rename(treatment=treatment_group) %>%
    # ungroup the analysis by patients
    ungroup()

# print the columns for debugging
print(colnames(data))
# print the number of rows of data
print(nrow(data))
# check whether there are duplicate rows
if (check_duplicate_rows(data)) stop("Fatal error: there are duplicate rows")

# create formulas
formulas <- create_formulas("treatment", "month", "PatientName", "msfc", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "msfc", "continuous"))
