# code for evaluating the secondary outcome LCVA

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# package for imputing data
library(mice)

# read the baseline covariate data
covariate_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# read the lcva data
data <- data.frame(read_excel(data_file_name, sheet="lcva")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only relevant columns
    select(c(PatientName, month, cseye_25_chart)) %>%
    # rename column slightly
    rename(lcva=cseye_25_chart) %>%
    # keep only observed rows of lcva
    filter(!is.na(lcva)) %>%
    # group by patient name
    group_by(PatientName) %>%
    # fill in the observation gaps for each patient
    complete(month=full_seq(month, 6)) %>%
    # merge in the covariate data
    inner_join(covariate_data, by="PatientName")

# use MICE to impute missing values for lcva in between visits
imp <- mice(data, m=1, maxit=20, seed=0)

data <- complete(imp, action=1)

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
data <- data %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data))

formulas <- create_formulas("treatment", "month", "PatientName", "lcva", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "lcva", "continuous"))
