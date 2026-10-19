# code for evaluating the secondary outcome timed 25 foot walk
# this outcome will be treated as continuous

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the imputed t25fw data
data <- readRDS("../primary_analysis/t25fw_data.RDS") %>%
    filter(!is.na(trial_average_seconds))

print(head(data))

# set the seed so that the experiments are reproducible
set.seed(0)

# append the previously generated treatments to the data
treatment_data <- readRDS("../primary_analysis/treatment_data.RDS")

data <- data %>%
    # group by patient name so that each patient gets the same
    # treatment
    group_by(PatientName) %>%
    # get the treatment data
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

formulas <- create_formulas("treatment", "month", "PatientName", "trial_average_seconds", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "trial_average_seconds", "continuous"))
