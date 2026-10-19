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
    # merge in the covariate data
    inner_join(covariate_data, by="PatientName")

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
print(sum(is.na(data$lcva)))

formulas <- create_formulas("treatment", "month", "PatientName", "lcva", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "lcva", "continuous"))
