# code for evaluating the secondary outcome PDDS
# this is an ordinal outcome, so the model we fit will
# be slightly different

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the imputed EDSS and PDDS data
data <- readRDS("../primary_analysis/imputed_edss_pdds_data.RDS")

# preprocess the imputed data
data <- data %>%
    # get rid of columns we don't need
    select(-c(total_edss_score, fs_cfss_total,
              fsvs_on_total, fs_bfss_total,
              total_pyramidal_score, sensory_system_score_total,
              cerebellar_system_score_total, bowel_bladder_sys_score_total)) %>%
    # change the outcome to factor
    mutate(pdds_total_score = factor(pdds_total_score))

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
    ungroup() %>%
    # delete rows with a missing value for PDDS
    filter(!is.na(pdds_total_score))

# print the columns for debugging
print(colnames(data))
# print the number of rows of data
print(nrow(data))
# check whether there are duplicate rows
if (check_duplicate_rows(data)) stop("Fatal error: there are duplicate rows")

formulas <- create_formulas("treatment", "month", "PatientName", "pdds_total_score", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
# for PDDS, the run time of the chi-square test can take up to an hour
print(run_chi_square_test(data, formula_red, formula_full, "categorical"))

# run a bootstrap likelihood ratio test
# print(run_bootstrap_test(data, 2, formula_red, formula_full, "pdds_total_score", "categorical"))
