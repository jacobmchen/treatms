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

# add a randomly generated treatment to the data for simulation
data <- data %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data))

formulas <- create_formulas("treatment", "month", "PatientName", "pdds_total_score", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "categorical"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "pdds_total_score", "categorical"))
