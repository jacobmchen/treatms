# code for evaluating the secondary outcome timed 25 foot walk
# this outcome will be treated as continuous

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the imputed nhpt data
data_nondom <- readRDS("../primary_analysis/nhpt_nondom_data.RDS")
data_dom <- readRDS("../primary_analysis/nhpt_dom_data.RDS")

print(head(data_dom))
print(head(data_nondom))

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
data_dom <- data_dom %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data_dom))

# add a randomly generated treatment to the data for simulation
data_nondom <- data_nondom %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data_nondom))

# run analysis for dominant hand
print("dominant hand")

formulas <- create_formulas("treatment", "month", "PatientName", "dominant_hand_average_seconds", data_dom)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data_dom, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data_dom, 2, formula_red, formula_full, "dominant_hand_average_seconds", "continuous"))

# run analysis for non-dominant hand
print("non-dominant hand")

formulas <- create_formulas("treatment", "month", "PatientName", "non_dominant_hand_average_seconds", data_nondom)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data_nondom, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data_nondom, 2, formula_red, formula_full, "non_dominant_hand_average_seconds", "continuous"))
