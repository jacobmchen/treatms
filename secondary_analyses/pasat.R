# code for evaluating the secondary outcome timed 25 foot walk
# this outcome will be treated as continuous

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# package for imputing data
library(mice)

# read the baseline covariate data
covariate_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# read the pasat data
data <- data.frame(read_excel(data_file_name, sheet="msfc")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only relevant columns
    select(c(PatientName, month, pasat_forma, pasat_formb)) %>%
    # if form A value is unobserved, copy over form B value
    mutate(pasat_forma = ifelse(is.na(pasat_forma), pasat_formb, pasat_forma)) %>%
    # delete the column for form B
    select(-pasat_formb) %>%
    # rename the form A column as just PASAT
    rename(pasat = pasat_forma) %>%
    # keep only observed rows of PASAT; note that this will get
    # rid of patients who have no observed PASAT values
    filter(!is.na(pasat)) %>%
    # group by patient name
    group_by(PatientName) %>%
    # fill in the observation gaps for each patient
    complete(month=full_seq(month, 6)) %>%
    # merge in the covariate data
    inner_join(covariate_data, by="PatientName")

# use MICE to impute missing values for PASAT in between visits
imp <- mice(data, m=1, maxit=20, seed=0)

data <- complete(imp, action=1)

# save as RDS the imputed pasat data
saveRDS(data, file="imputed_pasat.RDS")

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
data <- data %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data))

formulas <- create_formulas("treatment", "month", "PatientName", "pasat", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "pasat", "continuous"))
