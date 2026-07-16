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
    select(c(PatientName, month, pasat_forma, pasat_formb, pasat_statusa, pasat_statusb, trial_one_seconds,
             pasat_trial1, SiteName)) %>%
    mutate(pasat_trial1 = as.integer(pasat_trial1)) %>%
    # remove the sites Geisinger, UMass Worcester, USouth Alabama, and
    # MCR/Tidewater
    filter(SiteName != "Geisinger-0106") %>%
    filter(SiteName != "UMass Worcester-0420") %>%
    filter(SiteName != "USouth Alabama-0449") %>%
    filter(SiteName != "MCR/Tidewater-0411") %>%
    # if form A value is unobserved, copy over form B value
    mutate(pasat_forma = ifelse(is.na(pasat_forma), pasat_formb, pasat_forma)) %>%
    # delete the column for form B
    select(-pasat_formb) %>%
    # rename the form A column as just PASAT
    rename(pasat = pasat_forma) %>%
    # if form A status is unobserved, copy over form B status
    mutate(pasat_statusa = ifelse(is.na(pasat_statusa), pasat_statusb, pasat_statusa)) %>%
    # delete the column for form B status
    select(-pasat_statusb) %>%
    # rename the form A column as just PASAT status
    rename(pasat_status = pasat_statusa) %>%
    # keep only the months where we were supposed to record a PASAT value
    filter(month %% 12 == 0) %>%
    # keep only rows where we actually recorded a patient visit
    filter(!is.na(trial_one_seconds)) %>%
    # if the status is Not Obtained, treat the observed pasat as missing
    mutate(pasat = ifelse(pasat_status == "Not Obtained", NA, pasat)) %>%
    # if the status is Not physically able, treat the observed pasat as missing
    mutate(pasat = ifelse(pasat_status == "Not physically able", NA, pasat)) %>%
    # if status is Able but refused, use 6 times their practice trial if available,
    # otherwise treat as missing
    mutate(pasat = ifelse(pasat_status == "Able but subject refused", ifelse(!is.na(pasat_trial1), pasat_trial1*6, NA), pasat)) %>%
    # remove unneeded columns
    select(-c(trial_one_seconds, pasat_trial1, pasat_status, SiteName)) %>%
    # group by patient name
    group_by(PatientName) %>%
    # fill in gaps if there are any for the 12-month intervals
    complete(month=full_seq(month, 12)) %>%
    # merge in the covariate data
    inner_join(covariate_data, by="PatientName") %>%
    ungroup()

# use MICE to impute the missing values for SDMT
imp <- mice(data, m=1, maxit=20, seed=0)
data <- complete(imp, action=1)

# save as RDS the final pasat data
# this dataset will be used in the analysis of the outcome msfc
saveRDS(data, file="observed_pasat.RDS")

# look at the site data and see which sites are recording
# PASAT at lower rates
site_pasat_data <- data.frame(read_excel(data_file_name, sheet="msfc")) %>%
    # take the FormGroup string and change it to a number
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only relevant columns
    select(c(PatientName, month, SiteName, trial_one_seconds, pasat_forma, pasat_formb)) %>%
    # if form A value is unobserved, copy over form B value
    mutate(pasat_forma = ifelse(is.na(pasat_forma), pasat_formb, pasat_forma)) %>%
    # delete the column for form B
    select(-pasat_formb) %>%
    # rename the form A column as just PASAT
    rename(pasat = pasat_forma) %>%
    # keep only months where PASAT was scheduled to be recorded
    filter(month %% 12 == 0) %>%
    # keep only rows where t25fw was also recorded as a baseline
    filter(!is.na(trial_one_seconds)) %>%
    select(-trial_one_seconds) %>%
    # group by the sites
    group_by(SiteName) %>%
    # count the number of NAs for each site and rows for each
    # site and also save the percentage of NAs
    summarise(
        total_rows = n(),
        na_count = sum(is.na(pasat)),
        na_fraction = na_count / total_rows
    ) %>%
    ungroup() %>%
    arrange(desc(na_fraction))
 
# print(site_pasat_data)
# write.csv(site_pasat_data, file="site_pasat_data.csv", row.names=FALSE)

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
