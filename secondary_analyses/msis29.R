# code for evaluating the outcome MSIS-29

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the msis-29 data
msis <- data.frame(read_excel(data_file_name, sheet="msis"))

# read the baseline covariate data
covariate_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# investigate the missingness of msis
msis <- msis %>%
    # create a new column that keeps track of how many 
    # subquestions are missing
    mutate(n_missing = rowSums(is.na(across(starts_with("q"))))) %>%
    # for now, just subset to the measurements that
    # have every subquestion observed
    filter(n_missing == 0) %>%
    # keep only rows that 
    filter(!is.na(completion_date))

msis <- msis %>%
    # rename the completion date column to make sure that the
    # compute_month_interval function runs properly
    rename(visit_date = completion_date) %>%
    # make a new column that keeps track of the original form group month
    mutate(formgroup_month=ifelse(str_detect(FormGroup, "Month"), as.integer(gsub("Month ", "", FormGroup)), NA))

# find the 6 month interval that corresponds to to the completion date
# of the exam; we do this because many rows have a form group with a
# non-month value
msis <- compute_month_interval(msis, c("msis29_score", "formgroup_month")) %>%
    # select only the relevant columns
    select(c(PatientName, closest_month, msis29_score, formgroup_month)) %>%
    # rename the column closest_month to just month
    rename(month=closest_month) %>%
    # if the original form group kept track of a month, then use that value;
    # otherwise, use the closest month calculated above based on time windows
    mutate(month=ifelse(!is.na(formgroup_month), formgroup_month, month)) %>%
    # get rid of the column formgroup_month
    select(-formgroup_month) %>%
    # remove duplicate rows 
    distinct(PatientName, month, .keep_all=TRUE) %>%
    # sort the data by patientname and month
    arrange(PatientName, month) %>%
    # merge covariate data
    group_by(PatientName) %>%
    inner_join(covariate_data, by="PatientName") %>%
    ungroup()

msis %>% slice_head(n=10) %>% print()

# set the seed so that the experiments are reproducible
set.seed(0)

# append the previously generated treatments to the data
treatment_data <- readRDS("../primary_analysis/treatment_data.RDS")

msis <- msis %>%
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
print(colnames(msis))
# print the number of rows of data
print(nrow(msis))
# check whether there are duplicate rows
if (check_duplicate_rows(msis)) stop("Fatal error: there are duplicate rows")

formulas <- create_formulas("treatment", "month", "PatientName", "msis29_score", msis)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(msis, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(msis, 2, formula_red, formula_full, "msis29_score", "continuous"))
