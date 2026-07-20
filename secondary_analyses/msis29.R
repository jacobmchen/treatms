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
    filter(!is.na(completion_date))

msis <- msis %>%
    # rename the completion date column to make sure that the
    # compute_month_interval function runs properly
    rename(visit_date = completion_date)

# find the 6 month interval that corresponds to to the completion date
# of the exam
msis <- compute_month_interval(msis, c("msis29_score")) %>%
    # select only the relevant columns
    select(c(PatientName, closest_month, msis29_score)) %>%
    # rename the column closest_month to just month
    rename(month=closest_month) %>%
    # remove duplicate rows measured in the same "closest month"
    distinct(PatientName, month, .keep_all=TRUE) %>%
    # merge covariate data
    group_by(PatientName) %>%
    inner_join(covariate_data, by="PatientName") %>%
    ungroup()

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
msis <- msis %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

formulas <- create_formulas("treatment", "month", "PatientName", "msis29_score", msis)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(msis, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(msis, 2, formula_red, formula_full, "msis29_score", "continuous"))
