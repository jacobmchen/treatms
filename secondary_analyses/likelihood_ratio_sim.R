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

# accept command line arguments and save in a list called args
args = commandArgs(trailingOnly = TRUE)

# set the seed to the input from the command line
set.seed(args[1])

# add a randomly generated treatment to the data for simulation
msis <- msis %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

# create formulas
formulas <- create_formulas("treatment", "month", "PatientName", "msis29_score", msis)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# define a function that runs likelihood ratio test simulations
# n_sim is the number of simulations to run
# n_bootstrap is the number of bootstrap samples to use for each bootstrap test
run_likelihood_ratio_test_simulations <- function(n_sim, n_bootstrap) {
    # count how often we find that fitting a randomized treatment value
    # helps
    significant <- 0
    significant_bootstrap <- 0

    # repeat for a certain number of simulations
    for (i in 1:n_sim) {
        # create a dummy treatment variable
        msis <- msis %>%
            group_by(PatientName) %>%
            mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
            ungroup()

        # run a chi square test
        pval <- run_chi_square_test(msis, formula_red, formula_full, "continuous")
	    print(pval)
        
        if (pval < 0.05) {
            significant <- significant + 1
        }

        # run a bootstrap test
        pval <- run_bootstrap_test(msis, n_bootstrap, formula_red, formula_full, "msis29_score", "continuous")
	    print(pval)

        # if the p-value is less than 0.05, count this test as significant
        if (pval < 0.05) {
            significant_bootstrap <- significant_bootstrap + 1
        }
    }

    # print the number of significant tests
    # print("number of significant simulations for likelihood ratio test")
    # print(significant)
    # print("number of significant simulations for bootstrap likelihood ratio test")
    # print(significant_bootstrap)
}

# run a single simulation with 1000 bootstraps
run_likelihood_ratio_test_simulations(1, 1000)
