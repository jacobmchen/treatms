# code for evaluating the outcome MSIS-29

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the msis-29 data
msis <- data.frame(read_excel(data_file_name, sheet="msis"))

# investigate the missingness of msis
msis <- msis %>%
    # create a new column that keeps track of how many 
    # subquestions are missing
    mutate(n_missing = rowSums(is.na(across(starts_with("q"))))) %>%
    # for now, just subset to the measurements that
    # have every subquestion observed
    filter(n_missing == 0) %>%
    filter(!is.na(completion_date))

# use the completion date to get a month number for every observation
msis_month <- compute_month_interval(msis %>% select(c(PatientName, completion_date)) %>%
                                     rename(visit_date = completion_date)) %>%
    rename(completion_date = visit_date)

msis <- merge(msis, msis_month, by=c("PatientName", "completion_date")) %>%
    select(c(PatientName, closest_month, msis29_score)) %>%
    rename(month=closest_month) 

# merge the covariate data in
baseline_data <- readRDS("../primary_analysis/baseline_data_merge_states.RDS")

# merge the msis data with the baseline data
msis <- merge(msis, baseline_data, by="PatientName")

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

# run the simulation
run_likelihood_ratio_test_simulations(1, 1000)
