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

# print that what follows are just warmups
print("warm up tests")

# run a single chi-square test
print(run_chi_square_test(msis, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(msis, 2, formula_red, formula_full, "msis29_score", "continuous"))

# print that we are starting simulations
print("start simulations")

# define number of simulations to run
n_sim <- 100

# define number of bootstraps to run for each bootstrap test
n_bootstrap <- 200

# count how often we find that fitting a randomized treatment value
# helps
significant <- 0

# repeat for a certain number of simulations
for (i in 1:n_sim) {
    # create a dummy treatment variable
    msis <- msis %>%
        group_by(PatientName) %>%
        mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
        ungroup()

    # run a chi square test
    # output <- run_chi_square_test(msis)
    # pval <- output[1]
    # cur_bic <- output[2]
    #
    # if (pval < 0.05) {
    #     significant <- significant + 1
    # }
    # if (cur_bic == 1) {
    #     bic_test <- bic_test + 1
    # }

    # run a bootstrap test
    pval <- run_bootstrap_test(msis, n_bootstrap, formula_red, formula_full, "msis29_score", "continuous")
    # if the p-value is less than 0.05, count this test as significant
    if (pval < 0.05)
        significant <- significant + 1
}

# print the number of significant tests
print("number of significant simulations")
print(significant)
print("total number of simulations")
print(n_sim)
