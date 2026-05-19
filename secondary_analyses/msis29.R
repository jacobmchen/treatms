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

print(colnames(msis))
# fit two models as done in the SAP
library(glmmTMB)

# save the treatment, patient_id, time, and
# outcome variables separately

# save string of the treatment
treatment <- "treatment"
# save a string representing the interaction term of the treatment
# with the month of the study
treatment_month <- "treatment:factor(month)"
# save a string representing the random error term for each patient
patient_id <- "(1 | PatientName)"
# save a string representing the coefficients for the month
month <- "factor(month)"
# save a string for the outcome variable
outcome <- "msis29_score"
# save the list of covariates, which is all of the columns except
# the treatment variable, patient ids, and the month
covariates <- colnames(msis)
covariates <- covariates[!covariates %in% c("treatment", "PatientName", "month", outcome)]
print(covariates)

# create the formula of the reduced model, which does not have the interaction
# term between treatment and month of study
formula_red <- as.formula(paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month, patient_id), collapse=" + ")))

# create the formula of the full model, which does have the interaction
# term between the treatment and month of study
formula_full <- as.formula(paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month, treatment_month, patient_id), collapse=" + ")))

# code for running one chi square test given a dataset
run_chi_square_test <- function(data) {
    # fit a reduced model
    model_red <- glmmTMB(formula_red, family=gaussian(link = "identity"), data=msis)
    # print(model_red)

    # fit a full model
    model_full <- glmmTMB(formula_full, family=gaussian(link = "identity"), data=msis)
    # print(model_full)

    # run a chi-square test to get a p-value and determine whether
    # including treatment-month interaction terms improves the fit
    test <- anova(model_red, model_full, test="Chisq")
    print(test)

    # the output is a matrix, position [2,8] gives the p-value of the test
    # position [1,3] gives the BIC score of the reduced model
    # position [2,3] gives the BIC score of the full model

    bic_test <- 0
    if (test[1,3] < test[2,3]) {
        # the reduced model has a better BIC score than the full model
        bic_test <- 0
    } else {
        # the full model has a better BIC score
        bic_test <- 1
    }

    # return the p-value and result of the BIC score comparison
    return(c(test[2,8], bic_test))
}

# code for running a bootstrap test
run_bootstrap_test <- function(data, num_bootstraps) {
    # fit a reduced model
    model_red <- glmmTMB(formula_red, family=gaussian(link = "identity"), data=msis)
    # print(model_red)

    # fit a full model
    model_full <- glmmTMB(formula_full, family=gaussian(link = "identity"), data=msis)
    # print(model_full)

    # compute the log-likelihood ratio of the reduced and full model
    LR_obs <- 2*(logLik(model_full) - logLik(model_red))

    # declare a vector for storing likelihood ratios from the bootstrap
    # samples
    LR <- c()

    # iterate through the bootstraps
    for (b in 1:num_bootstraps) {
        # create a copy of the data
        data_b <- data
        # resample the outcome from the reduced model
        data_b$msis29_score <- simulate(model_red, nsim=1)[[1]]

        # refit the reduced and full models using the dataset
        # with the resampled outcome
        model_red_b <- update(model_red, data=data_b)
        model_full_b <- update(model_full, data=data_b)

        # calculate a new log-likelihood ratio from the models fitted on
        # the bootstrap data
        LR <- c(LR, 2*(logLik(model_full_b) - logLik(model_red_b)))
    }

    # print(LR)
    # print(LR_obs)

    # compute the p-value as how often we observe a likelihood ratio
    # where the full model fits better than the reduced model compared
    # to the observed log-likelihood ratio
    p_val <- mean(LR >= LR_obs)

    return(p_val)
}

# count how often we find that fitting a randomized treatment value
# helps
significant <- 0
# bic_test <- 0

# repeat for a certain number of simulations
for (i in 1:20) {
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
    pval <- run_bootstrap_test(msis, 20)
    # if the p-value is less than 0.05, count this test as significant
    if (pval < 0.05)
        significant <- significant + 1
}
# print the number of significant tests
print(significant)
# print(bic_test)
