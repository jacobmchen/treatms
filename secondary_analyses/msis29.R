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

msis <- merge(msis, baseline_data, by="PatientName")

# create fake treatment data
set.seed(0)

msis <- msis %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(msis))
# fit two models as done in the SAP
library(glmmTMB)

# save the treatment, patient_id, time, and
# outcome variables separately
treatment <- "treatment"
treatment_month <- "treatment:factor(month)"
patient_id <- "(1 | PatientName)"
month <- "factor(month)"
outcome <- "msis29_score"
# list of covariates
covariates <- colnames(msis)
covariates <- covariates[!covariates %in% c("treatment", "PatientName", "month", outcome)]
print(covariates)

formula_red <- as.formula(paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month, patient_id), collapse=" + ")))

formula_full <- as.formula(paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month, treatment_month, patient_id), collapse=" + ")))

run_chi_square_test <- function(data) {
    model_red <- glmmTMB(formula_red, family=gaussian(link = "identity"), data=msis)
    # print(model_red)

    model_full <- glmmTMB(formula_full, family=gaussian(link = "identity"), data=msis)
    # print(model_full)

    # run a chi-square test to get a p-value and determine whether
    # treated group has lower mean scores
    test <- anova(model_red, model_full, test="Chisq")
    print(test)

    bic_test <- 0
    if (test[1,3] < test[2,3]) {
        bic_test <- 0
    } else {
        bic_test <- 1
    }

    return(c(test[2,8], bic_test))
}

run_bootstrap_test <- function(data, num_bootstraps) {
    model_red <- glmmTMB(formula_red, family=gaussian(link = "identity"), data=msis)
    # print(model_red)

    model_full <- glmmTMB(formula_full, family=gaussian(link = "identity"), data=msis)
    # print(model_full)

    LR_obs <- 2*(logLik(model_full) - logLik(model_red))

    LR <- c()

    for (b in 1:num_bootstraps) {
        data_b <- data
        data_b$msis29_score <- simulate(model_red, nsim=1)[[1]]

        model_red_b <- update(model_red, data=data_b)
        model_full_b <- update(model_full, data=data_b)

        LR <- c(LR, 2*(logLik(model_full_b) - logLik(model_red_b)))
    }

    # print(LR)
    # print(LR_obs)
    p_val <- mean(LR >= LR_obs)

    return(p_val)
}

significant <- 0
# bic_test <- 0

for (i in 1:20) {
    msis <- msis %>%
        group_by(PatientName) %>%
        mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
        ungroup()

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

    pval <- run_bootstrap_test(msis, 20)
    if (pval < 0.05)
        significant <- significant + 1
}
print(significant)
# print(bic_test)
