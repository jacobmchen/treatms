# file that contains global variables that 
# other files in the analysis will use

# package for operations on manipulating data
library(tidyverse)

# package for fitting models for secondary outcomes
library(glmmTMB)
library(ordinal)

# save file name for the longitudinal data
data_file_name <- "../longitudinal_data_set_2026-06-05.xlsx"

# save the string of the patient for whom we have no 
# data for and will be removed
no_data_patient <- "0225-016"

# declare a function that reads the msfc data
# and returns a version with the average for the t25fw and
# nhpt for dom and non-dom hands computed
compute_average_msfc <- function(msfc_data) {
    # all we need to do in this function is to create three additional columns
    # for the average of t25fw and nhpt for dom and non-dom hands
    msfc_data <- msfc_data %>%
        mutate(trial_average_seconds = (trial_one_seconds + trial_two_seconds)/2) %>%
        mutate(dominant_hand_average_seconds = (dominant_hand_t1_seconds + dominant_hand_t2_seconds)/2) %>%
        mutate(non_dominant_hand_average_seconds = (non_dom_hand_t1_seconds + non_dom_hand_t2_seconds)/2)

    # exception handling for patient with incorecctly entered 0 values
    msfc_data <- msfc_data %>%
        mutate(dominant_hand_average_seconds = ifelse(PatientName == "0216-012" & FormGroup == "Month 42", NA, dominant_hand_average_seconds))

    return(msfc_data)
}

# declare a function that takes in a dataframe of patient names,
# visit dates and returns a dataframe with a new column corresponding
# to the 6-month intervals at which the visit occurred
# Input: data that has two columns, PatientName and visit-date
# Output: data with one additional column corresponding to the 6 month
#         visit interval
compute_month_interval <- function(data) {
    # read the visit windows data
    visit_windows <- data.frame(read_excel(data_file_name, sheet="visit windows")) %>%
        rename(PatientName = Patient)

    # process visit windows data
    visit_windows <- visit_windows %>%
        # get only the patients who are in the data
        filter(PatientName %in% data$PatientName) %>%
        # renomve the columns Site and Status
        select(-c(Site, Status)) %>%
        # subtract one day to each close date
        # the across() function takes two arguments: a list of columns and
        # a function to apply to each column; we pass in every column that
        # starts with "close" and apply the function that subtracts 1
        # to each column. The syntax ~ .x - 1 is a purrr-style formula.
        mutate(across(starts_with("close"), ~ .x - 1))

    # get the names of the columns of the visit windows sheet after processing
    window_names <- names(visit_windows %>% select(-PatientName))

    # process the data to get the closest date to the visit date
    # where the exam-confirmed relapse was detected
    data <- merge(data, visit_windows, by="PatientName") %>%
        # make subsequent changes occur row-wise across the whole dataframe
        rowwise() %>%
        # create a new column called closest_month as follows
        mutate(closest_month = {
                # get the value of the visit_date
                v <- visit_date
                if (is.na(v)) print(PatientName)
                # get the values of the dates across all the time 
                # windows
                xs <- c_across(all_of(window_names))
                # return the name of the column that is closest in date
                # to visit_date by identifying the index where visit_date
                # and the value of the column are closest
                window_names[ which.min(abs(as.numeric(xs - v))) ]
            }
        ) %>%
        ungroup() %>%
        # change the string open or close to just the empty string
        # so that we can cast it as a number
        mutate(closest_month = sub("open", "", closest_month)) %>%
        mutate(closest_month = sub("close", "", closest_month)) %>%
        # cast the closest_month as a numeric variable
        mutate(closest_month = as.numeric(closest_month)) %>%
        # keep only the columns of patient name, visit date, and the
        # closest month
        select(c(PatientName, visit_date, closest_month))

    return(data)
}

###
# Below are functions related to fitting models for the secondary outcome.
###

# declare a helper function that fits a glmmTMB model using
# input for the formula, data, and outcome type
fit_glmmTMB <- function(formula, data, outcome_type) {
    if (outcome_type == "continuous")
        model <- glmmTMB(formula, family=gaussian(link = "identity"), data=data) 
    else if (outcome_type == "count")
        model <- glmmTMB(formula, family=poisson(link = "log"), data=data) 
    else if (outcome_type == "categorical")
        model <- clmm(formula, data=data)

    return(model)
}

# declare a function that takes in a dataframe, a formula for the reduced
# model (which does not include treatment-time coefficients), a formula
# for the full model (which does include treatment-time coefficients) 
# and runs a likelihood ratio test for the null hypothesis that the 
# extra coefficients in the full model are all equal to 0, the function
# then returns a p-value representing the probability of observing the
# data under the null hypothesis
# Input: (i) full dataset with treatment, outcome, covariate, and time data
#        (ii) a formula for the reduced model
#        (iii) a formula for the full model
#        (iv) a string specifying the type of the outcome data
# Output: p-value representing the probability of observing the data 
#         under the null hypothesis
run_chi_square_test <- function(data, formula_red, formula_full, outcome_type="continuous") {
    # fit a reduced model
    model_red <- fit_glmmTMB(formula_red, data, outcome_type)
    # fit a full model
    model_full <- fit_glmmTMB(formula_full, data, outcome_type)

    # if outcome type is categorical, compute the likelihood
    # ratio test manually to avoid mysterious error
    if (outcome_type == "categorical") {
        LL_red  <- logLik(model_red)
        LL_full <- logLik(model_full)

        LR <- 2 * (LL_full - LL_red)
        df <- attr(LL_full, "df") - attr(LL_red, "df")
        p <- pchisq(LR, df = df, lower.tail = FALSE)

        return(as.numeric(p))
    }

    # run a chi-square test to get a p-value and determine whether
    # including treatment-month interaction terms improves the fit
    test <- anova(model_red, model_full, test="Chisq")

    # return the p-value and result of the BIC score comparison
    return(test[2,8])
}

# declare a function that takes in a dataframe, the number of bootstraps,
# a formula for the reduced
# model (which does not include treatment-time coefficients), a formula
# for the full model (which does include treatment-time coefficients) 
# and runs a bootstrap likelihood ratio test for the null hypothesis that the 
# extra coefficients in the full model are all equal to 0, the function
# then returns a p-value representing the probability of observing the
# data under the null hypothesis
### 
# the bootstrap likelihood ratio test works by first computing the  
# likelihood ratio for the observed data then resampling the outcome
# a prespecified number of times and calculating a new likelihood 
# ratio each time. The p-value is computed as the proportion of times
# we observe a likelihood ratio larger than the observed data
# likelihood ratio
###
# Input: (i) full dataset with treatment, outcome, covariate, and time data
#        (ii) the number of bootstrap datasets to simulate
#        (iii) a formula for the reduced model
#        (iv) a formula for the full model
#        (v) a string for the name of the outcome variable
#        (vi) a string specifying the type of the outcome data
# Output: p-value representing the probability of observing the data 
run_bootstrap_test <- function(data, num_bootstraps, formula_red, formula_full,
                               outcome_var, outcome_type="continuous") {
    # fit a reduced model
    model_red <- fit_glmmTMB(formula_red, data, outcome_type)
    # fit a full model
    model_full <- fit_glmmTMB(formula_full, data, outcome_type)

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
        data_b[[outcome_var]] <- simulate(model_red, nsim=1)[[1]]

        # refit the reduced and full models using the dataset
        # with the resampled outcome; the update function
        # completely refits the model
        model_red_b <- update(model_red, data=data_b)
        model_full_b <- update(model_full, data=data_b)

        # calculate a new log-likelihood ratio from the models fitted on
        # the bootstrap data
        LR <- c(LR, 2*(logLik(model_full_b) - logLik(model_red_b)))
    }

    # compute the p-value as how often we observe a likelihood ratio
    # where the full model fits better than the reduced model compared
    # to the observed log-likelihood ratio
    p_val <- mean(LR >= LR_obs)

    return(p_val)
}

# declare a function that takes in strings for the treatment
# variable, month variable, outcome variable, and the dataset
# then returns a vector containing two strings: the reduced
# formula string and the full formula string, the reduced and full formulas
# are the same except for that the full formula contains
# treatment month interaction terms
# Input: (i) a string of the name of the treatment variable
#        (ii) a string of the name of the month variable
#        (iii) a string of the name of the patient_id variable
#        (iv) a string of the name of the outcome variable
#        (v) the dataframe
#        (vi) a string in formula form for what to add on to the treatment (blank by default)
# Output: a vector containing two strings, one for the reduced
#         formula, and one for the full formula
create_formulas <- function(treatment, month, patient_id, outcome, data, treatment_addon="") {
    # save a string representing the interaction term of the treatment
    # with the month of the study
    treatment_month <- paste0(treatment, ":factor(", month, ")")

    # save a string representing the random error term for each patient
    patient_error_term <- paste0("(1 | ", patient_id, ")")

    # save a string representing the coefficients for the month
    month_term <- paste0("factor(", month, ")")

    # save the list of covariates, which is all of the columns except
    # the treatment variable, patient ids, and the month
    covariates <- colnames(data)
    covariates <- covariates[!covariates %in% c(treatment, patient_id, month, outcome)]

    # if there is something to add on to the treatment, do it here
    if (treatment_addon != "")
        treatment <- paste0(treatment, " + ", treatment_addon)

    # if data contains time as a factor, the formula needs to include month
    # as a categorical variable, otherwise, the formula can just be outcome
    # as a function of covariates and the treatment term for the full model
    if (month != "") {
        # create the formula of the reduced model, which does not have the interaction
        # term between treatment and month of study
        formula_red <- paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month_term, patient_error_term), collapse=" + "))

        # create the formula of the full model, which does have the interaction
        # term between the treatment and month of study
        formula_full <- paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), month_term, treatment_month, patient_error_term), collapse=" + "))
    } else {
        formula_red <- paste(outcome, " ~ ", paste(covariates, collapse=" + "))

        formula_full <- paste(outcome, " ~ ", paste(c(paste(covariates, collapse=" + "), treatment), collapse=" + "))
    }
    
    # return the reduced and full formula strings
    return(c(formula_red, formula_full))
}
