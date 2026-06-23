# Code for the restricted mean survival time analysis.

# import the functions written by Diaz et al.
source("../estimator_functions.R")

# import package for tibble manipulations
library(tidyverse)

# declare a function that creates the long form dataset
# needed for the RMST analysis
create_dlong <- function(data) {
    # data processing before analysis
    data <- data %>%
        # keep only patients that have a censoring time greater
        # than 0, since if a patient was censored at month 0
        # they have no observed values
        filter(censor > 0) %>%
        # remove patients for whom we could not compute an event time,
        # which are patients with less than 3 observed event values
        # for EDSS and MSFC
        filter(!is.na(event_time)) %>%
        # group the dataset by patient names
        group_by(id) %>%
        # compute D, an indicator for whether we observed the event
        mutate(D = ifelse(event_time <= censor, 1, 0)) %>%
        # compute T, the minimum of the censoring time and the event
        # time
        mutate(T = min(censor, event_time)) %>%
        # remove the columns censor and event time as they are summarized by
        # T and D now
        select(-c(censor, event_time)) %>%
        # rename the treatment group column as A
        rename(A = treatment_group) 

    # restart the ordering of the ids from 1 to the size of the
    # actual dataset that we have to work with as the ordering
    # is important for the estimating function later
    data <- data %>%
        ungroup() %>%
        mutate(id = 1:nrow(data))

    # save the list of covariates as a single string to plug into
    # a formula later
    covars <- colnames(data %>% ungroup() %>% select(-c(A, T, id)))
    covars_m <- paste(c(covars, "m"), collapse=" + ")
    covars <- paste(covars, collapse=" + ")

    # the second parameter means don't rescale the time data at all
    dlong <- transformData(data, 1) 

    # fit initial estimators
    # fit a model for p(L_m=1 | I_m=1, A=a, W=w)
    # the A * (...) notation means that we include each of the main effects
    # inside the parantheses and A as well as all interaction terms between A and
    # those variables
    formula <- as.formula(paste(c("Lm ~ A + ", covars_m, ""), collapse=""))
    fitL <- glm(formula,
                data = dlong, subset = Im == 1, family = binomial())
    # fit a model for p(R_m=1 | J-m=1, A=a, W=w)
    formula <- as.formula(paste(c("Rm ~ A + ", covars_m, ""), collapse=""))
    fitR <- glm(formula,
                data = dlong, subset = Jm == 1, family = binomial())
    # fit a model for p(A=a | W=w)
    formula <- as.formula(paste(c("A ~ ", covars), collapse=""))
    fitA <- glm(formula,
                data = dlong, subset = m == 1, family = binomial())

    # add 5 additional columns to the dlong dataset
    # first column is predictions for R_m when the the treatment is always 1
    # second column is predictions for R_m when the treatment is always 0
    # third column is predictions for L_m when the the treatment is always 1
    # fourth column is predictions for L_m when the treatment is always 0
    # fifth column is predictions for the propensity score p(A=1)
    # the bound01() function is defined in estimator_functions.R and clips the
    # predicted probabilities
    dlong <- mutate(dlong,
                    gR1 = bound01(predict(fitR, newdata = mutate(dlong, A = 1), type = 'response')),
                    gR0 = bound01(predict(fitR, newdata = mutate(dlong, A = 0), type = 'response')),
                    h1 = bound01(predict(fitL, newdata = mutate(dlong, A = 1), type = 'response')),
                    h0 = bound01(predict(fitL, newdata = mutate(dlong, A = 0), type = 'response')),
                    gA1 = bound01(predict(fitA, newdata = mutate(dlong, A = 1), type = 'response')))

    return(dlong)
}

# set the restriction time at 84 months
tau <- 84

# # read the data
# data <- readRDS("full_data_all_clusters.RDS")
#
# # create long form data using the data with all clusters
# dlong_all <- create_dlong(data)
#
# print("RMST estimate for all clusters")
# print(tmle(dlong_all, tau))
#
# # read the data
# data <- readRDS("full_data_no_clusters.RDS")
#
# # create long form data using the data with all clusters
# dlong_no <- create_dlong(data)
#
# print("RMST estimate for no clusters")
# print(tmle(dlong_no, tau))
#
# # read the data
# data <- readRDS("full_data_merge_rare_clusters.RDS")
#
# # create long form data using the data with all clusters
# dlong_rare <- create_dlong(data)
#
# print("RMST estimate for rare clusters")
# print(tmle(dlong_rare, tau))
#
# # read the data
# data <- readRDS("full_data_merge_states.RDS")
#
# # create long form data using the data with all clusters
# dlong_states <- create_dlong(data)
#
# print("RMST estimate for state clusters")
# print(tmle(dlong_states, tau))

# see if the difference covers zero by computing Wald-type confidence intervals
# where the point estimate is the difference in theta and the standard error is the
# estimate given, still need to multiply critical values from standard normal distribution

# declare a function that, when run, runs a simulation study on the estimated
# variance when using various time restriction windows
time_restriction_simulation <- function(dlong) {
    # declare the restriction times we are interested in comparing variances for;
    # this will be month 12 to the max censoring time, month 84
    restriction_times <- seq(from=12, to=max(dlong$m), by=6)

    # declare a vector to store the estimated RMST
    estimate <- c()
    # declare a vector to store the estimated variances
    estimated_variance <- c()

    # iterate through the restriction times we want to compute estimates for
    for (t in restriction_times) {
        print(paste("restriction window", t))
        # compute estimates for the RMST at the current restriction time
        tmle_est <- tmle(dlong, t)
        print(tmle_est)

        # save the estimate for the RMST for the treatment group (only save one
        # because by simulation design treatment and non-treatment groups
        # will have similar estimates)
        estimate <- c(estimate, tmle_est$theta[1])
        # save the estimated variance
        estimated_variance <- c(estimated_variance, tmle_est$sdn)
    }

    # save the simulation results into a dataframe
    variance_data <- data.frame(restriction_time=restriction_times, estimate=estimate, variance=estimated_variance)
    print(variance_data)

    # save the simulation results into an RDS file
    saveRDS(variance_data, file="simulation_results.RDS")
}

# time_restriction_simulation(dlong_all)

set.seed(0)

# declare a function that, when run, runs a simulation study on
# the power of the rmst analysis in detecting an effect size of 12 months /
# 1 year
power_simulation <- function(num_simulations) {
    # read the data with states 
    data <- readRDS("full_data_merge_states.RDS")

    num_success <- 0

    for (i in 1:num_simulations) {
        # make a copy of the original dataset
        cur_data <- data

        # simulate treatment assignments again
        treatment_assignment <- rbinom(nrow(cur_data), 1, 0.5)

        # make the new simulated treatments the treatments
        cur_data$treatment_group <- treatment_assignment

        # for each person's event time, if they are in the treatment
        # group, increase it by 12 months
        cur_data <- cur_data %>%
            mutate(event_time = ifelse(treatment_group == 1 & !is.na(event_time) & is.finite(event_time),
                                       event_time + 12,
                                       event_time))

        # sample the dataset with the specified sample size
        cur_data <- cur_data %>%
            slice_sample(n=200, replace=FALSE)

        # create long form data using the data with all clusters
        dlong_states <- create_dlong(cur_data)

        # run the RMST analysis and get the outputs
        output <- tmle(dlong_states, tau)

        # compute the difference
        difference <- output$theta[2] - output$theta[1]
        # get the estimated standard error
        sdn <- output$sdn
        # compute the confidence interval
        conf_int <- c(difference - 1.96*sdn, difference+1.96*sdn)
        print(difference)
        print(conf_int)

        if (conf_int[1] > 0) {
            num_success <- num_success + 1
        }
    }

    return(num_success / num_simulations)
}

power_simulation(100)
