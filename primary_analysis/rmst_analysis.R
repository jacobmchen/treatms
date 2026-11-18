# Code for the restricted mean survival time analysis.

# import the functions written by Diaz et al.
source("../estimator_functions.R")

# read the global variables
source("global_variables.R")

# import package for tibble manipulations
library(tidyverse)

# read the user input data that specifies the restriction time
args <- commandArgs(trailingOnly = TRUE)

# first check that the user actually entered an argument, if not then
# exit the program
if (length(args) < 1) {
    stop(paste(c("User did not supply argument for the restriction time of interest.",
         "Usage is: Rscript rmst_analysis.R 84",
         "The restriction time must be greater than 0."), collapse="\n"))
}

tau <- as.numeric(args[1])

# make sure that the user entered a valid restriction time
if (tau < 0) {
    stop(paste(c("User did not input a restriction time.",
                 "The restriction time must be greater than 0."), collapse="\n"))
}

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

# read the data
data <- readRDS("full_data_merge_states.RDS")

data <- data %>%
    # divide the times by 30 to get the closest
    mutate(censor = censor/30, event_time = event_time/30) %>%
    # round to the nearest integer
    mutate(censor = round(censor), event_time = round(event_time)) 

print("the maximum censoring time of the observed data")
print(max(data$censor))

# create long form data using the data with all clusters
dlong_states <- create_dlong(data)

print("RMST estimate for state clusters with exceptions")
output <- tmle(dlong_states, tau)

print("RMST estimates for treatment 0 and treatment 1")
print(paste(output$theta[1], output$theta[2]))

diff <- output$theta[2] - output$theta[1]
print("difference in RMST estimates")
print(diff)

print("95% confidence intervals")
print(c(diff - 1.96*output$sdn, diff + 1.96*output$sdn))

# exit the program here if don't want to run experiments below
q()

# set the seed for experiments below
set.seed(0)

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

# declare a function that, when run, runs a simulation study on
# the power of the rmst analysis in detecting an effect size of 12 months /
# 1 year
power_simulation <- function(num_simulations) {
    # read the data with states 
    data <- readRDS("full_data_merge_states.RDS")

    # declare a variable to record the number of "successful" confidence
    # intervals
    num_success <- 0
    # declare a vector for storing point estimates from the simulations
    point_estimates <- c()

    for (i in 1:num_simulations) {
        # make a copy of the original dataset
        cur_data <- data

        # simulate treatment assignments again
        treatment_assignment <- rbinom(nrow(cur_data), 1, 0.5)

        # make the new simulated treatments the treatments
        cur_data$treatment_group <- treatment_assignment

        # for each person's event time, if they are in the treatment
        # group, increase it by 6 months
        cur_data <- cur_data %>%
            mutate(event_time = ifelse(treatment_group == 1 & !is.na(event_time) & is.finite(event_time),
                                       event_time + 6,
                                       event_time)) %>%
            # if after increasing the event time it becomes greater than the
            # censoring time, make it a missing value; also take care not to
            # change missing and infinite values (infinite values means event never occurred)
            mutate(event_time = ifelse(event_time > censor & !is.na(event_time) & is.finite(event_time), NA, event_time))

        # sample the dataset with the specified sample size
        cur_data <- cur_data %>%
            slice_sample(n=450, replace=FALSE)

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

        point_estimates <- c(point_estimates, difference)

        if (conf_int[1] > 0) {
            num_success <- num_success + 1
        }
    }

    return(c(num_success / num_simulations, mean(point_estimates)))
}

# power_simulation(100)

# declare a function that, when run, runs a simulation study on
# the power of the rmst analysis in finding false positives
type_I_error_sim <- function(num_simulations) {
    # read the data with states 
    data <- readRDS("full_data_merge_states.RDS")

    # declare a variable to record the number of "successful" confidence
    # intervals that cover 0
    num_success <- 0
    # declare a vector for storing point estimates from the simulations
    point_estimates <- c()

    for (i in 1:num_simulations) {
        # make a copy of the original dataset
        cur_data <- data

        # simulate treatment assignments again
        treatment_assignment <- rbinom(nrow(cur_data), 1, 0.5)

        # make the new simulated treatments the treatments
        cur_data$treatment_group <- treatment_assignment

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

        point_estimates <- c(point_estimates, difference)

        if (conf_int[1] < 0 & conf_int[2] > 0) {
            num_success <- num_success + 1
        }
    }

    return(c(num_success / num_simulations, mean(point_estimates)))
}

type_I_error_sim(200)
