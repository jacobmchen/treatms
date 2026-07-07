# code for evaluating the secondary outcome msfc
# this outcome will be treated as continuous

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# read the imputed t25fw data
t25fw <- readRDS("../primary_analysis/t25fw_data.RDS")

# read the imputed nhpt data
nhpt_nondom <- readRDS("../primary_analysis/nhpt_nondom_data.RDS") %>%
    select(c(PatientName, month, non_dominant_hand_average_seconds))
nhpt_dom <- readRDS("../primary_analysis/nhpt_dom_data.RDS") %>%
    select(c(PatientName, month, dominant_hand_average_seconds))

# read the observed pasat data, which is observed only every year
pasat <- readRDS("observed_pasat.RDS") %>%
    select(c(PatientName, month, pasat))

data <- t25fw %>%
    # join the three metrics needed to compute msfc
    # we do an inner join here so that rows where PASAT is
    # not observed will be ignored
    inner_join(nhpt_nondom, by=c("PatientName", "month")) %>%
    inner_join(nhpt_dom, by=c("PatientName", "month")) %>%
    inner_join(pasat, by=c("PatientName", "month")) %>%
    # compute the average of the inverse of nhpt
    mutate(nhpt_inverse = (1/dominant_hand_average_seconds + 1/non_dominant_hand_average_seconds)/2) %>%
    # get rid of rows we don't need
    select(-c(dominant_hand_average_seconds, non_dominant_hand_average_seconds))

# compute means and stds for nhpt, t25fw, and pasat
nhpt_inverse_mean <- mean(data$nhpt_inverse)
t25fw_mean <- mean(data$trial_average_seconds)
pasat_mean <- mean(data$pasat)
nhpt_inverse_std <- sd(data$nhpt_inverse)
t25fw_std <- sd(data$trial_average_seconds)
pasat_std <- sd(data$pasat)

# compute the msfc score
data <- data %>%
    mutate(msfc = ((nhpt_inverse - nhpt_inverse_mean) / nhpt_inverse_std - (trial_average_seconds - t25fw_mean) / t25fw_std + (pasat - pasat_mean) / pasat_std) / 3) %>%
    select(-c(nhpt_inverse, trial_average_seconds, pasat))

# set the seed so that the experiments are reproducible
set.seed(0)

# add a randomly generated treatment to the data for simulation
data <- data %>%
    group_by(PatientName) %>%
    mutate(treatment = rbinom(1, size=1, prob=0.5)) %>%
    ungroup()

print(colnames(data))

formulas <- create_formulas("treatment", "month", "PatientName", "msfc", data)
formula_red <- as.formula(formulas[1])
formula_full <- as.formula(formulas[2])

# run a single chi-square test
print(run_chi_square_test(data, formula_red, formula_full, "continuous"))

# run a bootstrap likelihood ratio test
print(run_bootstrap_test(data, 2, formula_red, formula_full, "msfc", "continuous"))
