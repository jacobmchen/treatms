# file for plotting changes between observations to check for anomalies

# package for operations on manipulating data
library(tidyverse)
# library for plotting graphs
library(ggplot2)

source("../global_variables.R")

# read the imputed EDSS and PDDS data with annotations;
# the annotations are simply an extra column that tells us
# whether a value for the outcome was originally missing/
# imputed
edss_pdds_data <- readRDS("../annotated_imputed_edss_pdds_data.RDS") %>%
    # subset to only relevant columns
    select(c(PatientName, month, total_edss_score, pdds_total_score,
             edss_missing, pdds_missing)) %>%
    # change the binary notation to a string indicating whether the value
    # was imputed or observed
    mutate(edss_missing=ifelse(edss_missing == 1, "Imputed", "Observed")) %>%
    mutate(pdds_missing=ifelse(pdds_missing == 1, "Imputed", "Observed")) %>%
    # keep only observed values since we only want to look at 
    # changes in observed values
    filter(edss_missing == "Observed") %>%
    filter(pdds_missing == "Observed") %>%
    select(-c(edss_missing, pdds_missing))

edss_pdds_data <- edss_pdds_data %>% 
    # sort by patient name and month
    arrange(PatientName, month) %>%
    # group data for all patients together
    group_by(PatientName) %>%
    # compute the change in edss between observations
    mutate(edss_change = total_edss_score - lag(total_edss_score)) %>%
    # do the same for pdds
    mutate(pdds_change = pdds_total_score - lag(pdds_total_score)) %>%
    # get the number of months in between observed values
    mutate(month_change = month - lag(month)) %>%
    # get the first observed edss value for each patient
    mutate(starting_edss = first(total_edss_score)) %>%
    mutate(starting_pdds = first(pdds_total_score))

# write the data with various subsets to csv files
write.csv(edss_pdds_data, "change_plots/edss_pdds_changes.csv", row.names=FALSE)
write.csv(edss_pdds_data %>% filter(abs(edss_change) >= 3), "change_plots/edss_pdds_changes_more_than_3.csv", row.names=FALSE)
write.csv(edss_pdds_data %>% filter(abs(edss_change) >= 2), "change_plots/edss_pdds_changes_more_than_2.csv", row.names=FALSE)

# make a histogram for edss change values
p <- ggplot(edss_pdds_data, aes(x=edss_change)) +
    geom_histogram(binwidth=0.5, fill="steelblue", color="white") +
    labs(title="Histogram of Change in Observed EDSS Between Visits") +
    theme(plot.margin = margin(10, 20, 10, 10))

# save the plot as pdf
ggsave("change_plots/edss_histogram.pdf", plot=p)

# make a histogram for pdds change values
p <- ggplot(edss_pdds_data, aes(x=pdds_change)) +
    geom_histogram(binwidth=1, fill="steelblue", color="white") +
    labs(title="Histogram of Change in Observed PDDS Between Visits") +
    theme(plot.margin = margin(10, 20, 10, 10))

# save the plot as pdf
ggsave("change_plots/pdds_histogram.pdf", plot=p)

# read the annotated nhpt data from saved data
nhpt_data <- readRDS("../annotated_nhpt_data.RDS") %>%
    # subset to only relevant columns
    select(c(PatientName, month, hand_average_seconds, nhpt_missing)) %>%
    # change binary indicator of imputed value to strings
    mutate(nhpt_missing=ifelse(nhpt_missing == 1, "Imputed", "Observed")) %>%
    rename(nhpt=hand_average_seconds) %>%
    filter(nhpt_missing == "Observed") %>%
    select(-nhpt_missing)

nhpt_data <- nhpt_data %>%
    arrange(PatientName, month) %>%
    group_by(PatientName) %>%
    mutate(nhpt_change = nhpt - lag(nhpt))

# make a histogram for nhpt change values
p <- ggplot(nhpt_data, aes(x=nhpt_change)) +
    geom_histogram(binwidth=5, fill="steelblue", color="white") +
    labs(title="Histogram of Change in Observed NHPT Between Visits") +
    theme(plot.margin = margin(10, 20, 10, 10))

# save the plot as pdf
ggsave("change_plots/nhpt_histogram.pdf", plot=p)

# read the annotated nhpt data from saved data
t25fw_data <- readRDS("../annotated_t25fw_data.RDS") %>%
    # subset to only relevant columns
    select(c(PatientName, month, trial_average_seconds, t25fw_missing)) %>%
    # change binary indicator of imputed value to strings
    mutate(t25fw_missing=ifelse(t25fw_missing == 1, "Imputed", "Observed")) %>%
    rename(t25fw=trial_average_seconds) %>%
    filter(t25fw_missing == "Observed") %>%
    select(-t25fw_missing)

t25fw_data <- t25fw_data %>%
    arrange(PatientName, month) %>%
    group_by(PatientName) %>%
    mutate(t25fw_change = t25fw - lag(t25fw))

# make a histogram for nhpt change values
p <- ggplot(t25fw_data, aes(x=t25fw_change)) +
    geom_histogram(binwidth=5, fill="steelblue", color="white") +
    labs(title="Histogram of Change in Observed T25FW Between Visits") +
    theme(plot.margin = margin(10, 20, 10, 10))

# save the plot as pdf
ggsave("change_plots/t25fw_histogram.pdf", plot=p)

write.csv(nhpt_data %>% filter(abs(nhpt_change) > 25), "change_plots/nhpt_outliers.csv", row.names=FALSE)


