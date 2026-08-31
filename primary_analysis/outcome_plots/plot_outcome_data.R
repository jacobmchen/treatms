# file for plotting outcome data to check for anomalies

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
    mutate(pdds_missing=ifelse(pdds_missing == 1, "Imputed", "Observed")) 

# note that as of 8/31/2026 there are 888 unique patients in the
# above data

# read the annotated nhpt data from saved data
nhpt_data <- readRDS("../annotated_nhpt_data.RDS") %>%
    # subset to only relevant columns
    select(c(PatientName, month, hand_average_seconds, nhpt_missing)) %>%
    # change binary indicator of imputed value to strings
    mutate(nhpt_missing=ifelse(nhpt_missing == 1, "Imputed", "Observed")) %>%
    rename(nhpt=hand_average_seconds)

# note that as of 8/31/2026 there are 886 unique patients in the
# above data

# read the annotated nhpt data from saved data
t25fw_data <- readRDS("../annotated_t25fw_data.RDS") %>%
    # subset to only relevant columns
    select(c(PatientName, month, trial_average_seconds, t25fw_missing)) %>%
    # change binary indicator of imputed value to strings
    mutate(t25fw_missing=ifelse(t25fw_missing == 1, "Imputed", "Observed")) %>%
    rename(t25fw=trial_average_seconds)

# note that as of 8/31/2026 there are 885 unique patients in the
# above data

# define a function to plot data
# @param data is the dataframe
# @param filename_prefix is the prefix to use for the filenames of the plots
# @param column_name is a string representing the data we want to plot
# @param missing_indicator is a string representing
# @return does not return anything, but will create plots in a local file
create_plots <- function(data, filename_prefix, column_name, missing_indicator) {
    # get the vector of unique patient ids
    patient_ids <- unique(edss_pdds_data$PatientName)

    for (i in seq(1, length(patient_ids), by=10)) {
        # check if next group of 10 exceeds the total number of patients
        # in the dataframe
        if (i+9 <= length(patient_ids)) {
            # if it does not, take the next 10 patients
            end_index <- i+9
        } else {
            # if it does, then go up to the last patient
            end_index <- length(patient_ids)
        }

        # subset to only the patients we want to plot
        data_to_plot <- data %>%
            filter(PatientName %in% patient_ids[i:end_index])

        # set a string for the filename
        filename <- paste0("plots/", filename_prefix, "_patients_", i, "-", end_index, ".pdf")

        # create the plot where each patient has its own line for edss
        p <- ggplot(data_to_plot, aes(x = month, y = .data[[column_name]], group = PatientName, color = PatientName)) +
          geom_line() +
          # specify that the shape of an observed value is a circle and an
          # imputed value is a triangle
          geom_point(aes(shape = .data[[missing_indicator]]), size = 2) +
          # use whether value is observed or imputed to decide shape
          scale_shape_manual(values = c("Observed" = 16, "Imputed" = 17)) +
          theme_minimal()

        # save the plot as a pdf file
        ggsave(filename, plot = p, width = 8, height = 6)
    }
}

create_plots(edss_pdds_data, "edss", "total_edss_score", "edss_missing")
create_plots(edss_pdds_data, "pdds", "pdds_total_score", "pdds_missing")
create_plots(nhpt_data, "nhpt", "nhpt", "nhpt_missing")
create_plots(t25fw_data, "t25fw", "t25fw", "t25fw_missing")
