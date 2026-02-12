# Pre-process PDDS data and plot histograms for even
# numbered months, including month 0.

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

# read the data for PDDS
pdds_data <- data.frame(read_excel("../preliminary_longitudinal_data.xlsx", sheet="pdds"))

# clean up the data and select only the columns that are relevant to plotting histograms
pdds_data <- pdds_data %>% 
    # replace Month with empty string then cast the string as an integer
    mutate(month = as.integer(gsub("Month ", "", FormGroup))) %>%
    # keep only patient name, month, and edss score columns
    select(PatientName, month, pdds_total_score)

# get a sequence from 0 to 60 with a step of 6 to get measurements of PDDS
# separated by 6 months
months <- seq(0, 60, by=6)

for (m in months) {
    # subset to measurements at month m
    pdds_subset_data <- pdds_data %>% filter(month == m)

    # only plot histograms that have at least one observation for
    # that month
    if (nrow(pdds_subset_data) > 0) {
        png(paste0("pdds_month", m, ".png"))
        hist(pdds_subset_data$pdds_total_score,
             main=paste("Histogram of PDDS Score Measured at Month", m))
    }
}

# turn off the plotting device
dev.off()
