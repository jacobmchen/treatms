# Create a histogram for the number of days between visits for
# EDSS and MSFC

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)

source("global_variables.R")

# read the data for EDSS
edss_data <- data.frame(read_excel(data_file_name, sheet="edss")) %>%
    # select only relevant rows
    select(c(PatientName, visit_date, total_edss_score, fs_finding)) %>%
    # get rid of measurements with an exception finding
    filter(fs_finding != "Yes") %>%
    # sort by patient name and visit date
    arrange(PatientName, visit_date) %>%
    # get rid of rows with the same visit date
    distinct(PatientName, visit_date, .keep_all=TRUE) %>%
    group_by(PatientName) %>%
    # compute the difference between the visit date and the previous one
    mutate(days_since_previous = as.numeric(visit_date - lag(visit_date))) 

edss_data %>% filter(days_since_previous >= 2000) %>% slice_head(n=10) %>% print(width=Inf)

# get the vector of days between measurements without the missing values
intervals <- edss_data$days_since_previous[!is.na(edss_data$days_since_previous)]

# print the number of intervals
print(paste("length of intervals", length(intervals)))
# save as png
png(file="inter_visit_lengths.png", width=800, height=600, res=150)
hist(intervals, col="skyblue", main="Histogram of Days Between Visits", xlab="Days Between Visits")

# subset to intervals with days >=160 and <=180
intervals_160_180 <- intervals[intervals >= 160 & intervals <= 180]
print(paste("length of intervals >=160 and <=180", length(intervals_160_180)))
png(file="inter_visit_lengths_160-180.png", width=800, height=600, res=150)
hist(intervals_160_180, col="skyblue", main="Histogram of Days Between Visits (160-180)", xlab="Days Between Visits")

# subset to intervals with days <= 500
intervals_500 <- intervals[intervals <= 500]
print(paste("length of intervals <=500", length(intervals_500)))
png(file="inter_visit_lengths_500.png", width=800, height=600, res=150)
hist(intervals_500, col="skyblue", main="Histogram of Days Between Visits (<500)", xlab="Days Between Visits")

dev.off()
