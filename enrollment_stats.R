# code for computing enrollment months and enrollment rate

# package for real excel files
library(readxl)

# package for data frame processing
library(tidyverse)
library(lubridate)

# read the baseline manuscript data
baseline <- data.frame(read_excel("baseline_data.xlsx", sheet="baseline_data"))

baseline <- baseline %>%
    select(c(patient_id, randomization_date)) %>%
    mutate(randomization_date=as.Date(randomization_date)) %>%
    # set the day of the date to 01 for every row so that we are only
    # comparing months
    mutate(randomization_month = floor_date(randomization_date, unit="month")) %>%
    # compute the earliest month
    mutate(earliest_month = min(randomization_month)) %>%
    # compute the difference between each person's randomization month
    # and earliest month
    mutate(months_since_earliest = interval(earliest_month, randomization_month) %/% months(1)) %>%
    select(c(patient_id, months_since_earliest)) 

print(head(baseline))

# print the last enrollment month
print(max(baseline$months_since_earliest))
print(nrow(baseline))

# get count data of how many patients were enrolled at each month
count_data <- baseline %>%
    count(months_since_earliest)

print(count_data)
