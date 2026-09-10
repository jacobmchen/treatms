# file that contains global variables that 
# other files in the analysis will use

# package for operations on manipulating data
library(tidyverse)

# package for reading excel files
library(readxl)

# package for handling dates
library(lubridate)

# save the substudy start and end dates
# march 1, 2020
substudy_start <- ymd("2020-03-01")
# march 31, 2022
substudy_end <- ymd("2022-03-31")

# save file name for the longitudinal data
covar_breakthrough_med_postpone <- "covid_substudy_data/covar_breakthrough_med_postpone.xlsx"

# create a dataframe with just the patient_ids
patient_id_data <- data.frame(read_excel(covar_breakthrough_med_postpone, sheet="Sheet1")) %>%
    select(patient_id)
