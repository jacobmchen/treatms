# file that contains global variables that 
# other files in the analysis will use

# package for operations on manipulating data
library(tidyverse)

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
