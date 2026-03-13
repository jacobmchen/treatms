# file that contains global variables that 
# other files in the analysis will use

# package for operations on manipulating data
library(tidyverse)

# save file name for the longitudinal data
data_file_name = "../longitudinal_data_set_10MAR2026.xlsx"

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
