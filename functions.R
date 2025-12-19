###
# This file contains functions that may be used by multiple files in this repository.
###

# define a function that returns a vector of observed values with every interim month filled in
create_full_vector <- function(censoring_index, cur_patient_value_truncated, 
                              cur_patient_month_truncated) {
    # create a named list that behaves like a dictionary where the key is the month and the
    # value is the edss value
    month_value_dict <- list()
    for (j in 1:censoring_index) {
        month_value_dict[[toString(cur_patient_month_truncated[j])]] <- cur_patient_value_truncated[j]
    }

    # first fill in any gaps in the months by creating a sequence that includes every
    # 6 month interval
    last_observed_month <- cur_patient_month_truncated[length(cur_patient_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)
    # use the named list to copy over the edss values at each 6 month interval; if a value
    # is not in the named list, then insert a missing value
    patient_values <- c()
    for (j in 1:length(patient_months)) {
        if (is.null(month_value_dict[[toString(patient_months[j])]])) {
            patient_values <- c(patient_values, NA)
        } else {
            patient_values <- c(patient_values, month_value_dict[[toString(patient_months[j])]])
        }
    }

    # return the full vector
    return(patient_values)
}
