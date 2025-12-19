library(readxl)
library(stringr)

source("functions.R")

# read the data
baseline_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="baseline chars"))

# print number of rows in the data
print(paste("number of rows", nrow(baseline_data)))

# print the number of unique patient ids
print(paste("number of unique patient ids", length(unique(baseline_data$PatientName))))

# get a table for the patient status
print("table of patient status")
print(table(baseline_data$patient_status))

# get EDSS data
edss_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="edss"))

# get MSFC data for timed walk and nine hole peg test data
msfc_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="msfc"))

# get the patient IDs and print the number of different patients
# note that we have also verified that the patient IDs in the EDSS data and the
# MSFC data are exactly the same
patient_ids <- unique(edss_data$PatientName)
print("number of unique patient_ids in EDSS data")
print(length(patient_ids))

# get the site IDs and print the number of different sites
site_ids <- unique(edss_data$SiteName)
print("number of unique site_ids in EDSS data")
print(length(site_ids))

# convert the FormGroup column to numbers
# the FormGroup column tells us the month at which the visit was conducted (0, 6, 12, etc.)
FormGroup_num <- as.integer(word(edss_data$FormGroup, 2))
edss_data$FormGroup_num <- FormGroup_num
# do the same for MSFC data
FormGroup_num <- as.integer(word(msfc_data$FormGroup, 2))
msfc_data$FormGroup_num <- FormGroup_num

# create a function that takes in a missingness vector and formgroup vector and
# returns a two value vector containing the month and index at which there is censoring
compute_censoring_time <- function(is_missing, cur_patient_formgroup_num) {
    # catch the corner case that there are no observed edss scores,
    # by counting the number of missing values in the vector is_missing and
    # comparing it to the length of the vector
    if (sum(is_missing) == length(is_missing)) {
        # assign default values of -1
        month_censored <- -1
        index_censored <- -1
    } else {
        month_censored <- 0
        # iterate through each index
        for (i in 1:length(is_missing)) {
            # if the count of missing values after this index is the same as the
            # number of values left in the vector, then we are censored after this
            # also remember the corner case where we're just at the last value of the vector
            if (i == length(is_missing) | sum(is_missing[(i+1):length(is_missing)]) == length(is_missing)-i) {
                month_censored <- cur_patient_formgroup_num[i]
                index_censored <- i
                break
            }
        }    
    }
    
    # create a vector containing the month and index of censoring
    result <- c(month_censored, index_censored)

    return(result)
}

# record the censoring time for each patient, which is defined as
# the time point at which every value after is missing
censoring_time <- c()
# also record the index at which you become censored
censoring_index <- c()

# record the censoring time and index for t25fw and nhpt
censoring_time_t25fw <- c()
censoring_index_t25fw <- c()
censoring_time_nhpt <- c()
censoring_index_nhpt <- c()

# iterate through all of the patients
for (patient_id in patient_ids) {
    # first get all of the relevant information for this patient

    # get EDSS data for this patient
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num
    # get the index at which every recorded value is missing after, i.e. the 
    # censoring time
    # step 1: get a vector of 0s and 1s representing whether the value is missing
    is_missing <- as.integer(is.na(cur_patient_edss))

    # repeat the process for MSFC data
    # get MSFC data for this patient
    # note that sometimes observation times for t25fw can be different from
    # nhpt observation
    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num
    cur_patient_t25fw_status <- cur_patient_msfc_data$trial_average_seconds
    cur_patient_nhpt_status <- cur_patient_msfc_data$dominant_hand_average_seconds
    t25fw_is_missing <- as.integer(is.na(cur_patient_t25fw_status))
    nhpt_is_missing <- as.integer(is.na(cur_patient_nhpt_status))
    
    # compute censoring time for EDSS
    result <- compute_censoring_time(is_missing, cur_patient_formgroup_num)

    # add this censoring time to the vector
    censoring_time <- c(censoring_time, result[1])
    censoring_index <- c(censoring_index, result[2])       

    # compute censoring time for t25fw
    result <- compute_censoring_time(t25fw_is_missing, cur_patient_msfc_formgroup_num)

    # add this censoring time to the vector
    censoring_time_t25fw <- c(censoring_time_t25fw, result[1])
    censoring_index_t25fw <- c(censoring_index_t25fw, result[2])       

    # compute censoring time for t25fw
    result <- compute_censoring_time(nhpt_is_missing, cur_patient_msfc_formgroup_num)

    # add this censoring time to the vector
    censoring_time_nhpt <- c(censoring_time_nhpt, result[1])
    censoring_index_nhpt <- c(censoring_index_nhpt, result[2])       
}

print(paste("number of patients with no EDSS observations:", length(which(censoring_index == -1))))
print(paste("number of patients with no t25fw observations:", length(which(censoring_index_t25fw == -1))))
print(paste("number of patients with no nhpt observations:", length(which(censoring_index_nhpt == -1))))

# keep track of patients with observed values of EDSS
observed_patient_ids <- patient_ids[-which(censoring_index == -1)]
observed_censoring_time <- censoring_time[-which(censoring_index == -1)]
observed_censoring_index <- censoring_index[-which(censoring_index == -1)]

# keep track of patients with observed values of t25fw
observed_patient_ids_t25fw <- patient_ids[-which(censoring_index_t25fw == -1)]
observed_censoring_time_t25fw <- censoring_time_t25fw[-which(censoring_index_t25fw == -1)]
observed_censoring_index_t25fw <- censoring_index_t25fw[-which(censoring_index_t25fw == -1)]

# keep track of patients with observed values of nhpt
observed_patient_ids_nhpt <- patient_ids[-which(censoring_index_nhpt == -1)]
observed_censoring_time_nhpt <- censoring_time_nhpt[-which(censoring_index_nhpt == -1)]
observed_censoring_index_nhpt <- censoring_index_nhpt[-which(censoring_index_nhpt == -1)]

print(paste("average observed censoring time EDSS:", mean(observed_censoring_time)))
print(paste("median observed censoring time EDSS:", median(observed_censoring_time)))

print(paste("average observed censoring time t25fw:", mean(observed_censoring_time_t25fw)))
print(paste("median observed censoring time t25fw:", median(observed_censoring_time_t25fw)))

print(paste("average observed censoring time nhpt:", mean(observed_censoring_time_nhpt)))
print(paste("median observed censoring time nhpt:", median(observed_censoring_time_nhpt)))
print("")

# now get the number of missing values for the edss score prior to censoring for each patient
num_missing_observations <- c()
percent_missing_observations <- c()

# we also want to get how many times missing values are surrounded by observed values
# and how many consecutive missingness; we can keep track of this by making a list
# of vectors each representing the "span" of missingness for a particular patient
missingness_spans <- list()

# create a vector for storing patient IDs where there is a missingness span of more than 3
missingness_span_morethan3 <- c()
missingness_span_morethan2 <- c()

# create a vector for patient IDs where the first value is missing
first_value_missing <- c()

# keep track of baseline EDSS, which is the EDSS score at month 0;
# note that 17 patients don't have a baseline EDSS, in which case
# their baseline EDSS is a missing value
baseline_edss <- c()

# keep track of whether a patient has disease progression, which is denoted as
# (a) increase of >= 1.0 if baseline EDSS is <= 5.5 or (b) increase of >= 0.5
# if baseline EDSS is >= 6.0; this will be a vector of binary variables
disease_progression <- c()

# keep track of patients that have unsustained disease progression
patient_unsustained_progression <- c()

# keep track of amongst progression how many missing values are between
# the significant change and the sustained change
progress_missing_vals_between <- c()

# keep track of which patients have less than 3 observed values,
# preventing us from computing disease progression
patient_less_than_three <- c()

# keep track of whether a patient has disease progression with respect to
# timed 25 foot walk or nine hole peg test. disease progression is defined
# as an increase of 20% or more between 6 months
disease_progression_t25fw <- c()
patient_t25fw_unsustained_progression <- c()
progress_t25fw_missing_vals_between <- c()
patient_t25fw_less_than_three <- c()

disease_progression_nhpt <- c()
patient_nhpt_unsustained_progression <- c()
progress_nhpt_missing_vals_between <- c()
patient_nhpt_less_than_three <- c()

# get the spans of missingness, which is a vector of missingness lengths
# ex. 1 the vector NA NA 1 1 NA 1 1 NA NA NA 1 will return 2, 1, 3
compute_missingness_span <- function(patient_values) {
    # declare an empty vector to save output
    cur_missingness_span <- c()

    # declare an iterator that will walk through patient_values
    j <- 1
    while (j <= length(patient_values)) {
        # check if current value is missing
        if (is.na(patient_values[j])) {
            cnt <- 1
            # walk forward until we reach something that is observed
            for (k in (j+1):length(patient_values)) {
                # if the next value is also missing, increment cnt
                if (is.na(patient_edss[k])) {
                    cnt <- cnt + 1
                } else {
                    # otherwise break out of the loop and update iterator
                    # to where we walked forward to
                    j <- k
                    break
                }
            }
            # add the count of missing values to the missingness span
            cur_missingness_span <- c(cur_missingness_span, cnt)
        } else {
            # if the current value is missing, just walk to the next index
            # for the iterator
            j <- j + 1
        }
    }

    return(cur_missingness_span)
}

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data for EDSS
for (patient_id in observed_patient_ids) {
    missing_cnt <- 0

    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num

    # get the edss scores and months up to the censored index
    cur_patient_edss_truncated <- cur_patient_edss[1:observed_censoring_index[i]]
    cur_patient_month_truncated <- cur_patient_formgroup_num[1:observed_censoring_index[i]]

    patient_edss <- create_full_vector(observed_censoring_index[i], cur_patient_edss_truncated,
                                            cur_patient_month_truncated)

    last_observed_month <- cur_patient_month_truncated[length(cur_patient_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)

    cur_missingness_span <- compute_missingness_span(patient_edss)

    # compute disease progression
    # first check if they have a baseline value
    if (is.na(patient_edss[1])) {
        first_value_missing <- c(first_value_missing, patient_id)
        baseline_edss <- c(baseline_edss, NA)
        # if there is no baseline EDSS, then we don't know if their
        # disease progressed (can change later if needed)
        disease_progression <- c(disease_progression, NA)
    } else {
        baseline_edss <- c(baseline_edss, patient_edss[1])
        baseline <- patient_edss[1]
        # evaluate if there was disease progression
        if (patient_edss[1] <= 5.5) threshold <- 1
        else threshold <- 0.5

        # get only the observed values of EDSS
        patient_edss_observed <- patient_edss[!is.na(patient_edss)]
        patient_months_observed <- patient_months[!is.na(patient_edss)]

        # Inf means EDSS did not progress, otherwise return the index at which they experienced
        # progression
        progress <- Inf
        # first check that there is at least two observed values, since
        # we need to check of sustained progression
        if (length(patient_edss_observed) > 2) {
            # iterate through all of the observed values
            for (j in 2:(length(patient_edss_observed)-1)) {
                # check if the change is more than the threshold
                # and if the change is sustained
                if (patient_edss_observed[j] - baseline >= threshold & patient_edss_observed[j+1] - baseline >= threshold) {
                    progress <- patient_months_observed[j]
                    break
                }
            }

            # evaluate how often patients have disease progression that is
            # not sustained
            for (j in 2:(length(patient_edss_observed)-1)) {
                if (patient_edss_observed[j] - baseline >= threshold & patient_edss_observed[j+1] - baseline < threshold) {
                    patient_unsustained_progression <- c(patient_unsustained_progression, patient_id)
                }
            }
        } else {
            # if there is only one observed value, we don't know if their disease
            # progressed
            progress <- NA

            # save the patient ids with less than three observed values
            patient_less_than_three <- c(patient_less_than_three, patient_id)
        }

        disease_progression <- c(disease_progression, progress)

        # amongst people who have disease progression, how many people have
        # one missing value between progression and sustainment? two
        # missing values? three or more missing values?
        if (!is.na(progress) & !is.infinite(progress)) {
            progress_index <- which(patient_months == progress)

            # from the progress_index, get the index of the next observed value
            patient_edss_after_progression <- patient_edss[(progress_index+1):length(patient_edss)]
            first_observed_index <- which(is.na(patient_edss_after_progression) == FALSE)[1]

            progress_missing_vals_between <- c(progress_missing_vals_between, first_observed_index-1)
        }
    }

    # see if the current missingness span contains a span greater than 3
    if (!is.null(cur_missingness_span) ) {
        if (max(cur_missingness_span) > 3) {
            missingness_span_morethan3 <- c(missingness_span_morethan3, patient_id)
        }
        if (max(cur_missingness_span) > 2) {
            missingness_span_morethan2 <- c(missingness_span_morethan2, patient_id)
        }
    }

    # save the missingness spans in a list
    missingness_spans[[i]] <- cur_missingness_span

    # the number of missing observations is the number of missing values in patient_edss
    num_missing_observations <- c(num_missing_observations, sum(is.na(patient_edss)))
    # the percent of missing observations is the number of missing values divided by the total length
    percent_missing_observations <- c(percent_missing_observations, sum(is.na(patient_edss))/length(patient_edss))

    i <- i + 1
}
print(paste("average number of missing rows for each patient:", mean(num_missing_observations)))
print(paste("average percent of missing rows for each patient:", mean(percent_missing_observations)))
print(paste("number of patients with more than 3 consecutive missing values", length(missingness_span_morethan3)))
print(paste("number of patients with more than 2 consecutive missing values", length(missingness_span_morethan2)))
# missing EDSS values are not bookended by observed values if and only if there is no
# observed first value since we are only considering up until the censoring timepoint
print(paste("number of patients with first value missing", length(first_value_missing)))
print(paste("mean of baseline EDSS, ignoring missing", mean(baseline_edss, na.rm=TRUE)))
print(paste("median of baseline EDSS, ignoring missing", median(baseline_edss, na.rm=TRUE)))
# a value is missing if we didn't have baseline or if there is only one observed value
print(paste("number of missing values for EDSS progression", sum(is.na(disease_progression))))
disease_progression_no_inf <- ifelse(is.infinite(disease_progression), NA, disease_progression)
print(paste("mean of EDSS disease progression, ignoring missing, ignoring no progression", mean(disease_progression_no_inf, na.rm=TRUE)))
print(paste("number of patients with disease progression", sum(!is.na(disease_progression_no_inf))))
print(paste("length of disease_progression", length(disease_progression)))
print(paste("number of patients that experience unsustained disease progression", length(unique(patient_unsustained_progression))))
print(paste("total number of times we observe unsustained disease progression (1 patient may have multiple observations)", length(patient_unsustained_progression)))
print(paste("among those with disease progression, number with 1 missing value between progression and sustained progression", sum(progress_missing_vals_between == 1)))
print(paste("among those with disease progression, number with 2 missing values between progression and sustained progression", sum(progress_missing_vals_between == 2)))
print(paste("among those with disease progression, number with 3+ missing values between progression and sustained progression", sum(progress_missing_vals_between >= 3)))
saveRDS(patient_less_than_three, file = "patient_less_than_three_edss.RDS")
print("")

# create a dataframe for EDSS progression
EDSS_progression_df <- data.frame(patient_id=observed_patient_ids,
                                    EDSS_progression=disease_progression)

# define a function that returns how many times a patient experiences
# unsustained disease progression (they have a significant change but the 
# following observation is not significant)
msfc_unsustained_progression <- function(patient_values, patient_months) {
    # get only the observed values of patient values
    patient_values_observed <- patient_values[!is.na(patient_values)]
    patient_months_observed <- patient_months[!is.na(patient_months)]

    # if the baseline value is missing, then return 0 since we cannot
    # calculate unsustained progression for this patient
    if (is.na(patient_values[1])) {
        return(0)
    }

    # get the baseline measurement
    baseline <- patient_values[1]

    # keep track of a counter
    cnt <- 0
    # count the number of times there was unsustained disease progression
    if (length(patient_values_observed) > 2) {
        for (j in 2:(length(patient_values_observed)-1)) {
            # check if there is an at least 20% increase in value and 
            # check whether that increase was sustained at the next observed value
            if (patient_values_observed[j] >= 1.2*baseline &
                patient_values_observed[j+1] < 1.2*baseline) {
                cnt <- cnt + 1
            }
        }
    } 

    # return the number of times there was unsustained
    # disease progression
    return(cnt)
}

# for patients with msfc progression, compute the number of missing values
# between the observed progression and observed sustainment
msfc_missing_values_between <- function(patient_values, patient_months, progress) {
    # amongst people who have disease progression, how many people have
    # one missing value between progression and sustainment? two
    # missing values? three or more missing values?
    progress_index <- which(patient_months == progress)

    # from the progress_index, get the index of the next observed value
    patient_values_after_progression <- patient_values[(progress_index+1):length(patient_values)]
    first_observed_index <- which(is.na(patient_values_after_progression) == FALSE)[1]

    return(first_observed_index-1)
}

msfc_less_than_3 <- function(patient_values) {
    patient_values_observed <- patient_values[!is.na(patient_values)]

    if (length(patient_values_observed) > 2) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# define a function that returns Inf, month of progression, or NA for whether a patient had disease progression
# for an MSFC score
msfc_progression <- function(patient_values, patient_months) {
    # get only the observed values of patient values
    patient_values_observed <- patient_values[!is.na(patient_values)]
    patient_months_observed <- patient_months[!is.na(patient_months)]

    # if the baseline value is missing, then return NA
    if (is.na(patient_values[1])) {
        return(NA)
    }

    # get the baseline measurement
    baseline <- patient_values[1]

    # check if there was progression for this MSFC metric
    # first check if there are at least two values that are observed
    # since we need to check whether progression was sustained
    if (length(patient_values_observed) > 2) {
        for (j in 2:(length(patient_values_observed)-1)) {
            # check if there is an at least 20% increase in value and 
            # check whether that increase was sustained at the next observed value
            if (patient_values_observed[j] >= 1.2*baseline &
                patient_values_observed[j+1] >= 1.2*baseline) {
                return(patient_months[j])
            }
        }
    } else {
        # if there were 2 or less observed values, then we cannot check sustained progression
        return(NA)
    }

    # there was no disease progression
    return(Inf)
}

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data
# now do the same for t25fw
for (patient_id in observed_patient_ids_t25fw) {
    # get the patient's t25fw data
    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_t25fw <- cur_patient_msfc_data$trial_average_seconds
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num

    # get the t25fw and months data up to the censored index
    cur_patient_t25fw_truncated <- cur_patient_t25fw[1:observed_censoring_index_t25fw[i]]
    cur_patient_t25fw_month_truncated <- cur_patient_msfc_formgroup_num[1:observed_censoring_index_t25fw[i]]

    # create a vector of the t25fw data with missing months filled in
    patient_t25fw <- create_full_vector(observed_censoring_index_t25fw[i], cur_patient_t25fw_truncated,
                                            cur_patient_t25fw_month_truncated)

    # create a vector corresponding to the full vector of the months that the data corresponds to
    last_observed_month <- cur_patient_t25fw_month_truncated[length(cur_patient_t25fw_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)

    # compute whether the patient had disease progression and when
    progress <- msfc_progression(patient_t25fw, patient_months)
    disease_progression_t25fw <- c(disease_progression_t25fw, progress)
    unsustained_count <- msfc_unsustained_progression(patient_t25fw, patient_months)
    while (unsustained_count > 0) {
        patient_t25fw_unsustained_progression <- c(patient_t25fw_unsustained_progression, patient_id)
        unsustained_count <- unsustained_count - 1
    }

    if (!is.na(progress) & !is.infinite(progress)) {
        progress_t25fw_missing_vals_between <- c(progress_t25fw_missing_vals_between, msfc_missing_values_between(patient_t25fw, patient_months, progress))
    }

    if (msfc_less_than_3(patient_t25fw)) {
        patient_t25fw_less_than_three <- c(patient_t25fw_less_than_three, patient_id)
    }

    i <- i + 1
}
print(paste("number of missing values in disease progression of t25fw", sum(is.na(disease_progression_t25fw))))
disease_progression_t25fw_no_inf <- ifelse(is.infinite(disease_progression_t25fw), NA, disease_progression_t25fw)
print(paste("mean of disease progression of t25fw, no missing, ignore no progression", mean(disease_progression_t25fw_no_inf, na.rm=TRUE)))
print(paste("number of patients with t25fw disease progression", sum(!is.na(disease_progression_t25fw_no_inf))))
print(paste("number of patients with t25fw unsustained progression", length(unique(patient_t25fw_unsustained_progression))))
print(paste("among those with t25fw disease progression, number with 1 missing value between progression and sustained progression", sum(progress_t25fw_missing_vals_between == 1)))
print(paste("among those with t25fw disease progression, number with 2 missing values between progression and sustained progression", sum(progress_t25fw_missing_vals_between == 2)))
print(paste("among those with t25fw disease progression, number with 3+ missing values between progression and sustained progression", sum(progress_t25fw_missing_vals_between >= 3)))
saveRDS(patient_t25fw_less_than_three, file = "patient_less_than_three_t25fw.RDS")
print("")

# create a dataframe for t25fw progression
t25fw_progression_df <- data.frame(patient_id=observed_patient_ids_t25fw,
                                    t25fw_progression=disease_progression_t25fw)

# declare an empty vector to keep track of nhpt disease progression
disease_progression_nhpt <- c()

# keep track of an iterating counter
i <- 1
# note here that we are only considering patients with observed data
for (patient_id in observed_patient_ids_nhpt) {
    # get the patient's nhpt data
    cur_patient_msfc_data <- msfc_data[msfc_data$PatientName == patient_id, ]
    cur_patient_nhpt <- cur_patient_msfc_data$dominant_hand_average_seconds
    cur_patient_msfc_formgroup_num <- cur_patient_msfc_data$FormGroup_num

    # get the nhpt up to the censored index
    cur_patient_nhpt_truncated <- cur_patient_nhpt[1:observed_censoring_index_nhpt[i]]
    cur_patient_nhpt_month_truncated <- cur_patient_msfc_formgroup_num[1:observed_censoring_index_nhpt[i]]

    # create a vector of the nhpt data with missing months filled in
    patient_nhpt <- create_full_vector(observed_censoring_index_nhpt[i], cur_patient_nhpt_truncated,
                                            cur_patient_nhpt_month_truncated)

    # create a vector corresponding to the full vector of the months that the data corresponds to
    last_observed_month <- cur_patient_nhpt_month_truncated[length(cur_patient_nhpt_month_truncated)]
    patient_months <- seq(0, last_observed_month, by=6)

    # compute whether the patient had disease progression and when
    progress <- msfc_progression(patient_nhpt, patient_months)
    disease_progression_nhpt <- c(disease_progression_nhpt, progress)
    unsustained_count <- msfc_unsustained_progression(patient_nhpt, patient_months)
    while (unsustained_count > 0) {
        patient_nhpt_unsustained_progression <- c(patient_nhpt_unsustained_progression, patient_id)
        unsustained_count <- unsustained_count - 1
    }

    if (!is.na(progress) & !is.infinite(progress)) {
        progress_nhpt_missing_vals_between <- c(progress_nhpt_missing_vals_between, msfc_missing_values_between(patient_nhpt, patient_months, progress))
    }

    if (msfc_less_than_3(patient_nhpt)) {
        patient_nhpt_less_than_three <- c(patient_nhpt_less_than_three, patient_id)
    }

    i <- i + 1
}
print(paste("number of missing values in disease progression of nhpt", sum(is.na(disease_progression_nhpt))))
disease_progression_nhpt_no_inf <- ifelse(is.infinite(disease_progression_nhpt), NA, disease_progression_nhpt)
print(paste("mean of disease progression of nhpt", mean(disease_progression_nhpt_no_inf, na.rm=TRUE)))
print(paste("number of patients with nhpt disease progression", sum(!is.na(disease_progression_nhpt_no_inf))))
print(paste("number of patients with nhpt unsustained progression", length(unique(patient_nhpt_unsustained_progression))))
print(paste("among those with nhpt disease progression, number with 1 missing value between progression and sustained progression", sum(progress_nhpt_missing_vals_between == 1)))
print(paste("among those with nhpt disease progression, number with 2 missing values between progression and sustained progression", sum(progress_nhpt_missing_vals_between == 2)))
print(paste("among those with nhpt disease progression, number with 3+ missing values between progression and sustained progression", sum(progress_nhpt_missing_vals_between >= 3)))
saveRDS(patient_nhpt_less_than_three, file = "patient_less_than_three_nhpt.RDS")
print("")

# create a dataframe for nhpt progression
nhpt_progression_df <- data.frame(patient_id=observed_patient_ids_nhpt,
                                    nhpt_progression=disease_progression_nhpt)

# merge the three dataframes of EDSS, t25fw, and nhpt disease progression
merged_progression_df <- merge(EDSS_progression_df, t25fw_progression_df, by="patient_id", all=TRUE)
merged_progression_df <- merge(merged_progression_df, nhpt_progression_df, by="patient_id", all=TRUE)

# compute the final progression metric defined as the minimum of the three progression
# metrics; if all three metrics are NA then so is the final progression metric
combined_progression <- c()
for (i in 1:nrow(merged_progression_df)) {
    # get the current patient's row
    cur_patient <- merged_progression_df[i,]

    # create a vector of 3 values with each progression value
    progression_values <- c(cur_patient$EDSS_progression,
                            cur_patient$t25fw_progression,
                            cur_patient$nhpt_progression)

    # if all three values are NA, then we don't know whether this patient progressed
    if (sum(is.na(progression_values)) == 3) {
        combined_progression <- c(combined_progression, NA)
    } else {
        # otherwise their progression value is the min, ignoring missingness
        combined_progression <- c(combined_progression, min(progression_values, na.rm=TRUE))
    }
}

merged_progression_df$combined_progression <- combined_progression
print(paste("number of patients with missing values in combined progression", sum(is.na(combined_progression))))
combined_progression_no_inf <- ifelse(is.infinite(combined_progression), NA, combined_progression)
print(paste("number of patients with combined progression, ignore missing", sum(!is.na(combined_progression_no_inf))))
print(paste("mean of combined progression, ignore missing, ignore no progression", mean(combined_progression_no_inf, na.rm=TRUE)))
