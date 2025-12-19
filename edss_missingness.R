library(readxl)
library(stringr)

source("functions.R")

# read the data
baseline_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="baseline chars"))

# get EDSS data
edss_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="edss"))

# get the patient IDs and print the number of different patients
# note that we have also verified that the patient IDs in the EDSS data and the
# MSFC data are exactly the same
patient_ids <- unique(edss_data$PatientName)
print("number of unique patient_ids in EDSS data")
print(length(patient_ids))

# convert the FormGroup column to numbers
# the FormGroup column tells us the month at which the visit was conducted (0, 6, 12, etc.)
FormGroup_num <- as.integer(word(edss_data$FormGroup, 2))
edss_data$FormGroup_num <- FormGroup_num

# keep track of which patients have no baseline value
patient_ids_no_baseline <- c()

# note that we verified that every patient has a month zero in formgroup_num
# get patients who have no baseline value for EDSS
for (patient_id in patient_ids) {
    # get EDSS data for this patient
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num   

    if (is.na(cur_patient_edss[1])) {
        patient_ids_no_baseline <- c(patient_ids_no_baseline, patient_id)
    }
}
print("number of patients with no baseline EDSS value")
print(length(patient_ids_no_baseline))

# get patient ids that have at least a baseline value
patient_ids_with_baseline <- setdiff(patient_ids, patient_ids_no_baseline)

# keep track of which patients have no follow-ups after the baseline value
patient_ids_no_followup <- c()

# now get the patients who have at least one follow-up, regardless of when
# the follow-up was
for (patient_id in patient_ids_with_baseline) {
    # get EDSS data for this patient
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num   

    # check whether the patient has at least one follow-up value
    if (sum(!is.na(cur_patient_edss)) < 2) {
        patient_ids_no_followup <- c(patient_ids_no_followup, patient_id)
    }
}

print("number of patients with no follow-up EDSS measurements")
print(length(patient_ids_no_followup))

# get patient ids that have a baseline value and at least one follow-up
patient_ids_with_baseline_followup <- setdiff(patient_ids_with_baseline, patient_ids_no_followup)

# keep track of patients with at least two follow-up visits
patient_ids_with_followups <- c()

# now get the patients who have at least two follow-up visits, regardless
# of when the follow-ups were
for (patient_id in patient_ids_with_baseline_followup) {
    # get EDSS data for this patient
    cur_patient_data <- edss_data[edss_data$PatientName == patient_id, ]
    cur_patient_edss <- cur_patient_data$total_edss_score
    cur_patient_formgroup_num <- cur_patient_data$FormGroup_num   

    # check whether the patient has at least two follow-up values,
    # which is equivalent to checking that they have three or more
    # observed values
    if (sum(!is.na(cur_patient_edss)) > 2) {
        patient_ids_with_followups <- c(patient_ids_with_followups, patient_id)
    }
}

print("number of patients with at least two follow-up EDSS measurements")
print(length(patient_ids_with_followups))
