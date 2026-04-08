# code for determining and evaluating exam-based relapse recovery

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)

# define a function that fills in the form group when the value is
# interim information
fill_in_formgroup <- function(relapse_data) {
    # get patients with form group "interim information"
    interim_formgroup <- relapse_data %>%
        filter(FormGroup == "Interim Information")

    # read the visit windows data
    visit_windows <- data.frame(read_excel(data_file_name, sheet="visit windows"))

    # process visit windows data
    visit_windows <- visit_windows %>%
        # rename the column name for patient names so that it matches
        # with the relapse data
        rename(PatientName = Patient) %>%
        # get only the patients who have "Interim Information" as their
        # form group data
        filter(PatientName %in% interim_formgroup$PatientName) %>%
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

    # process the interim_formgroup data to get the closest date to the visit date
    # where the exam-confirmed relapse was detected
    interim_formgroup <- merge(interim_formgroup, visit_windows, by="PatientName") %>%
        # make subsequent changes occur row-wise across the whole dataframe
        rowwise() %>%
        # create a new column called closest date as follows
        mutate(closest_date = {
                # get the value of the visit_date
                v <- visit_date
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
        # change the string open or close to "Month " to match with
        # the values of the formgroup
        mutate(closest_date = sub("open", "Month ", closest_date)) %>%
        mutate(closest_date = sub("close", "Month ", closest_date)) %>%
        # keep only the patient name and the closest 6 month interval
        select(c(PatientName, closest_date)) %>%
        # rename the column to closest_formgroup
        rename(closest_formgroup = closest_date)

    # merge the relapse data with the interim formgroup data where the closest 6 month
    # interval was found
    relapse_data <- merge(relapse_data, interim_formgroup, by="PatientName", all.x=TRUE) %>%
        # if a patient has interim information as its form group, change it to the
        # formgroup that we found
        mutate(FormGroup = ifelse(FormGroup == "Interim Information", closest_formgroup, FormGroup)) %>%
        # remove the closest formgroup column since no patient should have an interim formgroup now
        select(-closest_formgroup)

    return(relapse_data)
}

# read the relapse data
relapse_data_orig <- data.frame(read_excel(data_file_name, sheet="relapse"))

# process the relapse data
relapse_data <- relapse_data_orig %>%
    # only consider patients with exam-confirmed relapse recovery
    filter(ra_est_event == "Exam-confirmed exacerbation") %>%
    group_by(PatientName) %>%
    # for now, only use patients that have one relapse to evaluate
    # relapse recovery
    filter(n() == 1) %>%
    # for each patient, create a vector with a string indicating
    # the exam-confirmed relapse symptoms that they are experiencing
    mutate(symptoms = list(as.vector(na.omit(c(ra_deficit_bowel_bladder,
                        ra_deficit_brainstem,
                        ra_deficit_cerebellar,
                        ra_deficit_mental,
                        ra_deficit_pyramidal,
                        ra_deficit_sensory,
                        ra_deficit_visual))))) %>%
    # count the number of symptoms that each patient experience
    mutate(num_symptoms = length(symptoms[[1]])) %>%
    # for now, remove patients that don't have any recorded symptoms
    filter(num_symptoms > 0) %>%
    select(c(PatientName, FormGroup, visit_date, symptoms, num_symptoms))

# replace patients with the formgroup "interim information" with the 6 month
# window corresponding to when they had their relapse
relapse_data <- fill_in_formgroup(relapse_data)

# read the imputed EDSS data
# edss_data <- data.frame(read_excel(data_file_name, sheet="edss"))
edss_data <- readRDS("../primary_analysis/imputed_edss_data.RDS")

# filter the EDSS data to only patients that experience relapse only once
# and have at least one symptom recorded
edss_data <- edss_data %>%
    filter(PatientName %in% relapse_data$PatientName) %>%
    # rename columns so that symptoms match the name of the functional
    # system scores
    rename(Mental = fs_cfss_total,
           Visual = fsvs_on_total,
           Brainstem = fs_bfss_total,
           Pyramidal = total_pyramidal_score,
           Sensory = sensory_system_score_total,
           Cerebellar = cerebellar_system_score_total,
           Bowel_or_bladder = bowel_bladder_sys_score_total,
           FormGroup = month)

print(head(relapse_data))

# define a function that takes as input a PatientName, a FormGroup month
# representing when the relapse occurred, and a list of relevant symptoms
# and returns whether the patient experienced full recovery, partial recovery,
# incomplete recovery, or other
determine_relapse_recovery <- function(patient, month, symptoms) {
    # filter edss data so that it only has the relevant patient
    # and remove columns that won't help us determine relapse recovery
    patient_edss <- edss_data %>%
        filter(PatientName == patient) 

    # replace all spaces with underscores in the symptom names
    # to match with column names in the edss data
    symptoms <- gsub(" ", "_", symptoms[[1]])

    # change the string of the month of relapse to a numeric number
    month <- as.numeric(gsub("\\D", "", month))
    print(month)

    # keep only the columns that are relevant to the symptoms that
    # worsened during relapse
    patient_edss <- patient_edss %>%
        select(c(PatientName, FormGroup), all_of(symptoms)) %>%
        # keep only rows where the recorded data is between 6 months before relapse
        # and one year after relapse
        filter(FormGroup >= month-6) %>%
        filter(FormGroup <= month+12)
    
    print(patient_edss)

    return(0)
}

# print(determine_relapse_recovery(relapse_data$PatientName[1], relapse_data$FormGroup[1], relapse_data$symptoms[1]))

relapse_data <- relapse_data %>%
    rowwise() %>%
    mutate(recovery = determine_relapse_recovery(PatientName, FormGroup, symptoms))

print(nrow(relapse_data))
print(sum(relapse_data$recovery))
