library(tidyverse)

# create a blank dataframe with the appropriate column names
consort <- tibble(
    # record the id of the patient that was actually dropped
    patient_dropped = character(),
    # record the reason for dropping this particular patient
    reason = character(),
    # record the file in which this patient was dropped from
    # the analysis
    file_name = character()
)

# example code for how to add a new row to a tibble dataframe
# consort <- consort %>% add_row(patient_dropped="test", reason="test2", file_name="test3")

print(consort)

# save the empty consort file
saveRDS(consort, "consort.RDS")
# save another version of the empty consort file for
# the version of the anlaysis that does not use measurements
# with exception findings
saveRDS(consort, "consort_no_exceptions.RDS")
