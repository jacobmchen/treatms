library(readxl)
library(stringr)

# read the data
baseline_data <- data.frame(read_excel("preliminary_longitudinal_data.xlsx", sheet="baseline chars"))

# read relevant vectors
patient_less_than_three_edss <- readRDS("patient_less_than_three_edss.RDS")
patient_less_than_three_t25fw <- readRDS("patient_less_than_three_t25fw.RDS")
patient_less_than_three_nhpt <- readRDS("patient_less_than_three_nhpt.RDS")

data_edss <- baseline_data[baseline_data$PatientName %in% patient_less_than_three_edss,]
print(paste("EDSS less than 3 observed randomization to A percentage", sum(data_edss$treatment_group == "A")/nrow(data_edss)))

data_t25fw <- baseline_data[baseline_data$PatientName %in% patient_less_than_three_t25fw,]
print(paste("t25fw less than 3 observed randomization to A percentage", sum(data_t25fw$treatment_group == "A")/nrow(data_t25fw)))

data_nhpt <- baseline_data[baseline_data$PatientName %in% patient_less_than_three_nhpt,]
print(paste("nhpt less than 3 observed randomization to A percentage", sum(data_nhpt$treatment_group == "A")/nrow(data_nhpt)))

sites <- unique(baseline_data$SiteName)

for (site in sites) {
    cur_data <- baseline_data[baseline_data$SiteName == site,]
    treatment_A <- sum(cur_data$treatment_group == "A")/nrow(cur_data)
    if (treatment_A >= 0.6 | treatment_A <= 0.4) {
        print(site)
        print(treatment_A)
        print("")
    }
}
