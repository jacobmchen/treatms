# code for evaluating the extent of exceptions in edss data

# read the global variables
source("../primary_analysis/global_variables.R")

# package for real excel files
library(readxl)
# library(lubridate)

# read the edss data
edss <- data.frame(read_excel(data_file_name, sheet="edss"))

edss <- edss %>%
    filter(!is.na(total_edss_score)) %>%
    filter(!is.na(fs_finding))

print(head(edss))
print(nrow(edss))
print(sum(edss$fs_finding == "Yes"))

edss <- edss %>%
    group_by(PatientName) %>%
    summarise(
        n_rows = n(),
        n_exception = sum(fs_finding == "Yes", na.rm=TRUE)
    ) %>%
    mutate(percent_fs = n_exception/n_rows)

print(edss)
print(nrow(edss))
print(mean(edss$percent_fs))

print(edss %>% filter(percent_fs > 0.8))
print(nrow(edss %>% filter(percent_fs == 0)))
print(nrow(edss %>% filter(percent_fs <= 0.2)))

png(filename="percent_exception.png", width=800, height=600, res=100)

hist(edss$percent_fs, main="Histogram of Percentage of Exceptions for Each Patient", ylab="Frequency", xlab="Number of Patients", ylim=c(0, 900))

dev.off()
