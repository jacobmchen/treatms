# package for real excel files
library(readxl)

source("global_variables.R")

# read the edss data
edss <- data.frame(read_excel(data_file_name, sheet="edss")) %>%
    filter(SiteName == "Providence-0270") %>%
    select(c(SiteName, PatientName, FormGroup, total_edss_score, fs_finding))

edss %>% slice_head(n=10) %>% print()

write.csv(edss, "providence_edss.csv", row.names=FALSE)
