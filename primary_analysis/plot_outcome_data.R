# file for plotting outcome data to check for anomalies

# package for real excel files
library(readxl)
# package for operations on manipulating data
library(tidyverse)
# library for imputing missing values
library(mice)
# library for plotting graphs
library(ggplot2)

source("global_variables.R")

edss_pdds_data <- readRDS("annotated_imputed_edss_pdds_data.RDS") %>%
    select(c(PatientName, month, total_edss_score, pdds_total_score,
             edss_missing, pdds_missing)) %>%
    mutate(edss_missing=ifelse(edss_missing == 1, "Imputed", "Observed")) %>%
    # subset to the first 50 patients
    filter(PatientName %in% unique(PatientName)[1:10])

edss_pdds_data %>% slice_head(n=10) %>% print()

p <- ggplot(edss_pdds_data, aes(x = month, y = total_edss_score, group = PatientName, color = PatientName)) +
  geom_line() +
  geom_point(aes(shape = edss_missing), size = 2) +
  scale_shape_manual(values = c("Observed" = 16, "Imputed" = 17)) +
  theme_minimal()

ggsave("patients_1-10.pdf", plot = p, width = 8, height = 6)

