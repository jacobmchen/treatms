# Code that plots results from the simulation study on 
# the variance of the RMST estimator at different restriction
# time windows.

# import package for tibble manipulations
library(tidyverse)

# read the data
data <- readRDS("simulation_results.RDS")

print(data)

png("variance_sim.png")
plot(x=data$restriction_time, y=data$variance, type="b",
     main="Estimated Variance at Different Time Restrictions",
     xlab="Restriction Time (Months)",
     ylab="Estimated Variance",
     xaxt = "n")
axis(1, at=data$restriction_time)

dev.off()
