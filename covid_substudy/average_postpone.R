# import the global variables file
source("global_variables.R")

postpone_count <- readRDS("postpone_count.RDS")

print("average number of postponed or cancelled visits")
print(mean(postpone_count$num_postpone))
