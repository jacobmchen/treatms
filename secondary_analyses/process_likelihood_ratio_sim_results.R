library(readr)

# define number of output files
num_files <- 2

# keep track of the number of significant tests
# for each type of test
chi_square_sig <- 0
bootstrap_sig <- 0

for (i in 0:(num_files-1)) {
	# create the string for the file name
	file_name <- paste0("result_likelihood_ratio-", i, ".log")

	# read the text from the file
	lines <- readLines(file_name)
	# remove the leading [1] from each line, and convert
	# the text to numbers
	vals <- as.numeric(sub("^\\[1\\]\\s*", "", lines))

	# read the results of the chi square test and bootstrap test
	chi_square_test <- vals[1]
	bootstrap_test <- vals[2]

	# evaluate if the tests are significant, and add to a counter
	if (chi_square_test < 0.05) chi_square_sig <- chi_square_sig + 1
	if (bootstrap_test < 0.05) bootstrap_sig <- bootstrap_sig + 1
}

# output results
print("total number of files read")
print(num_files)

print("number of significant chi square tests")
print(chi_square_sig)

print("number of significant bootstrap tests")
print(bootstrap_sig)

