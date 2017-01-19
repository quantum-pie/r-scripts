setwd("C:/Users/Ermakov_P/Documents/R/")

library(stringr)

data <- read.csv("res/avianHabitat.csv")

# get names of columns that are about coverage (starts with "P")
coverage_values <- names(data)[str_detect(names(data), "^P")]

# make new variable that is sum of coverage values along each row
data$total_coverage <- rowSums(data[, coverage_values])

# get rid of numeric endings of sites identificators and write new sites variable as factor
data$site_name <- factor(str_replace(data$Site, "[0-9]+", ""))

# calculate site that corresponds to minimal mean total coverage (mean over all observations on this site))
# first, apply mean function to total_coverage variable, grouping by site. Result is array of named minimums
# then find minimal value in the result
min_mean_coverage_by_sites <- which.min(tapply(data$total_coverage, data$site_name, mean))

# get names of columns that are about plants height (ends with "Ht")
height_cols <- names(data)[str_detect(names(data), "Ht$")]

# apply max func to every height column in data, grouping by Observer factor
# result is matrix with Observer in rows and height values in columns
res <- sapply(data[, height_cols], function(x) tapply(x, data$Observer, max))

# first, find row indexes for each column of matrix, that corresponds to maximums over that columns
# then get names of that rows by indexes
# finally merge column names with row names by rows to get champions table
nice_output <- rbind(colnames(res), rownames(res)[apply(res, 2, which.max)])


