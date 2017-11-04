#2
data <- read.csv('Projects/R/github/data_cholesterol.csv', sep = ";")
str(data)
data_sub <- subset(data, day == 4 | group == 2)
t.test(cholest ~ factor(group), data_sub)

#4
data <- read.csv('Projects/R/github/contigency_table_analysis_4.csv')
tab <- table(data[, c("A", "B")])
chisq.test(tab)

#6
data <- read.csv('Projects/R/github/hierarchical_clustering_1.csv')
dist_matrix <- dist(data) 
fit <- hclust(dist_matrix, method = "single") 
plot(fit)
test_data$cluster <- factor(cutree(fit, cluster_number))

#8
data <- read.csv('Projects/R/github/world_finance_data.csv')
pc <- prcomp(data[, -1], scale. = T)
summary(pc)
