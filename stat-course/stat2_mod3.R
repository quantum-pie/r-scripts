#1
smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data) 
  fit <- hclust(dist_matrix) 
  test_data$cluster <- factor(cutree(fit, cluster_number))
  return(test_data)
}

smart_hclust(swiss, 3)

#2
get_difference<-  function(test_data, n_cluster){
  dist_matrix <- dist(test_data) 
  fit <- hclust(dist_matrix) 
  new_data <- test_data
  new_data$cluster <- factor(cutree(fit, n_cluster))
  p_val <- sapply(test_data, function(x) summary(aov(x ~ cluster, new_data))[[1]][["Pr(>F)"]][[1]])
  names(test_data[p_val < 0.05])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
get_difference(test_data, 2)

#3
get_pc <- function(d){
  pc <- prcomp(d, retx = T)
  cbind(d, pc$x[, 1:2])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc(test_data)

#4
get_pca2 <- function(data){
  pc <- prcomp(data)
  cum_prop <- summary(pc)$importance["Cumulative Proportion", ]
  first_idx <- which(cum_prop > 0.9)[1]
  return(cbind(data, pc$x[, 1:first_idx]))
}

result  <- get_pca2(swiss)
str(result)

#5 Chuck Norris task
check_collinearity <- function(vars){
  coeffs <- lm(vars[, 1] ~ vars[, 2])$coefficients
  isTRUE(all.equal(vars[, 1], coeffs[1] + vars[, 2] * coeffs[2]))
}

is_multicol <- function(d){
  row_combs <- combn(names(d), 2, simplify = F)
  check_result <- sapply(row_combs, function(x) check_collinearity(d[, x]))
  collinear <- row_combs[check_result]
  ifelse(length(collinear) > 0, return(unlist(collinear)), return("There is no collinearity in the data"))
}

test_data <- as.data.frame(list(V1 = c(14, 14, 7, 23, 8), V2 = c(8, -3, 8, 2, 11), V3 = c(-3, 8, -3, 3, -6), V4 = c(19, 19, 12, 28, 13), V5 = c(21, 5, 18, 7, 20)))
is_multicol(test_data)

#6
library(ggplot2)
dist_matrix <- dist(swiss) 
fit <- hclust(dist_matrix) 
swiss$clusters <- factor(cutree(fit, 2))
my_plot <- ggplot(swiss, aes(Education, Catholic, col = clusters))+
  geom_point()+
  geom_smooth(method = "lm")
my_plot
