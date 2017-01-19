# 1
smart_test <-  function(x){
  tab <- table(x)
  if(all(tab >= 5)) {
    tst <- chisq.test(tab)
    c(tst$statistic, tst$parameter, tst$p.value)
  } else
  {
    fisher.test(tab)$p.value
  }
  
}

smart_test(mtcars[1:20,c("am", "vs")])

# 2
most_significant <-  function(x){
  p_vec <- apply(x, 2, function(y) chisq.test(table(y))$p.value)
  names(x)[which(p_vec == min(p_vec))]
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant(test_data)

# 3
str(iris)
iris_wo_sp <- subset(iris, select = -Species)
means <- colMeans(iris_wo_sp)
iris$important_cases <- factor(apply(iris_wo_sp, 1, function(x) ifelse(sum(x > means) >=3, "Yes", "No")))
table(iris$important_cases)

# 4
get_important_cases <- function(x){
 thr <- (ncol(x) %/% 2)
 means <- colMeans(x)
 x$important_cases <- factor(apply(x, 1, function(y) ifelse(sum(y > means) > thr, 1, 0)), levels = c(0, 1), labels = c("No", "Yes"))
 return(x)
}

test_data <- data.frame(rbind(c(16, 17, 25, 20, 16), c(21, 7, 23, 22, 17), c(18, 16, 27, 18, 19)))
get_important_cases(test_data)

#5
stat_mode <- function(x){
  uni_x <- unique(x)
  freqs <- sapply(uni_x, function(y) sum(y == x))
  uni_x[which(freqs == max(freqs))]
}

v <- c(1, 1, 1, 2, 3, 3, 3)
stat_mode(v)
  
#6
max_resid <- function(x){
  t <- table(x)
  fit <- chisq.test(t) 
  idx <- which(fit$residuals == max(fit$residuals), arr.ind = T)
  c(rownames(t)[idx[1]], colnames(t)[idx[2]])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
max_resid(test_data)

#7
library(ggplot2)
obj <- ggplot(diamonds, aes(color))+
  geom_bar(aes(fill = cut), position=position_dodge())
obj
