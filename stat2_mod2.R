#1
get_coefficients <- function(dataset){
  exp(glm(y ~ x, dataset, family = "binomial")$coefficients)
}

#2
centered <- function(test_data, var_names){
  test_data[, var_names] <- scale(test_data[, var_names], scale = F)
  return(test_data)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")
df <- centered(test_data, var_names)

#3
get_features <- function(dataset){
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  res <- anova(fit, test = "Chisq")
  significant_vars <- names(dataset)[res$`Pr(>Chi)` < 0.05][-1]
  ifelse(length(significant_vars) > 0, return(significant_vars), return("Prediction makes no sense"))
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
get_features(test_data)

#4
most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
  predicted_data <- predict(object = fit, newdata = data_for_predict)
  data_for_predict$passangers[which(predicted_data == max(predicted_data))]
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
gg <- most_suspicious(test_data, data_for_predict)

#5
normality_test <- function(dataset){
  sapply(dataset[, sapply(dataset, is.numeric)], function(x) shapiro.test(x)$p.value)
}

normality_test(iris)

#6
smart_anova <- function(test_data){
  shapiro_p <- aggregate(x ~ y, data = test_data, function(x) shapiro.test(x)$p.value)$x
  p_value <- bartlett.test(x ~ y, test_data)$p.value
  
  if(sum(c(shapiro_p, p_value) < 0.05) > 0) {
    p_value <- kruskal.test(x ~ y, test_data)$p.value
    names(p_value) <- "KW"
  } else {
    fit <- aov(x ~ y, test_data)
    p_value <- summary(fit)[[1]]$'Pr(>F)'[1]
    names(p_value) <- "ANOVA"
  }
  return(p_value)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
smart_anova(test_data)

#7
normality_by <- function(test){
  df <- aggregate(x = test[1], by = test[2:3], function(x) shapiro.test(x)$p.value)
  names(df)[3] = "p_value"
  return(df)
}

normality_by(mtcars[, c("mpg", "am", "vs")])

#8
library(ggplot2)
obj <- ggplot(iris, aes(x = Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2)
obj
