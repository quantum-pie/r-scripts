#1
sapply(airquality[, c('Ozone', 'Solar.R', 'Wind', 'Temp')], function(x) {
                                  x[is.na(x)] = mean(x, na.rm = T)
                                  sd(x) } )

#2
library(ggplot2)
belgium_jobs <- data.frame(
  industry<-factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                   labels = c("all", "science", "IT & telecom")),
  job_share<-c(2.1, 3.3, 4.5, 2, 2.8, 3.5, 2, 3.4, 3.7, 2.3, 3.2, 5.6, 2.4, 3.7, 4.3, 2.4, 3.8, 4.8, 2.2, 3.9, 4, 2.5, 4.1, 5, 2.5, 3.7, 5, 2.3, 3.3, 4.3, 2.2, 3.5, 4.1, 2.6, 3.5, 5.2, 2.5, 3, 4.7, 2.4, 3.1, 4.6),
  time<-c(2013.25, 2013.25, 2013.25, 2013.5, 2013.5, 2013.5, 2013.75, 2013.75, 2013.75, 2014, 2014, 2014, 2014.25, 2014.25, 2014.25, 2014.5, 2014.5, 2014.5, 2014.75, 2014.75, 2014.75, 2015, 2015, 2015, 2015.25, 2015.25, 2015.25, 2015.5, 2015.5, 2015.5, 2015.75, 2015.75, 2015.75, 2016, 2016, 2016, 2016.25, 2016.25, 2016.25, 2016.5, 2016.5, 2016.5)
)

my_plot <- ggplot(belgium_jobs, aes(time, job_share)) +
  geom_line(aes(col = industry))

#3
is.greater <- function(x, y) {
  ks.test(x, y, alternative = 'greater')$p.value < 0.05
}

set.seed(13)
is.greater(rexp(100), rnorm(100))
is.greater(rnorm(100), rnorm(100))
is.greater(rnorm(100), rnorm(100))

#4
test_selection <- function(x) {
  if(any(x < 5)) {
    "F-test"
  } else {
    "Chi-squared"
  }
}

d <- cbind(c(10, 7), c(9, 11))
test_selection(d)

#5
is.no.effect <- function (formula, data) {
 model <- summary(lm(formula, data))
 fstat <- model$fstatistic 
 as.logical(pf(fstat[1], fstat[2], fstat[3], lower.tail = F) >= 0.05)
}

library(dplyr)
set.seed(13)
is.no.effect(x ~ ., data_frame(
  y = rnorm(100),
  z = rnorm(100),
  x = y + 2 * z + rnorm(100, sd = sd(y + 2 * z))
  ))

set.seed(13)
is.no.effect(x ~ ., data_frame(
  y = rnorm(100),
  z = rnorm(100),
  x = y + 2 * z + rnorm(100, sd = 5 * sd(y + 2 * z))
  ))

#6
apply.transform <- function(data, fun_name) {
  if (fun_name != "identity") {
    fun = match.fun(fun_name)
    data <- as.data.frame(sapply(data, function(x) {
                                                      if(is.numeric(x)) {
                                                        x = fun(x)
                                                      } else {
                                                        x
                                                      }
                                                    }))
  }
  return(data)
}

test.regression <- function(formula, data)
{
  res <- tryCatch(
    {
      fit <- lm(formula, data, na.action=na.exclude)
      fit_sum <- summary(fit)
      fstat <- fit_sum$fstatistic
      is_significant <- as.logical(pf(fstat[1], fstat[2], fstat[3], lower.tail = F) < 0.05)
      res_are_normal = shapiro.test(fit$residuals)$p.value >= 0.05  
      return(is_significant & res_are_normal)
    },
    error=function(cond) {
      return(F)
    }
  )
}

choose.transform <- function(formula, data) {
  transforms = c("identity", "scale", "log", "exp")
  transformed_data_list <- lapply(transforms, function(x) apply.transform(data, x))
  res <- sapply(transformed_data_list, function(x) test.regression(formula, x))
  good_trans <- transforms[res]
  if(length(good_trans) > 0) {
    good_trans[1]
  } else {
    NA
  }
}

df <- data_frame(x = exp(rnorm(100)), y = exp(2 * log(x) + 0.5 + rnorm(100)))
choose.transform(y ~ x, df)

#7
library(dplyr)
library(tidyr)

na.approximate <- function(data) {
  na_idx <- which(is.na(data))
  good_idx <- which(!is.na(data))
  res <- data
  for(i in na_idx) {
    left_slice <- good_idx[good_idx < i]
    right_slice <- good_idx[good_idx > i]
    
    if(length(left_slice) == 0) {
      idx1 <- right_slice[1]
      idx2 <- right_slice[2]
    } else if(length(right_slice) == 0) {
      slice_len <- length(left_slice)
      idx2 <- left_slice[slice_len - 1]
      idx1 <- left_slice[slice_len]
    }
    else {
      idx1 <- left_slice[length(left_slice)]
      idx2 <- right_slice[1]
    }
    
    delta_x0 <- idx1 - idx2
    delta_y0 <- data[idx1] - data[idx2]
    delta_x1 <- i - idx1
    res[i] <- data[idx1] + delta_y0 / delta_x0 * delta_x1
  }
  return(res)
}

normalize_data <- function (data) {
  data %>%
    gather(year, milksolids, -region) %>%
    mutate(year = strtoi(substring(year, 2))) %>%
    arrange(year) %>%
    group_by(region) %>%
    mutate(milksolids = na.approximate(milksolids), year = ordered(year)) %>%
    as.data.frame() %>%
    return()
}

raw <- read.csv("https://stepik.org/media/attachments/lesson/31921/nz_milk_sample.csv")
res <- normalize_data(raw)
str(res)

#8
library(ggplot2)
library(Hmisc)

universities <- read.csv('https://stepik.org/media/attachments/lesson/31919/universities.csv')
my_plot <-  ggplot(universities, aes(year, students, col = university, group = university)) +
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2), geom = "errorbar", width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.2)) + 
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.2)) +
  facet_wrap(~ university)

#9
company <- read.csv('https://stepik.org/media/attachments/lesson/33775/company.csv')
my_plot <- ggplot(company, aes(department, revenue, col = factor(year), group = factor(year))) +
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2)) + 
  scale_color_discrete(name = "Год", labels = c("2014", "2015")) + 
  scale_x_discrete(name = "Филиал", labels = paste0("Филиал №", 1:4)) + 
  scale_y_continuous(name = "Прибыль", breaks = seq(1490, 1520, 3))
  
