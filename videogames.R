library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

data <- read.csv("~/Projects/R/github/vgsales.csv")

#1
overall_data <- 
  data %>%
  mutate(Year = strtoi(Year)) %>%
  select(Year, Global_Sales, Platform) %>%
  filter(Platform %in% c("PS", "PS2", "PS3", "PS4", "XB", "X360", "XOne"), !is.na(Year)) %>%
  group_by(Year, Platform) %>%
  summarize(Total_Sales = sum(Global_Sales)) 

# plot
ggplot(overall_data, aes(Year, Total_Sales)) +
  geom_col(aes(fill = Platform)) 

#2
timeline_data <- 
  overall_data %>%
  mutate(Platform = fct_collapse(Platform, PS = c("PS", "PS2", "PS3", "PS4"), XBOX = c("XB", "X360", "XOne"))) %>%
  group_by(Year, Platform) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  mutate(Portion = 100 * Total_Sales / sum(Total_Sales)) %>%
  filter(Platform == "PS" & Total_Sales != sum(Total_Sales)) 

#3
console_generation_contingency_table <- 
  overall_data %>%
  ungroup() %>%
  filter(Platform %in% c("PS3", "X360", "PS2", "XB")) %>%
  select(Total_Sales, Platform) %>%
  mutate(Generation = fct_collapse(Platform, First = c("PS2", "XB"), Second = c("PS3", "X360"))) %>%
  mutate(Platform = fct_collapse(Platform, PS = c("PS2", "PS3"), XBOX = c("XB", "X360"))) %>%
  group_by(Platform, Generation) %>%
  summarise(Total_Sales = sum(Total_Sales)) 

fit <- chisq.test(matrix(console_generation_contingency_table$Total_Sales * 1e6, nrow = 2))
mosaicplot(matrix(console_generation_contingency_table$Total_Sales, nrow = 2), color = T, shade = T)
fit <- lm(Total_Sales ~ Platform + Generation, console_generation_contingency_table)

ggplot(timeline_data, aes(Year, Portion))+
  geom_point()+
  geom_line()





