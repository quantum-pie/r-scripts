library(corrplot)
library(ggplot2)
library(dplyr)
library(forcats)

data <- read.csv("r-github/vgsales.csv")

proc <- 
  data %>%
  select(Year, Global_Sales) %>%
  group_by(Year) %>%
  summarize(Total_Sales = sum(Global_Sales))

proc2 <- 
  data %>%
  mutate(Year = strtoi(Year)) %>%
  select(Year, Global_Sales, Platform) %>%
  filter(Platform %in% c("PS", "PS2", "PS3", "PS4", "XB", "X360", "XOne"), Year != "N/A") %>%
  group_by(Year, Platform) %>%
  summarize(Total_Sales = sum(Global_Sales)) 

ggplot(proc2, aes(Year, Total_Sales)) +
  geom_col(aes(fill = Platform)) 

proc3 <- 
  proc2 %>%
  mutate(Platform = fct_collapse(Platform, PS = c("PS", "PS2", "PS3", "PS4"), XBOX = c("XB", "X360", "XOne"))) %>%
  group_by(Year, Platform) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  filter(Total_Sales != sum(Total_Sales)) %>%
  mutate(Portion = 100 * Total_Sales / sum(Total_Sales)) %>%
  filter(Platform == "PS") 

ggplot(proc3, aes(Year, Portion))+
  geom_point()+
  geom_line()
