library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

data <- read.csv("r-github/vgsales.csv")

# overall look
overall_data <- 
  data %>%
  mutate(Year = strtoi(Year)) %>%
  select(Year, Global_Sales, Platform) %>%
  filter(Platform %in% c("PS", "PS2", "PS3", "PS4", "XB", "X360", "XOne"), !is.na(Year)) %>%
  group_by(Year, Platform) %>%
  summarize(Total_Sales = sum(Global_Sales)) 

# plot
ggplot(overall_data, aes(Year, Total_Sales))+
  ggtitle("Annual Video Games Sales Timeline")+
  geom_col(aes(fill = Platform))+
  scale_x_continuous(breaks = seq(1990, 2017, 2))+
  scale_y_continuous(breaks = seq(0, 350, 50), name = "Annual Sales, millions of copies")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# timeline of percentage
timeline_data <- 
  overall_data %>%
  mutate(Platform = fct_collapse(Platform, 
                                 PS = c("PS", "PS2", "PS3", "PS4"), 
                                 XBOX = c("XB", "X360", "XOne"))
                                 ) %>%
  group_by(Year, Platform) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  mutate(Portion = 100 * Total_Sales / sum(Total_Sales)) %>%
  filter(Platform == "PS" & Total_Sales != sum(Total_Sales)) 

# plot
ggplot(timeline_data, aes(Year, Portion))+
  geom_smooth(method = "loess", se = F, color = "#ffa700", size = 1.4)+
  geom_point(color = "#0057e7", size = 2)+
  scale_x_continuous(breaks = seq(2000, 2017, 2))+
  scale_y_continuous(breaks = seq(40, 100, 10), name = "PS Market Share, %")+
  ggtitle("PS against XBOX market share timeline")+
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5))
  
# contingency table
console_gen_contingency <- 
  overall_data %>%
  ungroup() %>%
  filter(Platform != "PS") %>%
  select(Total_Sales, Platform) %>%
  mutate(Generation = droplevels(
                        fct_collapse(Platform, 
                                     First = c("PS2", "XB"), 
                                     Second = c("PS3", "X360"), 
                                     Third = c("PS4", "XOne"))
                                )) %>%
  mutate(Platform = droplevels(
                      fct_collapse(Platform, 
                                   PS = c("PS2", "PS3", "PS4"), 
                                   XBOX = c("XB", "X360", "XOne"))
                              )) %>%
  group_by(Platform, Generation) %>%
  summarise(Total_Sales = sum(Total_Sales)) 

console_gen_table <- xtabs(Total_Sales ~ Generation + Platform, console_gen_contingency)

# plot
mosaicplot(console_gen_table, 
           color = c("#c95719", "#128a94"), 
           main = "Console-Gen Video Games Sales Table",
           cex.axis = 1.1)







