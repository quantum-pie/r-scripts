# messing around tidy data (tidyr + dplyr) and pipe operation
library(tidyr)
library(dplyr)
library(stringr)

avian <- read.csv("res/avianHabitat.csv")

res <- 
  avian %>%
    # get rid of numbers at the end of the site names
    mutate(Site = factor(str_replace(Site, "[0-9]+", ""))) %>%
  
    # select Site, Observer variables and variables about plants height (ends with Ht)
    select(Site, Observer, ends_with("Ht")) %>%
  
    # Remove "Ht" from the ends of selected variables
    setNames(str_replace(names(.), "Ht", "")) %>%
  
    # Gather data frame:
    # Collect values of all variables excluding Site and Observer and put them into new variable Height
    # Put all names of variables excluding Site and Observer into new variable Species (factor)
    gather(Species, Height, -Site, -Observer) %>%
  
    # filter rows by condition
    filter(Height > 0) %>%
  
    # group by three variables
    group_by(Site, Observer, Species) %>%
  
    # for all combinations of grouped variables calculate length of corresponding Height vector
    summarise(Measurements = length(Height))
    