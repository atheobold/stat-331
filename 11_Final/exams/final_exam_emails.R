
library(tidyverse)
library(readxl)


exams <- read_xlsx(here::here("11_Final", "331_oral_exams.xlsx"))

wednesday <- exams %>% 
  filter(`Oral Final` == "W") %>% 
  pull(`Student Username`) %>% 
  str_c("@calpoly.edu") %>% 
  as.data.frame()


thursday <- exams %>% 
  filter(`Oral Final` == "R") %>% 
  pull(`Student Username`) %>% 
  str_c("@calpoly.edu") %>% 
  as.data.frame()


friday <- exams %>% 
  filter(`Oral Final` == "F") %>% 
  pull(`Student Username`) %>% 
  str_c("@calpoly.edu") %>% 
  as.data.frame()


#######

emails <- function(df, day){
  email <- df %>% 
    filter(`Oral Final` == day) %>%
    pull(`Student Username`) %>% 
    str_c("@calpoly.edu")
}

days <- c("W", "R", "F") 

map(days, ~emails(exams, .x))
                                      