
library(tidyverse)
library(googlesheets4)

survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1iWhyiShbn_f3R7ZIDTPuTUkrLJIVYEIcJPIyoLbEd1Q/edit?usp=sharing")

survey <- survey %>% 
  select(-Timestamp) %>% 
  rename(first = 'Your first name', 
         last = 'Your last name', 
         days = 'What days would you prefer to work on Stat 331?', 
         times = 'What times of the day do you prefer to work?', 
         comments = 'Do you have any additional considerations you would like for me to know while I am forming groups?') %>% 
  mutate(first = tolower(first), 
         last = tolower(last), 
         monday = str_detect(days, pattern = "Monday"), 
         tuesday = str_detect(days, pattern = "Tuesday"),
         wednesday = str_detect(days, pattern = "Wednesday"),
         thursday = str_detect(days, pattern = "Thursday"),
         friday = str_detect(days, pattern = "Friday"),
         saturday = str_detect(days, pattern = "Saturday"),
         sunday = str_detect(days, pattern = "Sunday")
  ) %>% 
  select(-days) 

course_info <- read_csv("names.csv") %>% 
  mutate(last = word(Student, 1, sep = ","), 
         last = tolower(last), 
         first = word(Student, 2, sep = ","),
         first = tolower(first), 
         section = if_else(str_detect(Section, pattern = "70") == TRUE, 
                           "70", 
                           "71")
         ) %>% 
  select(-Student, -Section)


master <- left_join(course_info, survey, by = c("last")) %>% 
  filter(last != "points possible", 
         last != "student") %>% 
  select(last, first.x, first.y, section, times, 
         monday:sunday)

# SECTION 70

master_70 <- master %>% 
  filter(section == "70", 
         last != "ly", 
         last != "schneider")

master_70 %>% 
  filter(times == "Evenings") %>% 
  View()


# SECTION 71

master_71 <- master %>% 
  filter(section == "71")

master_71 %>% 
  filter(times == "Afternoons") %>% 
  View()
         
