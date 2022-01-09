
library(tidyverse)
library(googlesheets4)

survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1R8-bjAfbKcItCDQt5XRW63JwGr-33iWjdsQRRzIlZDs/edit?usp=sharing")

survey <- survey %>% 
  select(-Timestamp) %>% 
  rename(last = 'Your last name (as seen in Canvas)', 
         first = 'Your preferred first name', 
         look_for = "What do you look for in group members? (select all that apply)",
         look_for_other = 'If you selected \"Other\" above, please elaborate on your selection!', 
         gender = "How would you describe your gender identity? (select all that apply)", 
         gender_other = 'If you selected \"Prefer to self describe\" above, please elaborate on your selection!...7',
         hispanic = "Do you identify as Hispanic / Latinx?", 
         race = "Which of the following racial groups do you identify with? (select all that apply)", 
         race_other = 'If you selected \"Prefer to self describe\" above, please elaborate on your selection!...10', 
         programming = "How would you describe your programming background? (select all that apply)", 
         days = 'What days would you prefer to work on Stat 331? (select all that apply)',
         times = 'What times of the day do you prefer to work? (select all that apply)', 
         comments = 'Do you have any additional considerations you would like for me to know while I am forming teams?') %>% 
  mutate(last = tolower(last), 
         monday = str_detect(days, pattern = "Monday"), 
         tuesday = str_detect(days, pattern = "Tuesday"),
         wednesday = str_detect(days, pattern = "Wednesday"),
         thursday = str_detect(days, pattern = "Thursday"),
         friday = str_detect(days, pattern = "Friday"),
         saturday = str_detect(days, pattern = "Saturday"),
         sunday = str_detect(days, pattern = "Sunday")
  ) %>% 
  select(-days) 

course_info <- read_csv(here::here("syllabus", "team-based tools", 
                                   "course_names.csv")
                        ) %>% 
  mutate(last = word(Student, 1, sep = ","), 
         last = tolower(last),  
         section = case_when(str_detect(Section, pattern = "70") == TRUE ~ "70",
                             str_detect(Section, pattern = "71") == TRUE ~ "71", 
                             str_detect(Section, pattern = "72") == TRUE ~ "72")
         ) %>% 
  select(-Student, -Section)


master <- left_join(course_info, survey, by = c("last")) %>% 
  filter(last != "student") %>% 
  select(last, first, section, times, monday:saturday, look_for, 
         programming, comments)

# SECTION 70

master_70 <- master %>% 
  filter(section == "70")

master_70 %>% 
  filter(times == "Evenings") %>% 
  View()


# SECTION 71

master_71 <- master %>% 
  filter(section == "71")

master_71 %>% 
  filter(times == "Afternoons") %>% 
  View()


# SECTION 72

master_71 <- master %>% 
  filter(section == "72")

master_71 %>% 
  filter(times == "Afternoons") %>% 
  View()

