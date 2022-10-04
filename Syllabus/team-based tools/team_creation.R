
library(tidyverse)
library(googlesheets4)

survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aF658zCvZ4jROYtXqIdZ4a1CrJS4FkG72fi6JlOlBOM/edit?resourcekey#gid=120982239")

survey <- survey %>% 
  select(-Timestamp) %>% 
  rename(last = 'Your last name (as seen in Canvas)', 
         first = 'Your preferred first name', 
         look_for = "What do you look for in group members? (select all that apply)",
         look_for_other = 'If you selected \"Other\" above, please elaborate on your selection!', 
         gender = "How would you describe your gender identity? (select all that apply)", 
         gender_other = 'If you selected \"Prefer to self describe\" above, please elaborate on your selection!...7',
         hispanic = "Do you identify as Hispanic or Latinx?", 
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
                                   "331_531_f22.csv"), 
                        skip = 1
                        ) %>% 
  rename(name = 'Points Possible', 
         section = `...5`) |> 
  select(name, section) |> 
  mutate(last = word(name, 1, sep = ","), 
         last = tolower(last),  
         section = case_when(str_detect(section, pattern = "331") == TRUE ~ "331",
                             str_detect(section, pattern = "531") == TRUE ~ "531", 
                              #str_detect(section, pattern = "72") == TRUE ~ "72"
                             )
         ) 


master <- left_join(course_info, survey, by = c("last")) |> 
  filter(last != "student") %>% 
  select(last, first, section, times, monday:saturday, look_for,
         gender, programming, comments)

## Morning Preference
master |>  
  filter(times %in% c("Mornings, Evenings", "Mornings, Afternoons", "Mornings"),
         gender != "Woman / Female / Femme", 
         monday, 
         wednesday) |> 
  view()

## Afternoon Preference

master %>% 
  filter(times %in% c("Afternoons, Evenings", "Afternoons", 
                      "Mornings, Afternoons"), 
         gender != "Woman / Female / Femme") |> 
  View()

## Evening Preference

master |>  
  filter(times %in% c("Mornings, Evenings", "Afternoons, Evenings",
                      "Mornings, Afternoons, Evenings", "Evenings"),
         gender != "Woman / Female / Femme") |>
  View()

