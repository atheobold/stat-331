
library(googlesheets4)

midterms <- read_sheet("https://docs.google.com/spreadsheets/d/1cAZLJJ93opbQZCPi7OZtY0DuMAeCD0i6Rxuj8AIfNhQ/edit?usp=sharing")


midterms <- midterms %>% 
  rename(name = "Your Name", 
         section = `What time do you attend STAT 331 / 531?`, 
         wednesday_exams = `Wednesday Exam Times`, 
         thursday_exams = `Thursday Exam Times...5`, 
         friday_exams = `Friday Exam Times...6`) %>% 
  select(- contains("Timeslots"), 
         - contains("Exam Times"))

############################## WEDNESDAY ####################################

wed_exams <- midterms %>% 
  filter(!is.na(wednesday_exams)) %>% 
  select(name, 
         section, 
         wednesday_exams) %>%
  rename(time = wednesday_exams) %>% 
  mutate(first_name = word(name, start = 1, end = 1), 
         last_name = if_else(str_count(name, pattern = "\\s") == 2,  
                              word(name, start = 2, end = 3), 
                              word(name, start = 2)
                              ), 
         morning = if_else(str_detect(time, pattern = "am"), 
                           time, 
                           NA_character_), 
         afternoon = if_else(str_detect(time, pattern = "pm"), 
                             time, 
                             NA_character_)
         ) %>% 
  select(-name, -time) %>%
  pivot_longer(morning:afternoon, names_to = "time_of_day", 
               values_to = "time") %>% 
  mutate(time = str_extract_all(time, pattern = ".*[^(am|pm)]"), 
         time = unlist(time)
         )

wed_morning <- wed_exams %>% 
  filter(time_of_day == "morning",
         !is.na(time)) %>%
  arrange(desc(time))

wed_afternoon <- wed_exams %>% 
  filter(time_of_day == "afternoon",
         !is.na(time)) %>% 
  arrange(time)


############################## THURSDAY ####################################

thurs_exams <- midterms %>% 
  filter(!is.na(thursday_exams)) %>% 
  select(name, 
         section, 
         thursday_exams) %>%
  rename(time = thursday_exams) %>% 
  mutate(first_name = word(name, start = 1, end = 1), 
         last_name = if_else(str_count(name, pattern = "\\s") == 2,  
                             word(name, start = 2, end = 3), 
                             word(name, start = 2)
         ), 
         morning = if_else(str_detect(time, pattern = "am"), 
                           time, 
                           NA_character_), 
         afternoon = if_else(str_detect(time, pattern = "pm"), 
                             time, 
                             NA_character_)
  ) %>% 
  select(-name, -time) %>%
  pivot_longer(morning:afternoon, names_to = "time_of_day", 
               values_to = "time") %>% 
  mutate(time = str_extract_all(time, pattern = ".*[^(am|pm)]"), 
         time = unlist(time)
  )

thurs_morning <- thurs_exams %>% 
  filter(time_of_day == "morning",
         !is.na(time)) %>%
  arrange(desc(time))

thurs_afternoon <- thurs_exams %>% 
  filter(time_of_day == "afternoon",
         !is.na(time)) %>% 
  arrange(time)

############################## FRIDAY ####################################

fri_exams <- midterms %>% 
  filter(!is.na(friday_exams)) %>% 
  select(name, 
         section, 
         friday_exams) %>%
  rename(time = friday_exams) %>% 
  mutate(first_name = word(name, start = 1, end = 1), 
         last_name = if_else(str_count(name, pattern = "\\s") == 2,  
                             word(name, start = 2, end = 3), 
                             word(name, start = 2)
         ), 
         morning = if_else(str_detect(time, pattern = "am"), 
                           time, 
                           NA_character_), 
         afternoon = if_else(str_detect(time, pattern = "pm"), 
                             time, 
                             NA_character_)
  ) %>% 
  select(-name, -time) %>%
  pivot_longer(morning:afternoon, names_to = "time_of_day", 
               values_to = "time") %>% 
  mutate(time = str_extract_all(time, pattern = ".*[^(am|pm)]"), 
         time = unlist(time)
  )

fri_morning <- wed_exams %>% 
  filter(time_of_day == "morning",
         !is.na(time)) %>%
  arrange(desc(time))

fri_afternoon <- wed_exams %>% 
  filter(time_of_day == "afternoon",
         !is.na(time)) %>% 
  arrange(time)


