library(tidyverse)

messy_data <- read_csv(here::here("5_strings-factors-dates",
                                  "Synchronous", 
                                  "practice_data",
                                  "data_2023-Jan-26.csv"))

messy_data |>
  separate_longer_delim(variants, 
                        delim = ",") |> 
  separate_wider_delim(col = variants, 
                       delim = ":", 
                       names = c("variable", "value")
  ) |>
  mutate(across(.cols = variable:value, 
                .fns = ~ str_trim(.x)
  ),
  across(.cols = variable:value, 
         .fns = ~str_remove(.x, pattern = "^[:punct:]{1,3}")
         ), 
  across(.cols = variable:value, 
         .fns = ~str_remove(.x, pattern = "[\\'\\}\\]]$")
         )
  ) |>
  pivot_wider(id_cols = areaType:date,
              names_from = variable,
              values_from = value,
              values_fn = list
  ) |>
  unnest(cols = c(variant, 
                  cumWeeklySequenced, 
                  newWeeklyPercentage)
  ) |>
  mutate(across(cumWeeklySequenced:newWeeklyPercentage, 
                ~as.numeric(.x)
  )
  ) |>
  arrange(desc(date))