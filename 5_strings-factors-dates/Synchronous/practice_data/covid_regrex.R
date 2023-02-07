library(tidyverse)

messy_data <- read_csv(here::here("01-data",
                                  "data_2023-Jan-26.csv"))

clean_data <- messy_data |>
  separate_longer_delim(variants, delim = ",") |> #requires tidyr 1.3.0
  separate_wider_delim(col = variants, delim = ":", names = c("variable", "value")) |>
  mutate(across(variable:value, ~ str_trim(.x)),
         variable = str_remove(variable, "\\["),
         variable = str_remove(variable, "\\{"),
         across(variable:value, ~ str_remove(.x, "\\'")),
         across(variable:value, ~ str_remove(.x, "\'")),
         value = str_remove(value, "\\}"),
         value = str_remove(value, "\\]"),
  ) |>
  pivot_wider(id_cols = areaType:date,
              names_from = variable,
              values_from = value,
              values_fn = list
  ) |>
  unnest(cols = c(variant, cumWeeklySequenced, newWeeklyPercentage)) |>
  mutate(across(cumWeeklySequenced:newWeeklyPercentage, ~ as.numeric(.x))) |>
  arrange(date)

clean_data