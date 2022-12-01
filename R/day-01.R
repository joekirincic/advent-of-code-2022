
library(tidyverse)

# The following is my original take using {dplyr}.
# (Total lines: 15)
df <- tibble(
  calories = read_lines(file = "data/day-01.txt") %>% parse_double(),
  elf_id = cumsum(is.na(calories))
) %>%
  group_by(elf_id) %>%
  summarize(
    calories = sum(calories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(calories))

answers <- summarize(
  df,
  max_calories = max(calories, na.rm = TRUE),
  sum_top_three_highest_calories = sum(calories[1:3], na.rm = TRUE)
)

# The following riffs off Antoine Fabri's more elegant solution using `tapply`
# (Total lines: 9).
df <- parse_double(read_lines(file = "data/day-01.txt")) %>%
  tapply(X = ., INDEX = cumsum(is.na(.)), FUN = sum, na.rm = TRUE) %>%
  enframe(name = "elf_id", value = "calories") %>%
  arrange(desc(calories))

answers <- summarize(
  df,
  max_calories = max(calories, na.rm = TRUE),
  sum_top_three_highest_calories = sum(calories[1:3], na.rm = TRUE)
)
