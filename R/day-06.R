
# Day 06

library(tidyverse)

# Solution ----------------------------------------------------------------

# We want a systematic way to produce slices of the original string. We can do
# that by taking the Cartesian product of the string input and a sequence of 
# length nchar(string). This setup allows us to easily nab ordered subsequences 
# of any length we choose.

input <- crossing(
  content = read_file("data/day-06.txt"),
  id = 1:nchar(content)
) %>%
  mutate(
    pos_04 = id + 3,
    pos_14 = id + 13,
    substr_len04 = str_sub(content, start = id, end = pos_04),
    substr_len14 = str_sub(content, start = id, end = pos_14),
    condition_part_one = str_split(substr_len04, pattern = "") %>% map_lgl(~length(unique(.x))==4),
    condition_part_two = str_split(substr_len14, pattern = "") %>% map_lgl(~length(unique(.x))==14),
  )

answers <- summarize(
  input,
  part_one = if_else(condition_part_one, pos_04, NA_real_) %>% min(na.rm = TRUE),
  part_two = if_else(condition_part_two, pos_14, NA_real_) %>% min(na.rm = TRUE)
)
