
# Day 03

library(tidyverse)

raw_data <- read_table(
  "data/day-03.txt",
  col_names = "content"
) 

df <- raw_data %>%
  mutate(
    n_chars = nchar(content),
    mid_point = floor(n_chars / 2),
    first = str_sub(content, start = 1L, end = mid_point),
    second = str_sub(content, start = mid_point + 1L, end = -1L),
    shared = map2_chr(
      .x = str_split(first, pattern = ""),
      .y = str_split(second, pattern = ""),
      .f = ~intersect(.x, .y)
    ),
    priority = match(shared, table = c(letters, LETTERS)),
    elf_group = rep(seq_len(100), 3) %>% sort()
  )

part_one_answer <- summarize(
  df,
  priority = sum(priority, na.rm = TRUE)
)

part_two_answer <- df %>%
  group_by(elf_group) %>%
  summarize(
    shared = str_split(content, pattern = "") %>% reduce(.f = intersect),
    priority = match(shared, table = c(letters, LETTERS)),
  ) %>%
  ungroup() %>%
  summarize(
    priority = sum(priority, na.rm = TRUE)
  )

