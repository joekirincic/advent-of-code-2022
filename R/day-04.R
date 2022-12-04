
# Day 04

library(tidyverse)

raw_data <- read_csv(
  "data/day-04.txt",
  col_names = c("first", "second")
)

df <- raw_data %>%
  mutate(
    # split each string on the "-" and pass endpoints to seq to make two ranges.
    first = str_split(first, pattern = "-") %>% map(~seq(.x[1], .x[2], by = 1)),
    second = str_split(second, pattern = "-") %>% map(~seq(.x[1], .x[2], by = 1)),
    # find the intersection of each pair.
    shared = map2(.x = first, .y = second, ~intersect(.x, .y)),
    # "fully contained" means the intersection is equal to either first or second.
    fully_contained_ind = pmap_lgl(
      list(first, second, shared),
      \(f, s, sh){
        setequal(f, sh) | setequal(s, sh)
      }
    ),
    # "overlap" means the intersection has at least one element.
    overlaps_ind = map_lgl(.x = shared, ~length(.x) > 0)
  )

part_one_answer <- summarize(df, fully_contained_ind = sum(fully_contained_ind, na.rm = TRUE)) # 550
part_two_answer <- summarize(df, overlaps_ind = sum(overlaps_ind, na.rm = TRUE)) # 931
