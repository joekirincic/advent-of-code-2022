
# Day 05

library(tidyverse)

original <- list(
  c("[P]", "[G]", "[R]", "[N]"),
  c("[C]", "[D]", "[G]", "[F]", "[L]", "[B]", "[T]", "[J]"),
  c("[V]", "[S]", "[M]"),
  c("[P]", "[Z]", "[C]", "[R]", "[S]", "[L]"),
  c("[Q]", "[D]", "[W]", "[C]", "[V]", "[L]", "[S]", "[P]"),
  c("[S]", "[M]", "[D]", "[W]", "[N]", "[T]", "[C]"),
  c("[P]", "[W]", "[G]", "[D]", "[H]"),
  c("[V]", "[M]", "[C]", "[S]", "[H]", "[P]", "[L]", "[Z]"),
  c("[Z]", "[G]", "[W]", "[L]", "[F]", "[P]", "[R]")
) %>% map(rev)

raw_data <- tibble(
  content = read_lines("data/day-05.txt", skip = 10),
  digits = str_extract_all(content, pattern = "\\d{1,}") %>% map(parse_integer),
  move = map_int(digits, 1),
  from = map_int(digits, 2),
  to = map_int(digits, 3),
  args = pmap(
    .l = list(move, from, to),
    .f = \(m, f, t){
      list(move = m, from = f, to = t)
    }
  ),
  result1 = accumulate(
    .x = args,
    .f = \(res, args){
      # Get `move` units from `from`, reverse them, and append to `to`.
      #print(args)
      f <- res[[args$from]]
      t <- res[[args$to]]
      chunk <- rev(tail(f, args$move))
      t <- append(t, values = chunk)
      f <- head(f, -args$move)
      res[[args$from]] <- f
      res[[args$to]] <- t
      return(res)
    },
    .init = original
  ) %>% `[`(2:length(.)),
  result2 = accumulate(
    .x = args,
    .f = \(res, args){
      # Get `move` units from `from`, don't reverse them, and append to `to`.
      #print(args)
      f <- res[[args$from]]
      t <- res[[args$to]]
      chunk <- tail(f, args$move)
      t <- append(t, values = chunk)
      f <- head(f, -args$move)
      res[[args$from]] <- f
      res[[args$to]] <- t
      return(res)
    },
    .init = original
  ) %>% `[`(2:length(.))
)

answers <- raw_data %>%
  slice_tail(n = 1) %>%
  summarize(
    part_one = map_chr(result1, ~map(.x, \(z){ tail(z, 1) }) %>% glue::glue_collapse()) %>% str_remove_all(pattern = "\\[|\\]"),
    part_two = map_chr(result2, ~map(.x, \(z){ tail(z, 1) }) %>% glue::glue_collapse()) %>% str_remove_all(pattern = "\\[|\\]")
  )
 