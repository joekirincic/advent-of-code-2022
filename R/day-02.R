# Day 2

library(tidyverse)
library(torch)


# Relational solution -----------------------------------------------------
# This solution'll be more familiar to folks that write a lot of SQL. The idea
# is to treat the input data as a fact table and then create dimension
# tables with the different point values to join onto the fact table.

outcome_points <- tibble(
  outcome = c("win", "draw", "lose"),
  outcome_points = c(6L, 3L, 0L)
)

choice_points <- tibble(
  choice = c("X", "Y", "Z"),
  choice_points = c(1L, 2L, 3L)
)

raw_data <- read_table(
  file = "data/day-02.txt",
  col_names = c("opp", "you")
) %>%
  mutate(
    outcome = case_when(
      (opp=="A") & (you=="X") ~ "draw",
      (opp=="A") & (you=="Y") ~ "win",
      (opp=="A") & (you=="Z") ~ "lose",
      (opp=="B") & (you=="X") ~ "lose",
      (opp=="B") & (you=="Y") ~ "draw",
      (opp=="B") & (you=="Z") ~ "win",
      (opp=="C") & (you=="X") ~ "win",
      (opp=="C") & (you=="Y") ~ "lose",
      (opp=="C") & (you=="Z") ~ "draw",
      TRUE ~ NA_character_
    )
  )

relational_part_one <- raw_data %>%
  left_join(
    outcome_points,
    by = c("outcome" = "outcome")
  ) %>%
  left_join(
    choice_points,
    by = c("you" = "choice")
  ) %>%
  mutate(
    total_points = outcome_points + choice_points
  )

#sanity check
relational_part_one %>%
  distinct(opp, you, total_points) %>%
  arrange(opp, you)

relational_part_one_answer <- summarize(
  relational_part_one,
  total_points = sum(total_points, na.rm = TRUE)
)

relational_part_two <- raw_data %>%
  mutate(
    outcome = you,
    you = case_when(
      (opp=="A") & (outcome=="X") ~ "Z",
      (opp=="A") & (outcome=="Y") ~ "X",
      (opp=="A") & (outcome=="Z") ~ "Y",
      (opp=="B") & (outcome=="X") ~ "X",
      (opp=="B") & (outcome=="Y") ~ "Y",
      (opp=="B") & (outcome=="Z") ~ "Z",
      (opp=="C") & (outcome=="X") ~ "Y",
      (opp=="C") & (outcome=="Y") ~ "Z",
      (opp=="C") & (outcome=="Z") ~ "X",
      TRUE ~ NA_character_
    ),
    outcome = case_when(
      outcome=="X" ~ "lose",
      outcome=="Y" ~ "draw",
      outcome=="Z" ~ "win"
    )
  ) %>%
  left_join(
    outcome_points,
    by = c("outcome" = "outcome")
  ) %>%
  left_join(
    choice_points,
    by = c("you" = "choice")
  ) %>%
  mutate(
    total_points = outcome_points + choice_points
  )

#sanity checkr
relational_part_two %>%
  distinct(opp, you, total_points) %>%
  arrange(opp, you)

relational_part_two_answer <- summarize(
  relational_part_two,
  total_points = sum(total_points, na.rm = TRUE)
)


# Tensor solution ---------------------------------------------------------
# We can also use tensors to solve this.

possibilities <- torch_tensor(
  matrix(c(4,8,3,1,5,9,7,2,6), nrow = 3, ncol = 3, byrow = TRUE)
)

tensor_part_one <- raw_data %>%
  mutate(
    tensor_repn = case_when(
      (opp=="A") & (you=="X") ~ list(torch_tensor(matrix(c(1,0,0,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="A") & (you=="Y") ~ list(torch_tensor(matrix(c(0,1,0,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="A") & (you=="Z") ~ list(torch_tensor(matrix(c(0,0,1,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="X") ~ list(torch_tensor(matrix(c(0,0,0,1,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="Y") ~ list(torch_tensor(matrix(c(0,0,0,0,1,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="Z") ~ list(torch_tensor(matrix(c(0,0,0,0,0,1,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="X") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,1,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="Y") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,0,1,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="Z") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,0,0,1), nrow = 3, ncol = 3, byrow = TRUE))),
      TRUE ~ list(1)
    )
  ) %>%
  pull(tensor_repn) %>%
  map(~torch_reshape(.x, shape = c(1,3,3))) %>%
  torch_cat()

tensor_part_one_answer <- sum(tensor_part_one*possibilities)

tensor_part_two <- raw_data %>%
  mutate(
    tensor_repn = case_when(
      (opp=="A") & (you=="X") ~ list(torch_tensor(matrix(c(0,0,1,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="A") & (you=="Y") ~ list(torch_tensor(matrix(c(1,0,0,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="A") & (you=="Z") ~ list(torch_tensor(matrix(c(0,1,0,0,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="X") ~ list(torch_tensor(matrix(c(0,0,0,1,0,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="Y") ~ list(torch_tensor(matrix(c(0,0,0,0,1,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="B") & (you=="Z") ~ list(torch_tensor(matrix(c(0,0,0,0,0,1,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="X") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,0,1,0), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="Y") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,0,0,1), nrow = 3, ncol = 3, byrow = TRUE))),
      (opp=="C") & (you=="Z") ~ list(torch_tensor(matrix(c(0,0,0,0,0,0,1,0,0), nrow = 3, ncol = 3, byrow = TRUE))),
      TRUE ~ list(1)
    )
  ) %>%
  pull(tensor_repn) %>%
  map(~torch_reshape(.x, shape = c(1,3,3))) %>%
  torch_cat()

tensor_part_two_answer <- sum(tensor_part_two*possibilities)
