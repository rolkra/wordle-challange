library(tidyverse)
library(explore)
source("wordle-tools.R")

data_ori <- read.csv("wordle-data.csv")

data_a <- data_ori |> 
  mutate(player = "Player A") |> 
  mutate(try = try_player_a) |> 
  select(word, noun, player, try)

data_b <- data_ori |> 
  mutate(player = "Player B") |> 
  mutate(try = try_player_b) |> 
  select(word, noun, player, try)

data <- rbind(data_a, data_b)
View(data)

data <- data |>
  mutate(
    aeiou_cnt = contains_letter_cnt("aeiou", word),
    unique = unique_letter_cnt(word) 
  )

data |> 
  explore(try, target = player)
data |> explain_tree(target = player)
data |> explain_tree(target = try)
