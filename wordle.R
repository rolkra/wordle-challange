library(tidyverse)
library(explore)
source("wordle-tools.R")


data |> explore(try, target = player)
data |> summarize(mean(try), .by = player) |> explore(player, `mean(try)`)

data |> 
  filter(round >= 5) |> 
  summarize(mean(try), .by = player) |> 
  explore(player, `mean(try)`)

data |> explore(common, target = try)
data |> explore(rare, target = try)

data |> explain_tree(target = player)
data |> explain_tree(target = try)

data_ori |> 
  mutate(diff = try_player_a - try_player_b) |> 
  add_var_id("round") |> 
  mutate(diff_cum = cumsum(diff)) |> 
  ggplot(aes(round, diff_cum)) +
    geom_line(color = "red", size = 2) +
    theme_minimal()
  