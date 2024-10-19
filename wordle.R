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

