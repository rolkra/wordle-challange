library(tidyverse)
library(explore)
source("wordle-tools.R")


data |> 
  explore(try, target = player)
data |> 
  explore(common, target = try)

data |> explain_tree(target = player)
data |> explain_tree(target = try)

