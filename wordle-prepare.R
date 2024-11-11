library(tidyverse)
library(explore)
source("wordle-tools.R")

data_ori <- read.csv("wordle-data.csv")
names(data_ori) <- c("word", "noun", "try_player_a", "try_player_b", "diff", "diff_cum")
data_ori$word <- tolower(data_ori$word)
data_ori$word <- substr(data_ori$word, 1, 5)

data_ori <- data_ori |> filter(!is.na(noun))

data_a <- data_ori |> 
  mutate(player = "Player A") |> 
  mutate(try = try_player_a) |> 
  add_var_id(name = "round") |> 
  select(round, word, noun, player, try)

data_b <- data_ori |> 
  mutate(player = "Player B") |> 
  mutate(try = try_player_b) |> 
  add_var_id(name = "round") |> 
  select(round, word, noun, player, try)

data <- rbind(data_a, data_b)
View(data)

data <- data |>
  mutate(
    aeiou = contains_letter_cnt("aeiou", word),
    unique = unique_letter_cnt(word),
    common = common_letter_cnt(word),
    rare = rare_letter_cnt(word)
  )

cnt <- letter_cnt(data$word)
cnt$n <- cnt$n/2
cnt |> explore(chr, n)
cnt |> arrange(-n) |> head(10)
 
data |> 
  mutate(language = "german") |> 
  select(round, word, language, everything()) |> 
  saveRDS("wordle.rds")
