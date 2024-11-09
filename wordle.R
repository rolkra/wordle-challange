library(tidyverse)
library(explore)
source("wordle-tools.R")


data |> explore(try, target = player)
data |> summarize(mean(try), .by = player) |> explore(player, `mean(try)`)

data |> 
  filter(round >= 5) |> 
  summarize(mean(try), .by = player) |> 
  explore(player, `mean(try)`)

data |> explore(common, target = try, color = mix_color("lightblue", "coral", n = 7))
data |> explore(rare, target = try, color = mix_color("lightblue", "coral", n = 7))

data |> explain_tree(target = player)
data |> explain_tree(target = try)

data_ori |> 
  mutate(diff = try_player_a - try_player_b) |> 
  add_var_id("round") |> 
  mutate(diff_cum = cumsum(diff)) |> 
  ggplot(aes(round, diff_cum)) +
    geom_line(color = "red", linewidth = 2) +
    theme_minimal()

word_vec <- data_ori$word
word_score("autor", word_vec)   ##99  #113
word_score("eiter", word_vec)   ##132 #143
word_score("einer", word_vec)   ##124 #135
word_score("raten", word_vec)   ##129 #149
word_score("linse", word_vec)   ##112 #123
word_score("musik", word_vec)   ##78  #83
word_score("aitel", word_vec)   ##    #149
word_score("urson", word_vec)   ##    #95

word_score("aitel", word_vec) + word_score("urson", word_vec)   ## 244
word_score("autor", word_vec) + word_score("eines", word_vec)   ## 242
word_score("autor", word_vec) + word_score("linse", word_vec)   ## 236
word_score("raten", word_vec) + word_score("filou", word_vec)   ## 232
