library(tidyverse)
library(explore)
source("wordle-tools.R")

cnt <- letter_cnt(data$word)
cnt$n <- cnt$n/2
cnt |> explore(chr, n, flip = FALSE,
               title = "Frequency of letters in words",
               subtitle = "(used in WORDLE game)")
cnt |> arrange(-n) |> head(10)

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

data_all <- data_ori |> 
  add_var_id(name = "round") |> 
  mutate(diff = try_player_b - try_player_a) |> 
  mutate(diff_cum = cumsum(diff))
  
data_1 <- data_all |> filter(round < 20)   
data_2 <- data_all |> filter(round >= 20)   
data_3 <- data_all |> filter(round >= 40)   

color_1 <- "#888888" # "#FFAAAA"
color_2 <- "#c8b359" # "#FFAAAA"
color_3 <- "#67a760" # "red"
  

data_all |> 
  ggplot(aes(round, diff_cum)) +
    geom_line(color = color_1, linewidth = 2) +
    geom_line(data = data_2, color =  color_2, linewidth = 2) +
    geom_line(data = data_3, color =  color_3, linewidth = 2) +
    ylab("me better (cummulated)") +
    ylim(c(0,30)) +
    ggtitle("If a Data Scientist plays WORDLE") +
    labs(subtitle = "against a friend (for 60 days)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 20, color = "darkgrey", linetype = "dashed", linewidth = 0.8, alpha = 0.5) +
    geom_vline(xintercept = 40, color = "darkgrey", linetype = "dashed", linewidth = 0.8, alpha = 0.5) +
    geom_hline(yintercept = 14, color = "darkgrey", linetype = "dashed", linewidth = 0.8, alpha = 0.5) +
    geom_text(x=10, y=25, color = color_1, size = 3.5,
              label = "just use logic & intuition\nstart collecting data") +
    geom_text(x=30, y=25, color = color_2, size = 3.5,
            label = "your opponent\nwill catch up") +
    geom_text(x=50, y=25, color = color_3, size = 3.5,
          label = "use insights\nfrom data") 

  


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
