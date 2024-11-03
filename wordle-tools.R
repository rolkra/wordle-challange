
contains_letter_cnt <- function(letters, word_vec)  {
  
  letter_vec <- strsplit(letters, "")
  
  letter_regex <- paste0("[", paste0(letter_vec, collapse = "|"), "]")
  result_all <- c()
  
  for (i in seq_along(word_vec)) {
    
    word <- word_vec[i]
    
    result <- sum(grepl(letter_regex, substr(word,1,1), ignore.case = TRUE)) +
      sum(grepl(letter_regex, substr(word,2,2), ignore.case = TRUE)) +
      sum(grepl(letter_regex, substr(word,3,3), ignore.case = TRUE)) +
      sum(grepl(letter_regex, substr(word,4,4), ignore.case = TRUE)) +
      sum(grepl(letter_regex, substr(word,5,5), ignore.case = TRUE))
    
    result_all <- c(result_all, result)
  }
  
  result_all
}

unique_letter_cnt <- function(word_vec)  {
  
  result_all <- c()
  
  for (i in seq_along(word_vec)) {
    
    word <- word_vec[i]
    letters <- strsplit(word, "") [[1]]
    result <- length(unique(letters))
    
    result_all <- c(result_all, result)
  }
    
  result_all
  
}

unique_letter_cnt(c("autos", "gummi", "banal"))


letter_cnt <- function(word_vec)  {
  
  tmp <- data.frame(
    chr = unlist(strsplit(word_vec, "")) )
  
  all <- data.frame(chr = letters)
  tmp <- rbind(tmp, all)
  
  freq <- tmp |> count(chr) |> arrange(chr)
  freq$n <- freq$n -1
  freq
  
}

common_letter_cnt <- function(word_vec) {
  
  freq <- letter_cnt(word_vec) |> arrange(-n)
  common <- head(freq, 10)
  
  result <- contains_letter_cnt(
    paste(common$chr, collapse = ""), 
    word_vec)
  
  result
  
}

rare_letter_cnt <- function(word_vec) {
  
  freq <- letter_cnt(word_vec) |> arrange(-n)
  rare <- tail(freq, 10)
  
  result <- contains_letter_cnt(
    paste(rare$chr, collapse = ""), 
    word_vec)
  
  result
  
}


replace_chr <- function(word, pos, replace_chr) {
  
  result <- paste0(
    substr(word, 1, pos-1),
    replace_chr,
    substr(word, pos+1, 5)
  )
  
  result
  
}

one_word_score <- function(word_test, word_right)  {
  
  score <- 0
  
  if (substr(word_test, 1, 1) == substr(word_right, 1, 1)) {
      score <- score + 2
      word_test <- replace_chr(word_test, 1, "X")
      word_right <- replace_chr(word_right, 1, "P")
  }

  if (substr(word_test, 2, 2) == substr(word_right, 2, 2)) {
    score <- score + 2
    word_test <- replace_chr(word_test, 2, "X")
    word_right <- replace_chr(word_right, 2, "P")
  }
  
  if (substr(word_test, 3, 3) == substr(word_right, 3, 3)) {
    score <- score + 2
    word_test <- replace_chr(word_test, 3, "X")
    word_right <- replace_chr(word_right, 3, "P")
  }
  
  if (substr(word_test, 4, 4) == substr(word_right, 4, 4)) {
    score <- score + 2
    word_test <- replace_chr(word_test, 4, "X")
    word_right <- replace_chr(word_right, 4, "P")
  }
  
  for (i in 1:5) {
    
    check <- substr(word_test, i,i) == unlist(strsplit(word_right, ""))
    if (any(check)) {
      score <- score + 1
      word_test <- replace_chr(word_test, i, "X")
      word_right <- replace_chr(word_right, which(check)[1], "C")
    }  
    
  }
 
  score  
  
}  

word_score <- function(word, word_vec) {
  
  score_total <- 0
  
  for (i in seq_along(word_vec))  {
    
    score <- one_word_score(word, word_vec[i])
    score_total <- score_total + score
  }
  
  score_total
  
}

word_vec <- data_ori$word

# word_score("einer", word_vec)   ##94 ##161
# word_score("adieu", word_vec)   ##88 ##157
# word_score("raten", word_vec)   ##95 ##153
# word_score("tarne", word_vec)   ##90 ##146
# word_score("autos", word_vec)   ##69 ##142
# word_score("audio", word_vec)   ##63 ##141
# word_score("musik", word_vec)   ##139

word_score("autor", word_vec)   ##99
word_score("eiter", word_vec)   ##132
word_score("einer", word_vec)   ##124
word_score("raten", word_vec)   ##129
word_score("linse", word_vec)   ##112
word_score("musik", word_vec)   ##78

word_score("autor", word_vec)   ##99

