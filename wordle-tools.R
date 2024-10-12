
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
