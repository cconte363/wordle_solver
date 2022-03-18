rm(list=ls())

load = function(package) {
  tryCatch({library(package, character.only = TRUE)},
           error = function(e) {install.packages(eval(package), dependencies = TRUE);
             library(package, character.only = TRUE)})
}

load('readr')
shakespeare = read_file("https://www.gutenberg.org/files/100/100-0.txt")
all_words = scan(text = shakespeare, what ='')
#remove punctuation & numbers
only_letters = gsub('[[:punct:]|[:digit:]|[:space:]|’|‘|—|”|“]', '', all_words)
#lowercase
lowercase = tolower(only_letters)
#subset 5 letter words
five_letters = lowercase[which(nchar(lowercase)==5)]
#dedupe
deduped = unique(five_letters)
word_df = data.frame(word = deduped)
#assuming all words have equal probability of being chosen
#a separate analysis could be done to determine if there were a relationship between the popularity of a word and it's chance of being chosen.
rm(list=setdiff(ls(), c('word_df', 'load')))
#create a data frame counting the number of words in which a letter appears. 
#I'm going to count repeated occurrences of a letter as a separate letter to be guessed. 
#For example, a second e could be more likely than the letter z.
letter_df = data.frame(letter = c(letters, paste(paste(letters, '2', sep = '')), paste(paste(letters, '3', sep = ''))))
letter_df$regex_pattern = sapply(letter_df$letter, FUN = function(x) {
  ifelse(grepl('2', x), 
         paste0(gsub('2', '', x), '.*', gsub('2', '', x)),
         ifelse(grepl('3', x), 
                paste0(gsub('3', '', x), '.*',gsub('3', '', x), '.*',gsub('3', '', x)),
                x))})
                                                                     
calculate_scores = function(words_table = word_df) {
  letter_df$words_containing = sapply(letter_df$regex_pattern, function(x) {sum(grepl(x, words_table$word))})
  words_table$score = rep(0, nrow(words_table))2
  for (i in 1:nrow(letter_df))
  {
    pattern = letter_df$regex_pattern[i]
    points = letter_df$words_containing[i]
    words_table$score = words_table$score + grepl(pattern, words_table$word) * points
  }
  words_table$ranked_score = rank(words_table$score, ties.method = "min")
  words_table$normalized_score = (words_table$ranked_score - mean(words_table$ranked_score))/sd(words_table$ranked_score)
  cat('Reccommends guessing', words_table$word[which(words_table$score==max(words_table$score))])
  return(words_table)
}

#helper function for update_guess
create_regex_pattern = function(letters, positions)
{
  pattern = rep('.', 5)
  for (i in 1:length(letters))
  {
    pattern[positions[i]] = letters[i]
  }
  return(pattern)
}

load('dplyr')
update_guess = function(word, #word guessed 
                        colors, #list of resulting tile colors, must be green, yellow, or gray
                        words_table = word_df) {
  letters = strsplit(word, '')[[1]]
  #update green tiles
  pattern = create_regex_pattern(letters[which(colors == 'green')], which(colors == 'green'))
  words_table = words_table %>% filter(grepl(paste0(pattern, collapse = ''), word))
  #update yellow tiles
  yellow_positions = which(colors == 'yellow')
  yellow_letters = letters[yellow_positions]
  for (i in 1:length(yellow_letters))
  {
    #require yellow letters
    words_table = words_table %>% filter(grepl(yellow_letters[i], word))
    #remove yellow letters in incorrect positions
    pattern = create_regex_pattern(yellow_letters[i], yellow_positions[i])
    words_table = words_table %>% filter(!grepl(paste0(pattern, collapse = ''), word))
  }
  #update gray tiles
  gray_letters = letters[which(colors == 'gray')]
  for (letter in gray_letters)
  {
    words_table = words_table %>% filter(!grepl(letter, word))
  }
  words_table = calculate_scores(words_table)
  return(words_table)
}

regression_test = FALSE
if (regression_test)
{
  test_1 = calculate_scores(word_df)
  test_2 = update_guess('arose', c('gray', 'gray', 'yellow', 'gray', 'gray'), words_table = test_1)
  test_3 = update_guess('pilot', c('gray', 'gray', 'gray', 'yellow', 'yellow'), words_table = test_2)
  rm(list = setdiff(ls(), c('calculate_scores', 'create_regex_pattern', 'load', 'update_guess', 'letter_df', 'word_df')))
}


