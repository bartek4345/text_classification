library(magrittr)
library(data.table)
library(ggplot2)
library(quanteda)
library(plotly)
library(tidytext)
library(dplyr)
library(h2o)
library(caret)

raw_data <- jsonlite::stream_in(file("data/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) %>% 
  as_tibble()
raw_data <- mutate(raw_data, is_sarcastic = as.factor(is_sarcastic)) # converting response variable to factor
prop.table(table(raw_data$is_sarcastic)) # checking distribution of response variable
length(which(!complete.cases(raw_data))) # checking missing data

# checking text length distributions
raw_data <- mutate(raw_data, text_length = nchar(headline))
ggplot(raw_data, aes(x = text_length, fill = is_sarcastic)) +
  geom_density(alpha=.6)

# preparing letters sequence pattern
letters_pattern <- c()
for (letter in letters) {
  letters_pattern <- c(letters_pattern, sapply(3:10, function(x) c(paste(rep(letter, x), collapse = ""))))
}
letters_pattern <- paste(letters_pattern, collapse = "|")

# Exploration based on regex
filter(raw_data, grepl(letters_pattern, headline)) %>% View() # it deosnt seem to confirm the rule that words with repewating letters are important
filter(raw_data, grepl("#", headline)) %>% View() # split the #
filter(raw_data, grepl("\\.{3}", headline)) %>%  View() #checking ...
filter(raw_data, grepl("[?]{1,10}", headline)) %>% View()# checking ? signs
filter(raw_data, grepl("[!]{1,10}", headline)) %>% View() # checking ! signs
filter(raw_data, grepl("[A-Z]", headline))# TODO: tehe arent upper cases - ask in report if it is possible to get data before lowercasing porcess  
filter(raw_data, grepl("<.*>", headline)) # no html tags
filter(raw_data, grepl("u\\.", headline)) %>% View() # it is nessesary to replace u.s. 
filter(raw_data, grepl("[!@#$%^&*]{3,10}", headline)) # checking weird sequences of signs
filter(raw_data, grepl("[0-9]", headline)) %>% View() # checking numerics

# raw_data <- mutate(raw_data, line = 1:n())

tokenized_data <- tokens(x = raw_data$headline, what = 'word',
                         remove_numbers = TRUE,
                         remove_hyphens = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE)
tokenized_data <- tokens_tolower(tokenized_data)
tokenized_data <- tokens_select(tokenized_data, stop_words$word, 
                                selection = "remove")
tokenized_data <- tokens_wordstem(tokenized_data, language = "english")

bag_of_words_model <- dfm(tokenized_data, tolower = FALSE)



# Transform to a matrix and inspect.
bag_of_words_data <- as.data.frame(bag_of_words_model)
View(bag_of_words_data[1:20, 1:100])
dim(bag_of_words_data)

freq <- as.data.table(colSums(bag_of_words_data[,-1]))
freq[, "variable" := names(bag_of_words_data[,-1])]
freq <- freq[order(-rank(V1))]
word_freq_plot <- ggplot(data = freq[1:100]) +
  geom_bar(aes(x = reorder(variable, -V1), y = V1), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("word") +
  ylab("count")

ggplotly(word_freq_plot)
