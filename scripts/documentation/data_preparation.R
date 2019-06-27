library(data.table)
library(magrittr)
library(ggplot2)
library(quanteda)
# library(plotly)
library(tidytext)
library(h2o)
library(caret)
library(shiny)

raw_data <- jsonlite::stream_in(file("data/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) %>% 
  as.data.table()
raw_data[, is_sarcastic := as.factor(is_sarcastic)]
raw_data[, text_length := nchar(headline)]
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
bag_of_words_data <- suppressWarnings(as.data.frame(bag_of_words_model))
freq <- as.data.table(colSums(bag_of_words_data[,-1]))
freq[, "variable" := names(bag_of_words_data[,-1])]
freq <- freq[order(-rank(V1))]
freq <- freq[1:50]
saveRDS(freq, "scripts/documentation/data/freq.rda")
