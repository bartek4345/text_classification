library(magrittr)
library(data.table)
library(ggplot2)
library(quanteda)
library(plotly)
# przyjzedc sie stopwordsom
# tfidf
# automl h2o    



raw_data <- jsonlite::stream_in(file("data/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) %>% 
  as.data.table()
raw_data[, is_sarcastic := as.factor(is_sarcastic)] # converting response variable to factor
prop.table(table(raw_data$is_sarcastic)) # checking distribution of response variable
length(which(!complete.cases(raw_data))) # checking missing data

# checking text length distributions
raw_data[, 'text_length' := nchar(headline)]
ggplot(raw_data, aes(x = text_length, fill = is_sarcastic)) +
  geom_density(alpha=.6)

raw_data[grepl("#", raw_data$headline)] # split the #
raw_data[grepl("&", raw_data$headline)]
raw_data[grepl("\\.{3}", raw_data$headline)] # maybe itll be new feature
raw_data[grepl("[A-Z]", raw_data$headline)] # tehe arent upper cases
raw_data[grepl("<.*>", raw_data$headline)] # no html tags
raw_data[grepl("u\\.", raw_data$headline)] # it is nessesary to replace u.s. 
raw_data[grepl("[!@#$%^&*]{3,10}", raw_data$headline)]
raw_data[grepl("[?]{2,10}", raw_data$headline)]


tokenized_data <- tokens(x = raw_data$headline, what = 'word',
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_hyphens = TRUE)
tokenized_data <- tokens_tolower(tokenized_data)
tokenized_data <- tokens_select(tokenized_data, stopwords(), 
                              selection = "remove")
tokenized_data <- tokens_wordstem(tokenized_data, language = "english")

bag_of_words_model <- dfm(tokenized_data, tolower = FALSE)


# Transform to a matrix and inspect.
bag_of_words_data <- as.data.table(bag_of_words_model)
View(bag_of_words_data[1:20, 1:100])
dim(bag_of_words_data)

# looking for more stopwords
# namsy <- names(bag_of_words_data)
# c(39, 40, 93)
freq <- as.data.table(colSums(bag_of_words_data[,-1]))
freq[, "variable" := names(bag_of_words_data[,-1])]
freq <- freq[order(-rank(V1))]
word_freq_plot <- ggplot(data = freq[50:100]) +
  geom_bar(aes(x = reorder(variable, -V1), y = V1), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("word") +
  ylab("count")

ggplotly(word_freq_plot)
  