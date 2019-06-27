library(data.table) # data manipulation
library(ggplot2) # plots
library(quanteda) # cleaning text
library(plotly) # plots
library(tidytext) # stopwords frame
library(dplyr) # data manipulation
library(h2o) # building classifiers
library(caret) # test/training splitting
library(irlba) # partial svd
library(tictoc) # evaluation time
set.seed(333)

# FUCTIONS ----
clean_data <- function(data){
  # don'ts removal
  data[grepl("don'ts", headline), headline := gsub("don'ts", "", headline)]
  # do's removal
  data[grepl("do's", headline), headline := gsub("do's", "", headline)]
  # dont's removal
  data[grepl("dont's", headline), headline := gsub("dont's", "", headline)]
  # websites - there is high ratio of sarcastic texts. I'll replace urls with 'weblink' and will add this variable to furtherr steps 
  data[grepl("[a-z]{2,}\\.[a-z]{2, }", headline), headline := gsub("[a-z]{2,}\\.[a-z]{2, }", "weblink", headline)]
  prop.table(table(data[grepl("weblink", headline), is_sarcastic]))
  # @jack removal
  data[grepl("@jack", headline), headline := gsub("@jack", "jack", headline)]
  # roman numerals removal
  data[grepl("\\s+([ivx]{2,})\\b", headline), headline := gsub("\\s+([ivx]{2,})\\b", "", headline)]
  # who'd removal
  data[grepl("who'd ", headline), headline := gsub("who'd ", "", headline)]
  # g.o.o.d replacement
  data[grepl("g.o.o.d ", headline), headline := gsub("g.o.o.d ", "good", headline)]
  # # removal
  data[grepl("#([a-z]|[0-9])+", headline), headline := gsub("#([a-z]|[0-9])+", "", headline)]
  return(data)
}

build_tokens <- function(data, include_bigrams = FALSE){
  data <- tokens(x = data$headline, what = 'word',
                 remove_numbers = TRUE,
                 remove_hyphens = TRUE,
                 remove_punct = TRUE,
                 remove_symbols = TRUE)
  data <- tokens_tolower(data)
  data <- tokens_select(data, stop_words$word, selection = "remove")
  data <- tokens_wordstem(data, language = "english")
  if(include_bigrams == TRUE){
    data <- tokens_ngrams(data, n = 1:2)
  }
  return(data)
}

term_frequency <- function(row) {
  row / sum(row)
}

inverse_doc_freq <- function(col) {
  corpus_size <- length(col)
  doc_count <- length(which(col > 0))
  
  log10(corpus_size / doc_count)
}

tf_idf <- function(x, idf) {
  x * idf
}

# READING AND SPLIT DATA ----
raw_data <- jsonlite::stream_in(file("data/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) 
raw_data <- as.data.table(raw_data)
raw_data[ , is_sarcastic := as.factor(is_sarcastic)] # converting response variable to factor
raw_data <- unique(raw_data) # there is ununique data - ~100 rows
raw_data[ , text_length := nchar(headline)] # preparing text length - new feature
indexes <- createDataPartition(raw_data$is_sarcastic, times = 1, # splitting data
                               p = 0.7, list = FALSE)
train_data <- raw_data[indexes,]
test_data <- raw_data[-indexes,]

# BUILDING TRAINING DATA ----
train_data <- clean_data(train_data)
train_tokens <- build_tokens(train_data, include_bigrams = TRUE) # preparing tokens
train_dfm <- dfm(train_tokens, tolower = FALSE) # preparing data frequency matrix
train_tf <- apply(train_dfm, 1, term_frequency) ; gc() 
train_idf <- apply(train_dfm, 2, inverse_doc_freq) ; gc() # this will be used both in trin and test data
train_tfidf <-  apply(train_tf, 2, tf_idf, idf = train_idf) ; gc()
train_tfidf <- t(train_tfidf)
incomplete_cases <- which(!complete.cases(train_tfidf)) # checking incomplete cases
train_tfidf[incomplete_cases,] <- rep(0.0, ncol(train_tfidf)) # fixing incomplete cases
tic()
tfidf_train_irlba <- irlba(t(train_tfidf), nv = 500, maxit = 1000 ) # partial singular vectors decompozition
toc()
svd_train_data <- data.frame(is_sarcastic = train_data$is_sarcastic, # preaparing final data frame
                             text_length = train_data$text_length,
                             tfidf_train_irlba$v)
saveRDS(svd_train_data, "data/svd_train_data.rda")

# BUILDING TEST DATA ----
test_data <- clean_data(test_data)
test_tokens <- build_tokens(test_data, include_bigrams = TRUE) # preparing tokens
test_dfm <- dfm(test_tokens, tolower = FALSE) # preparing data frequency matrix
test_dfm <- dfm_select(test_dfm, pattern = train_dfm, selection = "keep") # adjusting of test frame to has same tokens as training frame
test_tf <- apply(test_dfm, 1, term_frequency)
test_tfidf <-  apply(test_tf, 2, tf_idf, idf = train_idf) 
test_tfidf <- t(test_tfidf)
# test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
incomplete_cases <- which(!complete.cases(test_tfidf)) # checking uncomplete cases
test_tfidf[incomplete_cases,] <- rep(0.0, ncol(test_tfidf)) # fixing uncomplete cases
sigma.inverse <- 1 / tfidf_train_irlba$d 
u.transpose <- t(tfidf_train_irlba$u)
svd_test_data <- t(sigma.inverse * u.transpose %*% t(test_tfidf))
svd_test_data <- data.frame(is_sarcastic = test_data$is_sarcastic,
                            text_length = test_data$text_length,
                            svd_test_data)
saveRDS(svd_test_data, "data/svd_test_data.rda")

# BUILDING MODELS ----
h2o.init(max_mem_size = '70g')
x <- names(svd_train_data)
x <- x[!x %in% c("is_sarcastic")]
svd_train_data <- as.h2o(svd_train_data)

model <- h2o.automl(x = x,
                    y = "is_sarcastic",
                    training_frame = svd_train_data,
                    nfolds = 10,
                    balance_classes = TRUE,
                    exclude_algos = c("DeepLearning", "StackedEnsemble"),
                    max_runtime_secs = 60*60*10)
model_leader <- model@leader
model_leader@algorithm 
model_leader@model$model_summary
model_leader@model$training_metrics
model_leader@model$cross_validation_metrics 

glm_model <- h2o.getModel("GLM_grid_1_AutoML_20190627_093200_model_1")
glm_model@algorithm 
glm_model@model$model_summary
glm_model@model$training_metrics
glm_model@model$cross_validation_metrics

# MAKING PREDICTION ----

svd_test_data <- as.h2o(svd_test_data)
# Xgboost
prediction <- h2o.predict(model_leader, svd_test_data) %>% 
  as.data.table()
prediction$actual <- as.vector(svd_test_data[["is_sarcastic"]])
confusionMatrix(as.factor(prediction$actual), prediction$predict)
# GLM
prediction <- h2o.predict(glm_model, svd_test_data) %>% 
  as.data.table()
prediction$actual <- as.vector(svd_test_data[["is_sarcastic"]])
confusionMatrix(as.factor(prediction$actual), prediction$predict)
h2o.saveModel(glm_model, "results/")
file.rename(file.path("results/", glm_model@model_id), "results/model")
saveRDS(prediction, "results/prediction.rda")
# h2o.loadModel("/home/bsmulski/model")