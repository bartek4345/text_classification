library(magrittr) # %>% sign
library(data.table) # data manipulation
library(ggplot2) # plots
library(quanteda) # cleaning text
library(plotly) # plots
library(tidytext) # stopwords frame
library(dplyr) # data manipulation
library(h2o) # building classifiers
library(caret) # test/training splitting
library(irlba)
library(tictoc)
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
  # '#' - i think, there isnt way to automaticaly split the # - ill do it by hand ----
  # data[grepl("#xmasgiftsfromtrump", headline), headline := gsub("#xmasgiftsfromtrump", "xmas gifts from trump", headline)]
  # data[grepl("#icantbreathe", headline), headline := gsub("#icantbreathe", "i can't breathe", headline)]
  # data[grepl("#1", headline), headline := gsub("#1", "number 1", headline)]
  # data[grepl("#brownribboncampaign", headline), headline := gsub("#brownribboncampaign", "brown ribbon campaign", headline)]
  # data[grepl("#blacklivesmatter", headline), headline := gsub("#blacklivesmatter", "black lives matter", headline)]
  # data[grepl("#talktome", headline), headline := gsub("#talktome", "talk to me", headline)]
  # data[grepl("#trybeatingmelightly", headline), headline := gsub("#trybeatingmelightly", "try beating me lightly", headline)]
  # data[grepl("#dirtydenier\\$", headline), headline := gsub("#dirtydenier\\$", "dirty denier", headline)]
  # data[grepl("#goals", headline), headline := gsub("#goals", "goals", headline)]
  # data[grepl("#kellyonmymind", headline), headline := gsub("#kellyonmymind", "kelly on my mind", headline)]
  # data[grepl("#ridiculousexcusestostayhome", headline),
  #      headline := gsub("#ridiculousexcusestostayhome", " ridiculous excuses to stay home", headline)]
  # data[grepl("#prayforpaulgeorge", headline), headline := gsub("#prayforpaulgeorge", "pray for paul george", headline)]
  # data[grepl("#starwarschristmascarols", headline), headline := gsub("#starwarschristmascarols", "star wars christmas carols", headline)]
  # data[grepl("#trickortreatin100years", headline), headline := gsub("#trickortreatin100years", "trick or treat in 100 years", headline)]
  # data[grepl("#notme", headline), headline := gsub("#notme", "not me", headline)]
  # data[grepl("#trumphair", headline), headline := gsub("#trumphair", "trump hair", headline)]
  # data[grepl("#womenboycotttwitter", headline), headline := gsub("#womenboycotttwitter", "women boycott twitter", headline)]
  # data[grepl("#trumpbacktoschooltips", headline), headline := gsub("#trumpbacktoschooltips", "trump back to school tips", headline)]
  # data[grepl("#alternativefacts", headline), headline := gsub("#alternativefacts", "alternative facts", headline)]
  # data[grepl("#metoo", headline), headline := gsub("#metoo", "me too", headline)]
  # data[grepl("#mewesyria", headline), headline := gsub("#mewesyria", "mewe syria", headline)]
  # data[grepl("#addcandytoamovie", headline), headline := gsub("#addcandytoamovie", "add candy to a movie", headline)]
  # data[grepl("#nevertrump", headline), headline := gsub("#nevertrump", "never trump", headline)]
  # data[grepl("#squadgoals", headline), headline := gsub("#squadgoals", "squad goals", headline)]
  # data[grepl("#gopsongsaboutethics", headline), headline := gsub("#gopsongsaboutethics", "gop songs about ethics", headline)]
  # data[grepl("#badpicturemonday", headline), headline := gsub("#badpicturemonday", "bad picture monday", headline)]
  # data[grepl("#teamlogan", headline), headline := gsub("#teamlogan", "team logan", headline)]
  # data[grepl("#teamnocancer", headline), headline := gsub("#teamnocancer", "team no cancer", headline)]
  # data[grepl("#obamaandkids", headline), headline := gsub("#obamaandkids", "obama and kids", headline)]
  # data[grepl("#dropthecover", headline), headline := gsub("#dropthecover", "drop the cover", headline)]
  # data[grepl("#hscc2015", headline), headline := gsub("#hscc2015", "", headline)]
  # data[grepl("#napaquake", headline), headline := gsub("#napaquake", "napa quake", headline)]
  # data[grepl("#nofilter", headline), headline := gsub("#nofilter", "no filter", headline)]
  # data[grepl("#digitalhealth", headline), headline := gsub("#digitalhealth", "digital health", headline)]
  # data[grepl("#napastrong", headline), headline := gsub("#napastrong", "napa strong", headline)]
  # data[grepl("#alohahuffpost", headline), headline := gsub("#alohahuffpost", "aloha huff post", headline)]
  # data[grepl("#surfbort", headline), headline := gsub("#surfbort", "surfbort", headline)]
  # data[grepl("#4", headline), headline := gsub("#4", "number 4", headline)]
  # data[grepl("#nobannowall", headline), headline := gsub("#nobannowall", "no ban no wall", headline)]
  # data[grepl("#millionsmarchsf", headline), headline := gsub("#millionsmarchsf", "millions march sf", headline)]
  # data[grepl("#emojisinthewild", headline), headline := gsub("#emojisinthewild", "emojis in the wild", headline)]
  # data[grepl("#staywokeandvote", headline), headline := gsub("#staywokeandvote", "stay woke and vote", headline)]
  # data[grepl("#oscarssowhite", headline), headline := gsub("#oscarssowhite", "oscars so white", headline)]
  # data[grepl("#iphonefeatures4politicians", headline),
  #      headline := gsub("#iphonefeatures4politicians", "iphone features for politicians", headline)]
  # data[grepl("#myroommateisweird", headline), headline := gsub("#myroommateisweird", "my roommate is weird", headline)]
  # data[grepl("#youlookdisgusting", headline), headline := gsub("#youlookdisgusting", "you look disgusting", headline)]
  # data[grepl("#nspw2017", headline), headline := gsub("#nspw2017", "", headline)]
  # data[grepl("#ionceoverheard", headline), headline := gsub("#ionceoverheard", "i once overheard", headline)]
  # data[grepl("#explainthe90sin4words", headline), headline := gsub("#explainthe90sin4words", "explain the 90s in 4 words", headline)]
  # data[grepl("#dearbetsy", headline), headline := gsub("#dearbetsy", "dear betsy", headline)]
  # data[grepl("#friendshipgoals", headline), headline := gsub("#friendshipgoals", "friendship goals", headline)]
  # data[grepl("#missuniverse2015", headline), headline := gsub("#missuniverse2015", "miss universe 2015", headline)]
  # data[grepl("#addclimatechangetotv", headline), headline := gsub("#addclimatechangetotv", "add climate change to tv", headline)]
  # data[grepl("#5", headline), headline := gsub("#5", "number 5", headline)]
  # data[grepl("#feelthebern", headline), headline := gsub("#feelthebern", "feel the bern", headline)]
  # data[grepl("#trumpacandy", headline), headline := gsub("#trumpacandy", "trump a candy", headline)]
  # data[grepl("#middleagedschmovies", headline), headline := gsub("#middleagedschmovies", "middle aged schmovies", headline)]
  # data[grepl("#thereisaidit", headline), headline := gsub("#thereisaidit", "there i said it", headline)]
  # data[grepl("#dadlife", headline), headline := gsub("#dadlife", "dad life", headline)]
  # data[grepl("#meat14", headline), headline := gsub("#meat14", "me at 14", headline)]
  # data[grepl("#thrive", headline), headline := gsub("#thrive", "thrive", headline)]
  # data[grepl("#menforchoice", headline), headline := gsub("#menforchoice", "men for choice", headline)]
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
term.frequency <- function(row) {
  row / sum(row)
}
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}
tf.idf <- function(x, idf) {
  x * idf
}

# READING AND SPLIT DATA ----
raw_data <- jsonlite::stream_in(file("data/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) 
raw_data <- as.data.table(raw_data)
raw_data[ , is_sarcastic := as.factor(is_sarcastic)] # converting response variable to factor
raw_data <- unique(raw_data) # there is ununique data - ~100 rows
raw_data[ , text_length := nchar(headline)]
indexes <- createDataPartition(raw_data$is_sarcastic, times = 1,
                               p = 0.8, list = FALSE)
train_data <- raw_data[indexes,]
test_data <- raw_data[-indexes,]

# BUILDING TRAINING DATA ----
train_data <- clean_data(train_data)
train_tokens <- build_tokens(train_data) # preparing tokens
train_dfm <- dfm(train_tokens, tolower = FALSE)
train_tf <- apply(train_dfm, 1, term.frequency)
train_idf <- apply(train_dfm, 2, inverse.doc.freq) # this will be used both in trin and test data
train_tfidf <-  apply(train_tf, 2, tf.idf, idf = train_idf) 
train_tfidf <- t(train_tfidf)
incomplete_cases <- which(!complete.cases(train_tfidf)) # checking uncomplete cases
train_tfidf[incomplete_cases,] <- rep(0.0, ncol(train_tfidf)) # fixing uncomplete cases
tic()
tfidf_train_irlba <- irlba(t(train_tfidf), nv = 500, maxit = 1000 ) # singular vectors decompozition
toc()
svd_train_data <- data.frame(is_sarcastic = train_data$is_sarcastic,
                             text_length = train_data$text_length,
                             tfidf_train_irlba$v)

# BUILDING TEST DATA ----
test_data <- clean_data(test_data)
test_tokens <- build_tokens(test_data) # preparing tokens
test_dfm <- dfm(test.tokens, tolower = FALSE)
test_dfm <- dfm_select(test_dfm, pattern = train_dfm,
                       selection = "keep")
test_tf <- apply(test_dfm, 1, term.frequency)
test_tfidf <-  apply(test_tf, 2, tf.idf, idf = train_idf) 
test_tfidf <- t(test_tfidf)
# test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
incomplete_cases <- which(!complete.cases(test.tokens.tfidf)) # checking uncomplete cases
test_tfidf[incomplete_cases,] <- rep(0.0, ncol(test_tfidf)) # fixing uncomplete cases
sigma.inverse <- 1 / tfidf_train_irlba$d
u.transpose <- t(tfidf_train_irlba$u)
svd_test_data <- t(sigma.inverse * u.transpose %*% t(test_tfidf))
svd_test_data <- data.frame(is_sarcastic = test_data$is_sarcastic,
                             text_length = test_data$text_length,
                            svd_test_data)

# BUILDING MODELS ----
h2o.init(max_mem_size = '70g')
x <- names(svd_train_data)
x <- x[!x %in% c("is_sarcastic")]
svd_train_data <- as.h2o(svd_train_data)

model <- h2o.automl(x = x,
                    y = "is_sarcastic",
                    training_frame = svd_train_data,
                    nfolds = 5,
                    balance_classes = TRUE,
                    exclude_algos = c("DeepLearning", "StackedEnsemble"),
                    max_runtime_secs = 60*60*10)
model_leader <- model@leader
model_leader@algorithm 
model_leader@model$model_summary
model_leader@model$training_metrics
model_leader@model$cross_validation_metrics 

# MAKING PREDICTION ----
svd_test_data <- as.h2o(svd_test_data)
prediction <- h2o.predict(model_leader, svd_test_data) %>% 
  as.data.table()
prediction$actual <- as.vector(svd_test_data[["is_sarcastic"]])
table(prediction[, .(actual, predict)])
c<-confusionMatrix(as.factor(prediction$actual), prediction$predict)
