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
raw_data <- jsonlite::stream_in(file("/home/bsmulski/C4L Academy - Data Science Graduate - HEADLINES dataset (2019-06).json")) 
raw_data <- as.data.table(raw_data)
raw_data[ , is_sarcastic := as.factor(is_sarcastic)] # converting response variable to factor
raw_data[25390]$headline <- gsub(pattern = "inſurgency", replacement = "infurgency", x = raw_data[25390]$headline)
# there is ununique data - ~50 rows
raw_data <- unique(raw_data)
# don'ts removal
raw_data[grepl("don'ts", headline), headline := gsub("don'ts", "", headline)]
# do's removal
raw_data[grepl("do's", headline), headline := gsub("do's", "", headline)]
# dont's removal
raw_data[grepl("dont's", headline), headline := gsub("dont's", "", headline)]
# websites - there is high ratio of sarcastic texts. I'll replace urls with 'weblink' and will add this variable to furtherr steps 
raw_data[grepl("[a-z]{2,}\\.[a-z]{2, }", headline), headline := gsub("[a-z]{2,}\\.[a-z]{2, }", "weblink", headline)]
prop.table(table(raw_data[grepl("weblink", headline), is_sarcastic]))
# @jack removal
raw_data[grepl("@jack", headline), headline := gsub("@jack", "jack", headline)]
# roman numerals removal
raw_data[grepl("\\s+([ivx]{2,})\\b", headline), headline := gsub("\\s+([ivx]{2,})\\b", "", headline)]
# who'd removal
raw_data[grepl("who'd ", headline), headline := gsub("who'd ", "", headline)]
# g.o.o.d replacement
raw_data[grepl("g.o.o.d ", headline), headline := gsub("g.o.o.d ", "good", headline)]
# '#' - i think, there isnt way to automaticaly split the # - ill do it by hand ----
raw_data[grepl("#xmasgiftsfromtrump", headline), headline := gsub("#xmasgiftsfromtrump", "xmas gifts from trump", headline)]
raw_data[grepl("#icantbreathe", headline), headline := gsub("#icantbreathe", "i can't breathe", headline)]
raw_data[grepl("#1", headline), headline := gsub("#1", "number 1", headline)]
raw_data[grepl("#brownribboncampaign", headline), headline := gsub("#brownribboncampaign", "brown ribbon campaign", headline)]
raw_data[grepl("#blacklivesmatter", headline), headline := gsub("#blacklivesmatter", "black lives matter", headline)]
raw_data[grepl("#talktome", headline), headline := gsub("#talktome", "talk to me", headline)]
raw_data[grepl("#trybeatingmelightly", headline), headline := gsub("#trybeatingmelightly", "try beating me lightly", headline)]
raw_data[grepl("#dirtydenier\\$", headline), headline := gsub("#dirtydenier\\$", "dirty denier", headline)]
raw_data[grepl("#goals", headline), headline := gsub("#goals", "goals", headline)]
raw_data[grepl("#kellyonmymind", headline), headline := gsub("#kellyonmymind", "kelly on my mind", headline)]
raw_data[grepl("#ridiculousexcusestostayhome", headline),
         headline := gsub("#ridiculousexcusestostayhome", " ridiculous excuses to stay home", headline)]
raw_data[grepl("#prayforpaulgeorge", headline), headline := gsub("#prayforpaulgeorge", "pray for paul george", headline)]
raw_data[grepl("#starwarschristmascarols", headline), headline := gsub("#starwarschristmascarols", "star wars christmas carols", headline)]
raw_data[grepl("#trickortreatin100years", headline), headline := gsub("#trickortreatin100years", "trick or treat in 100 years", headline)]
raw_data[grepl("#notme", headline), headline := gsub("#notme", "not me", headline)]
raw_data[grepl("#trumphair", headline), headline := gsub("#trumphair", "trump hair", headline)]
raw_data[grepl("#womenboycotttwitter", headline), headline := gsub("#womenboycotttwitter", "women boycott twitter", headline)]
raw_data[grepl("#trumpbacktoschooltips", headline), headline := gsub("#trumpbacktoschooltips", "trump back to school tips", headline)]
raw_data[grepl("#alternativefacts", headline), headline := gsub("#alternativefacts", "alternative facts", headline)]
raw_data[grepl("#metoo", headline), headline := gsub("#metoo", "me too", headline)]
raw_data[grepl("#mewesyria", headline), headline := gsub("#mewesyria", "mewe syria", headline)]
raw_data[grepl("#addcandytoamovie", headline), headline := gsub("#addcandytoamovie", "add candy to a movie", headline)]
raw_data[grepl("#nevertrump", headline), headline := gsub("#nevertrump", "never trump", headline)]
raw_data[grepl("#squadgoals", headline), headline := gsub("#squadgoals", "squad goals", headline)]
raw_data[grepl("#gopsongsaboutethics", headline), headline := gsub("#gopsongsaboutethics", "gop songs about ethics", headline)]
raw_data[grepl("#badpicturemonday", headline), headline := gsub("#badpicturemonday", "bad picture monday", headline)]
raw_data[grepl("#teamlogan", headline), headline := gsub("#teamlogan", "team logan", headline)]
raw_data[grepl("#teamnocancer", headline), headline := gsub("#teamnocancer", "team no cancer", headline)]
raw_data[grepl("#obamaandkids", headline), headline := gsub("#obamaandkids", "obama and kids", headline)]
raw_data[grepl("#dropthecover", headline), headline := gsub("#dropthecover", "drop the cover", headline)]
raw_data[grepl("#hscc2015", headline), headline := gsub("#hscc2015", "", headline)]
raw_data[grepl("#napaquake", headline), headline := gsub("#napaquake", "napa quake", headline)]
raw_data[grepl("#nofilter", headline), headline := gsub("#nofilter", "no filter", headline)]
raw_data[grepl("#digitalhealth", headline), headline := gsub("#digitalhealth", "digital health", headline)]
raw_data[grepl("#napastrong", headline), headline := gsub("#napastrong", "napa strong", headline)]
raw_data[grepl("#alohahuffpost", headline), headline := gsub("#alohahuffpost", "aloha huff post", headline)]
raw_data[grepl("#surfbort", headline), headline := gsub("#surfbort", "surfbort", headline)]
raw_data[grepl("#4", headline), headline := gsub("#4", "number 4", headline)]
raw_data[grepl("#nobannowall", headline), headline := gsub("#nobannowall", "no ban no wall", headline)]
raw_data[grepl("#millionsmarchsf", headline), headline := gsub("#millionsmarchsf", "millions march sf", headline)]
raw_data[grepl("#emojisinthewild", headline), headline := gsub("#emojisinthewild", "emojis in the wild", headline)]
raw_data[grepl("#staywokeandvote", headline), headline := gsub("#staywokeandvote", "stay woke and vote", headline)]
raw_data[grepl("#oscarssowhite", headline), headline := gsub("#oscarssowhite", "oscars so white", headline)]
raw_data[grepl("#iphonefeatures4politicians", headline),
         headline := gsub("#iphonefeatures4politicians", "iphone features for politicians", headline)]
raw_data[grepl("#myroommateisweird", headline), headline := gsub("#myroommateisweird", "my roommate is weird", headline)]
raw_data[grepl("#youlookdisgusting", headline), headline := gsub("#youlookdisgusting", "you look disgusting", headline)]
raw_data[grepl("#nspw2017", headline), headline := gsub("#nspw2017", "", headline)]
raw_data[grepl("#ionceoverheard", headline), headline := gsub("#ionceoverheard", "i once overheard", headline)]
raw_data[grepl("#explainthe90sin4words", headline), headline := gsub("#explainthe90sin4words", "explain the 90s in 4 words", headline)]
raw_data[grepl("#dearbetsy", headline), headline := gsub("#dearbetsy", "dear betsy", headline)]
raw_data[grepl("#friendshipgoals", headline), headline := gsub("#friendshipgoals", "friendship goals", headline)]
raw_data[grepl("#missuniverse2015", headline), headline := gsub("#missuniverse2015", "miss universe 2015", headline)]
raw_data[grepl("#addclimatechangetotv", headline), headline := gsub("#addclimatechangetotv", "add climate change to tv", headline)]
raw_data[grepl("#5", headline), headline := gsub("#5", "number 5", headline)]
raw_data[grepl("#feelthebern", headline), headline := gsub("#feelthebern", "feel the bern", headline)]
raw_data[grepl("#trumpacandy", headline), headline := gsub("#trumpacandy", "trump a candy", headline)]
raw_data[grepl("#middleagedschmovies", headline), headline := gsub("#middleagedschmovies", "middle aged schmovies", headline)]
raw_data[grepl("#thereisaidit", headline), headline := gsub("#thereisaidit", "there i said it", headline)]
raw_data[grepl("#dadlife", headline), headline := gsub("#dadlife", "dad life", headline)]
raw_data[grepl("#meat14", headline), headline := gsub("#meat14", "me at 14", headline)]
raw_data[grepl("#thrive", headline), headline := gsub("#thrive", "thrive", headline)]
raw_data[grepl("#menforchoice", headline), headline := gsub("#menforchoice", "men for choice", headline)]
# end of cleaning # ----
# probably itll be additional feature in the momdel
raw_data[ , text_length := nchar(headline)]
# preparing tokens
tokenized_data <- tokens(x = raw_data$headline, what = 'word',
                         remove_numbers = TRUE,
                         remove_hyphens = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE)
tokenized_data <- tokens_tolower(tokenized_data)
tokenized_data <- tokens_select(tokenized_data, stop_words$word, 
                                selection = "remove")
tokenized_data <- tokens_wordstem(tokenized_data, language = "english")
# adding bigrams
tokenized_data <- tokens_ngrams(tokenized_data, n = 1:2)
# making tfidf
tfidf_data <- dfm(tokenized_data, tolower = FALSE)
rm(tokenized_data); gc()
tfidf_data <- dfm_tfidf(tfidf_data)
tfidf_data <- tfidf_data[, 2:ncol(tfidf_data)]

# preparing LSA & SVD
tic()
tfidf_irlba <- irlba(t(tfidf_data), nv = 500, maxit = 1000 ) 
toc()
svd_data <- data.frame(is_sarcastic = raw_data$is_sarcastic, tfidf_irlba$v)
svd_data$text_length <- raw_data$text_length
saveRDS(svd_data, "data/svd_data.rda")
rm(raw_data); gc()

# building classifier
set.seed(333)
indexes <- createDataPartition(svd_data$is_sarcastic, times = 1,
                               p = 0.7, list = FALSE)

train_data <- svd_data[indexes,]
test_data <- svd_data[-indexes,]


# Verify proportions.
prop.table(table(train_data$is_sarcastic))
prop.table(table(test_data$is_sarcastic))


h2o.init()
x <- names(train_data)
x <- x[!x %in% c("is_sarcastic")]
train_data <- as.h2o(train_data)

model <- h2o.automl(x = x,
                    y = "is_sarcastic",
                    training_frame = train_data,
                    nfolds = 5,
                    balance_classes = TRUE,
                    exclude_algos = c("DeepLearning", "StackedEnsemble"),
                    max_runtime_secs = 60*60*10)
model_leader <- model@leader
model_leader@algorithm # best model is glm
model_leader@model$model_summary
model_leader@model$training_metrics
model_leader@model$cross_validation_metrics 

# making prediction
test_data <- as.h2o(test_data)
prediction <- h2o.predict(model_leader, test_data) %>% 
  as.data.table()
prediction$actual <- as.vector(test_data[["is_sarcastic"]])
table(prediction[, .(actual, predict)])
c<-confusionMatrix(as.factor(prediction$actual), prediction$predict)
