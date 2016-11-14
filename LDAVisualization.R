# LDA Visualization

tweets1 <- parseTweets("user_tweets.json")
tweets_df <- subset(tweets_df, select = c("text", 
                                          "id_str", 
                                          "lang",
                                          "created_at",
                                          "listed_count",
                                          "location",
                                          "user_id_str",
                                          "description",
                                          "followers_count",
                                          "friends_count",
                                          "screen_name",
                                          "full_name",
                                          "place_name",
                                          "place_lat",
                                          "place_lon",
                                          "lat",
                                          "lon"))

tweets_df <- tweets_df[tweets_df$lang=="en",]
tweets_df$text <- sapply(tweets_df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

tweet_corpus <- corpus(tweets_df$text)
tweets_dfm <- dfm(tweet_corpus, ignoredFeatures=c(stopwords("english"),"just", "t.co", "https", "rt", "amp", "http", "t.c", "can","@"))
tweets_dfm <- trim(tweets_dfm, minDoc = 10)

tweets_dtm <- convert(tweets_dfm, to="topicmodels")
lda <- LDA(tweets_dtm, k = 20, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))
term <- terms(lda, 10)
colnames(term) <- paste("Topic",1:20)
term

library(LDAvis)
library(jsonlite)
source("functions.r")
json <- topicmodels_json_ldavis(lda,tweets_dfm,tweets_dtm)
new.order <- fromJSON(json)$topic.order


serVis(json, out.dir = 'ElectionLDA', open.browser = interactive())

