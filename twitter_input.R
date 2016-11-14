library(devtools)
library(twitteR)
library(ROAuth)
library(quanteda)
library(RColorBrewer)
library(psych) 
library(ggplot2)
library(stringr)
library(streamR)
library(quanteda)
library(glmnet)
library(doParallel)
library(RColorBrewer)
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(plotly)
library(LDAvis)
library(twitteR)
library(Rgraphviz)
source("functions.r")

#Setting seed to reproduce result
set.seed(123)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

twitter_consumer_key <- "J9o6Ab4eDffpPWBYFpgB0CYzv" 
twitter_consumet_secret <- "JXX8Xa6H2vo0wZEuZSBugVqmv12eHc0rPvBp0cOAjC3taOcFY0"
twitter_access_token <- "301280862-Y3sXqyHO2nb6hgFLSlv8L6jejZb2AkYptT2WSRRU"
twitter_access_secret <- "Xc0xXYWA1XUPgcDA9f6VWDapT0OUu2UluAwPA2XslkpkQ"

my_oauth <- OAuthFactory$new(consumerKey=twitter_consumer_key,
                             consumerSecret=twitter_consumet_secret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# 
save(my_oauth, file="oauth_token.Rdata")

setup_twitter_oauth(consumer_key = twitter_consumer_key,
                    consumer_secret = twitter_consumet_secret,
                    access_token = twitter_access_token,
                    access_secret = twitter_access_secret)

tweets_search <- searchTwitter(searchString=c("election2016", "Election2016","Hillary", "hillary","trump",
"Trump","donald","Donald","Bernie","bernie","Ted","cruze","republican","democrats"), 
n = 800000, lang = 'en' ,geocode = '39.872835,-101.320600, 1600mi', retryOnRateLimit = 20, since = '2016-09-24')


# 
hillary <- userTimeline("HillaryClinton")
# hillary
# donald <- userTimeline("realDonaldTrump")
# 
filterStream(file.name="user_tweets1.json",
             track=c("election2016", "Election2016","Hillary", "hillary","trump",
                     "Trump","donald","Donald","Bernie","bernie","Ted","cruze","republican","democrats"),
             locations = c(-125, 25, -66, 50), timeout=1500, oauth=my_oauth)

#tweets dataframe has all the extracted tweets with colum name
#[1] "text"                      "retweet_count"             "favorite_count"            "favorited"                 "truncated"                 "id_str"                   
#[7] "in_reply_to_screen_name"   "source"                    "retweeted"                 "created_at"                "in_reply_to_status_id_str" "in_reply_to_user_id_str"  
#[13] "lang"                      "listed_count"              "verified"                  "location"                  "user_id_str"               "description"              
#[19] "geo_enabled"               "user_created_at"           "statuses_count"            "followers_count"           "favourites_count"          "protected"                
#[25] "user_url"                  "name"                      "time_zone"                 "user_lang"                 "utc_offset"                "friends_count"            
#[31] "screen_name"               "country_code"              "country"                   "place_type"                "full_name"                 "place_name"               
#[37] "place_id"                  "place_lat"                 "place_lon"                 "lat"                       "lon"                       "expanded_url"             
#[43] "url"

#ParseTweet converts json into a dataframe
tweets <- parseTweets("user_tweets.json")
tweets1 <- parseTweets("user_tweets.json")
#Using tweets_df for all data manipulation as the original need not be modified.
tweets_df <- tweets1
#Selecting only a subset of the data [Removing unwanted terms]
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

#removing tweets which are not english
tweets_df <- tweets_df[tweets_df$lang=="en",]


#getting tweets that has location parameter
tweets_df_geo <-  tweets_df[!is.na(tweets_df$lon),]

map.data <- map_data("states")

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white",
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  # 2) limits for x and y axis
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  # 3) adding the dot for each tweet
  geom_point(data = tweets_df_geo, 
             aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "black", tex) +
  # 4) removing unnecessary graph elements
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())


tweets_df_geo <- read.csv("dataset_final.csv", header = TRUE)

#Plotting Map using Plotly 

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray"),
  countrywidth = 1,
  subunitwidth = 1
)

#Removing tweets that are irrelevant
tweets_df_geo <- tweets_df_geo[tweets_df_geo$related_to !="Neutral" & tweets_df_geo$related_to!="None", ]


plot_geo(tweets_df_geo, lat = tweets_df_geo$lat, lon = tweets_df_geo$lon) %>%
  add_markers(    text = ~paste(tweets_df_geo$related_to,tweets_df_geo$sentiment, tweets_df_geo$place_name, sep = "<br />"),
      color= tweets_df_geo$related_to,colors=c('red','blue'), symbol = I("circle"), size = I(8), hoverinfo = "text"
  ) %>%
  layout(
    title = 'Tweets by location<br />(Hover for more information)', geo = g
  )
        
        
#Finding Relationship between terms
freq.terms <- findFreqTerm(tweets_dtm, lowfreq = 200)
plot(tweets_dtm, freq.terms[1:20], corThreshold =0.1, weighting=T)


#getting tweet location by count 
states <- map.where(database="state", x=tweets_df_geo$lon, y=tweets_df_geo$lat)
head(sort(table(states), decreasing=TRUE))

#Custom function to  date format from "Sat Oct 01 23:19:02 +0000 2016" to "2016-10-01 23:19:02 EDT"
date_conversion <- function(x){ strptime(x,'%a %b %d %H:%M:%S +0000 %Y', tz = "")}

#applying the converted list to dataset
tweets_df$created_at <- date_conversion(tweets_df$created_at)


#aggregrating tweets by location 
agg_tweets <- tweets_df %>% group_by(location) %>% summarise(Count = n()) %>% arrange(desc(Count))
agg_tweets[-c(1:3),]

#Converting tweet body from ascii to latin1. (To remove non ascii characters such as  '¼í½', emoticons etc)
tweets_df_geo$text <- sapply(tweets_df_geo$text, function(row) iconv(row, "latin1", "ASCII", sub=""))

#Checking who the tweet is related to..
related_to <- data.frame(related_to=character(),stringsAsFactors = FALSE)
for(i in 1:nrow(tweets_df_geo)) {
  row <- tweets_df_geo[i, ]
  trump_count <- 0
  hillary_count  <- 0
  trump_count <- str_count(tolower(row$text), "trump")
  hillary_count <- str_count(tolower(row$text), "hillary")
  if (trump_count == 0 && hillary_count == 0) {
    related_to[i, 1] <- "None"
  }  else if (trump_count > hillary_count) {
    related_to[i, 1] <- "Trump"
  } else if (hillary_count > trump_count) {
    related_to[i, 1] <- "Hillary"
  } else if (trump_count == hillary_count) {
    related_to[i, 1] <- "Neutral"
  }
}

#Adding the relavance to the dataframe
tweets_df_geo1 <- tweets_df_geo
tweets_df_geo$related_to <- related_to[,1]


#Sentiment Analysis using 'Bag of Words' approach

#Initial data cleaning 
tweets_df_geo$text <- gsub('@[0-9_A-Za-z]+', '@', tweets_df_geo$text)

#getting the bag of words
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)

#Seperating the positive and negative words
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

text <- clean_tweets(tweets_df$text)
classifier(text, pos.words, neg.words)


#Getting corpus of input using corpus function in quantada package
tweet_corpus <- corpus(tweets_df_geo$text)
mydict <- dictionary(list(negative = neg.words,positive = pos.words))

#Getting the document frequency matrix
sent <- dfm(tweet_corpus, dictionary = mydict)

tweets_df_geo$score <- as.numeric(sent[,2]) - as.numeric(sent[,1])

#Meanest tweet
tweets_df_geo[which.min(tweets_df_geo$score),]

#Happiest tweet
tweets_df_geo[which.max(tweets_df_geo$score),]


tweets_df_geo$sentiment <- "neutral"
tweets_df_geo$sentiment[tweets_df_geo$score<0] <- "negative"
tweets_df_geo$sentiment[tweets_df_geo$score>0] <- "positive"


#Sentiment Analysis using machine learning
tweets_subset <- subset(tweets_df, related_to %in% c("Trump","Hillary"))
tweets_subset$trump <- ifelse(tweets_subset$related_to=="Trump", 0, 1)

tweets_df$text <- gsub('@[0-9_A-Za-z]+', '@', tweets_df$text)
tweet_corpus <- corpus(tweets_df$text)
#Creating a dfm from a corpus ...
tweets_dfm <- dfm(tweet_corpus, ignoredFeatures=c(stopwords("english"),"just", "t.co", "https", "rt", "amp", "http", "t.c", "can","@"))

#Removing features occurring in fewer than 10 documents: 64490
tweets_dfm <- trim(tweets_dfm, minDoc = 10)


tweets_training <- sample(1:nrow(tweets_subset), floor(.80 * nrow(tweets_subset)))
tweets_test <- (1:nrow(tweets_subset))[1:nrow(tweets_subset) %in% tweets_training == FALSE]

registerDoSEQ()

ridge <- cv.glmnet(tweets_dfm[tweets_training,], tweets_subset$trump[tweets_training], 
                   family="binomial", alpha=0, nfolds=5, parallel=TRUE,
                   type.measure="deviance")
plot(ridge)



accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}
accuracy(preds, tweets_subset$trump[tweets_test])

# function to compute precision
precision <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[1,2])/(tab[1,1]+tab[1,2]))
}
precision(preds, tweets_subset$trump[tweets_test])

# function to compute recall
recall <- function(ypred, y){
  tab <- table(ypred, y)
  return(tab[1,2]/(tab[1,1]+tab[1,2]))
}
recall(preds, tweets_subset$trump[tweets_test])


# computing predicted values
preds <- predict(ridge, tweets_dfm[tweets_test,], type="response") > mean(tweets_subset$trump[tweets_test])
# confusion matrix
table(preds, tweets_subset$trump[tweets_test])
# performance metrics



best.lambda <- which(ridge$lambda==ridge$lambda.min)
beta <- ridge$glmnet.fit$beta[,best.lambda]
head(beta)

## identifying predictive features
df <- data.frame(coef = as.numeric(beta),
                 word = names(beta), stringsAsFactors=F)

df <- df[order(df$coef),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")
df <- df[order(df$coef, decreasing=TRUE),]
head(df[,c("coef", "word")], n=30)
paste(df$word[1:30], collapse=", ")


#Topic Modelling 


plot(tweets_dfm, scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
     random.order = F, rot.per=0.1, max.words=250)
hashtags <- getCommonHashtags(tolower(tweets_df$text))
hashtags

similarity(tweets_dfm, c("liar", "health", "terror"), method = "cosine", margin = "features", n = 20)


#

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


serVis(json, out.dir = 'ElectionLDA', open.browser = interective())

#Word cloud
plot(tweets_dfm, scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
     random.order = F, rot.per=0.1, max.words=100)

#word cloud with tf-idf
plot(tfidf(tweets_dfm), scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
     random.order = F, rot.per=0.1, max.words=100)