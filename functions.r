################################################################################
# Extract hashtags from a character vector and return frequency table
################################################################################

getCommonHashtags <- function(text, n=30){
  text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",tolower(text)))
  hashtags <- unlist(hashtags)
  tab <- table(hashtags)
  return(head(sort(tab, dec=TRUE), n=n))
}


################################################################################
# Download tweets sent by a given user
################################################################################

#' @rdname getTimeline
#' @export
#'
#' @title 
#' Returns up to 3,200 recent tweets from a given user
#'
#' @description
#' \code{getTimeline} connects to the REST API of Twitter and returns up to
#' 3,200 recent tweets sent by these user.

#'
#' @param filename file where tweets will be stored (in json format)
#'
#' @param n number of tweets to be downloaded (maximum is 3,200)
#'
#' @param screen_name user name of the Twitter user for which his/her tweets
#' will be downloaded
#' 
#' @param id id of Twitter user for which his/her tweets will be downloaded
#' (Use either of these two arguments)
#'
#' @param oauth OAuth token
#'
#' @param since_id id of the oldest tweet to be downloaded. Useful if, for 
#' example, we're only interested in getting tweets sent after a certain
#' date.
#'
#' @param trim_user if "true", downloaded tweets will include user object
#' embedded. If "false", only tweet information will be downloaded.
#'
#' @param sleep numeric, number of seconds between API calls. Higher number
#' will increase reliability of API calls; lower number will increase speed.
#'
#' @examples \dontrun{
#' ## Download recent tweets by user "p_barbera"
#'  friends <- getTimeline(screen_name="p_barbera", oauth=my_oauth)
#' }
#'

getTimeline <- function(filename, n=3200, oauth, screen_name=NULL, 
                        id=NULL, since_id=NULL, trim_user="true", sleep=.5){
  
  require(rjson); require(ROAuth)
  
  ## while rate limit is 0, open a new one
  limit <- getLimitTimeline(my_oauth)
  cat(limit, " hits left\n")
  while (limit==0){
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(my_oauth)
    cat(limit, " hits left\n")
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
  
  ## first API call
  if (!is.null(screen_name)){
    params <- list(screen_name = screen_name, count=200, trim_user=trim_user)
  }
  if (!is.null(id)){
    params <- list(id=id, count=200, trim_user=trim_user)   
  }
  if (!is.null(since_id)){
    params[["since_id"]] <- since_id
  }
  
  url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
  Sys.sleep(sleep)
  ## one API call less
  limit <- limit - 1
  ## changing oauth token if we hit the limit
  cat(limit, " hits left\n")
  while (limit==0){
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(my_oauth)
    cat(limit, " hits left\n")
  }
  ## trying to parse JSON data
  ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
  json.data <- RJSONIO::fromJSON(url.data)
  if (length(json.data$error)!=0){
    cat(url.data)
    stop("error! Last cursor: ", cursor)
  }
  ## writing to disk
  conn <- file(filename, "a")
  invisible(lapply(json.data, function(x) writeLines(rjson::toJSON(x), con=conn)))
  close(conn)
  ## max_id
  tweets <- length(json.data)
  max_id <- json.data[[tweets]]$id_str
  cat(tweets, "tweets. Max id: ", max_id, "\n")
  max_id_old <- "none"
  if (is.null(since_id)) {since_id <- 1}
  
  while (tweets < n & max_id != max_id_old & 
         as.numeric(max_id) > as.numeric(since_id)){
    max_id_old <- max_id
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, count=200, max_id=max_id,
                     trim_user=trim_user)
    }
    if (!is.null(id)){
      params <- list(id=id, count=200, max_id=max_id, trim_user=trim_user)
    }
    if (!is.null(since_id)){
      params[['since_id']] <- since_id
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## changing oauth token if we hit the limit
    cat(limit, " hits left\n")
    while (limit==0){
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(my_oauth)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitTimeline(my_oauth)
      cat(limit, " hits left\n")
    }
    ## trying to parse JSON data
    ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
    json.data <- RJSONIO::fromJSON(url.data)
    if (length(json.data$error)!=0){
      cat(url.data)
      stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(filename, "a")
    invisible(lapply(json.data, function(x) writeLines(rjson::toJSON(x), con=conn)))
    close(conn)
    ## max_id
    tweets <- tweets + length(json.data)
    max_id <- json.data[[length(json.data)]]$id_str
    cat(tweets, "tweets. Max id: ", max_id, "\n")
  }
}


getLimitTimeline <- function(my_oauth){
  require(rjson); require(ROAuth)
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$statuses$`/statuses/user_timeline`[['remaining']]))
  
}


getLimitRate <- function(my_oauth){
  require(rjson); require(ROAuth)
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}


################################################################################
# Scrape NYT Congress API for information on Members of 114th Congress
################################################################################

scrape_nytimes_congress_api <- function(api_key, chamber){
  
  require(RCurl)
  require(RJSONIO)
  
  # query URL
  url <- paste0("http://api.nytimes.com/svc/politics/v3/us/legislative/",
                "congress/114/", chamber, "/members.json?",
                "api-key=", api_key)
  
  # downloading data
  data <- fromJSON(getURL(url))   
  
  # reading fields and transforming into data frame
  fields <- names(data[[3]][[1]]$members[[1]])
  df <- matrix(NA, nrow=length(data[[3]][[1]]$members), ncol=length(names(data[[3]][[1]]$members[[1]])))
  for (i in 1:length(fields)){
    df[,i] <- unlistWithNA(data[[3]][[1]]$members, fields[i])
  }   
  
  df <- data.frame(df, stringsAsFactors=F)
  names(df) <- fields 
  
  # adding extra field if senate
  if (chamber=="senate"){df$district <- NA}
  
  df$chamber <- chamber
  return(df)
}

unlistWithNA <- function(lst, field){
  notnulls <- unlist(lapply(lst, function(x) try(!is.null(x[[field]]), silent=TRUE)))
  notnulls[grep('Error', notnulls)] <- FALSE
  notnulls <- ifelse(notnulls=="TRUE", TRUE, FALSE)
  vect <- rep(NA, length(lst))
  vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
  return(vect)
}

#Custom function to clean tweet
clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}

classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words) 
  return(pos.matches - neg.matches)
}

classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}


getCount <- function(data,keyword)
{
  wcount <- str_count(data$text, keyword)
  return(data.frame(data,wcount))
}


#Custom function to  date format from "Sat Oct 01 23:19:02 +0000 2016" to "2016-10-01 23:19:02 EDT"
date_conversion <- function(x){ strptime(x,'%a %b %d %H:%M:%S +0000 %Y', tz = "")}

topicmodels_json_ldavis <- function(fitted, dfm, dtm){
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  
  doc_length <- ntoken(dfm[rownames(dtm)])
  
  temp_frequency <- inspect(dtm)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}


