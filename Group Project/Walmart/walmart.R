library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")

Vib.key <-"8ZksBO6EuNQ41btJQj2HeiSXD"
Vib.secret <-"Qxkxwh6Va96ChMVnZya5nLE4uLq5sY1FsqXgg94WBXQK0l4gtm"

cred <- OAuthFactory$new(consumerKey=Vib.key,
                         consumerSecret=Vib.secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL= 'https://api.twitter.com/oauth/authorize')

save(cred, file="twitter_authentication.Rdata")

## load("twitter_authentication.Rdata")
cred$handshake()

## check that authorization was successful

registerTwitterOAuth(cred)


## tweets <- searchTwitter("@Walmart",n=200)

tweets <- searchTwitter("@Walmart",n=200, lang="en", since='2014-11-28', until='2014-11-29')


###############################################################################
##
##   STOP
##
###############################################################################

length(tweets)

###############################################################################
##
##   STOP
##
###############################################################################

## Now, we will extract the fields that we want

tweets.id <- sapply(tweets, function(x) x$getId())
tweets.text <- sapply(tweets, function(x) x$getText())
tweets.screenname <- sapply(tweets, function(x) x$getScreenName())
tweets.isretweet <- sapply(tweets, function(x) x$getIsRetweet())
tweets.retweeted <- sapply(tweets, function(x) x$getRetweeted())
tweets.created <- sapply(tweets, function(x) x$getCreated())

###############################################################################
##
##   STOP
##
###############################################################################

head(tweets.text)

## Write data to file

df <- data.frame(tweets.id, tweets.text, tweets.screenname, tweets.isretweet, tweets.retweeted, tweets.created)
names(df) <-c("id", "text", "screenname", "isretweet", "retweeted", "created")
write.table(df, file = "Boeing.txt", append = TRUE)

###############################################################################
##
##   STOP
##
###############################################################################
## these are the files from ReggieNet
##load opinion lexicon
##from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
## the load path is relative to the working directory that is set above
pos <- scan("positive-words.txt",what="character",comment.char=";")
neg <- scan("negative-words.txt",what="character",comment.char=";")

## create corpus
## these functions from the tm package
tweets.corpus <- Corpus(VectorSource(tweets.text))

###############################################################################
##
##   STOP
##
###############################################################################



## clean up
tweets.corpus <- tm_map(tweets.corpus, tolower) 
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, function(x) removeWords(x,stopwords()))

## split the tweets in the corpus up into individual words
## lapply iterates over each element in the corpus
## and applies the strsplit function
## the splitting argument is the 3rd in the lapply function
## and is splitting on white space.
corpus.split <- lapply(tweets.corpus,strsplit,"\\s+")

## find matches for the individual words in the two lexicons
## lapply again, x takes on an element in the corpus
## at each iteration
matches <- lapply(corpus.split,function(x) {
  match.pos <- match(x[[1]],pos)
  match.neg <- match(x[[1]],neg) 
  
  ## return the length of each match vector, non-na 
  list(length(which(!is.na(match.pos))),length(which(!is.na(match.neg))))
})


###############################################################################
##
##   STOP
##
###############################################################################

## turn the matches into a matrix
## one column for positive, one for negative
match.matrix <- matrix(unlist(matches),nrow=length(matches),ncol=2,byrow=T)

## calculate a simple sentiment score by substracting
## positive count from negative count
simple.sentiment <- match.matrix[,1] - match.matrix[,2]

## histogram of sentiment
hist(simple.sentiment)

###############################################################################
##
##   STOP
##
###############################################################################

sum(simple.sentiment)


