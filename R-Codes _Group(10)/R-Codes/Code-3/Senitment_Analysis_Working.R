library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(openssl)
library(httpuv)

setup_twitter_oauth(consumer_key='CKDqbtF6F65ly1FthaMaaWRME',
                    consumer_secret='3vV1Yupe51POFVEdbxJQU8AGHg91pstpOxg2tdDn605VmsOp8A',
                    access_token = NULL,
                    access_secret = NULL)

tweets = searchTwitter("#SachinTendulkar", n=2000)
length(tweets)

Tweets.text = laply(tweets,function(t)t$getText())
Tweets.text
pos = scan('C:\\Users\\Varsha Holennavar\\Desktop\\sentiment analysis\\R-Codes\\Code-1\\positive-words.txt', what='character', comment.char=';')
neg = scan('C:\\Users\\Varsha Holennavar\\Desktop\\sentiment analysis\\R-Codes\\Code-1\\negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score = sum(pos.matches) - sum(neg.matches)
    #score <- rbind(pos.matches,neg.matches)
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}
analysis = score.sentiment(Tweets.text, pos, neg)
table(analysis$score)
hist(analysis$score)

#Histogram - iphone7
hist(analysis$score,xlab="Twitter Tweets",main="Analysis of Tweets",col="darkblue",xlim=c(-2,4),ylim=c(0, 2000))
#Histogram - Sachin Tendulkar
hist(analysis$score,xlab="Twitter Tweets",main="Analysis of Tweets",col="darkblue",xlim=c(-2,4),ylim=c(0, 190))