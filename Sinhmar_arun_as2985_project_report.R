
#title: "Data Wrangling Final Project"
#author: "Arun Sinhmar"
#date: "5/10/2019"




# Installing the required packages
# TwitteR package includes set of text analytics functions. 
#In this project, this library is primarily used to provide interface to Twitter web API
library(twitteR)

# TM package is a framework of text mining applications which provides variety of functions to process raw tweets 
# into high quality information from text
library(tm)

# HTTR package is a collection of tools for working with URLs and HTTP while accessing online data
library(httr)

# This package is used to create wordclouds
library(wordcloud)

# The stringr package provides string functions for string processing
library(stringr)

# PLYR package is used for split-apply-combine (SAC) procedures i.e.applying functions over grouped data
library(plyr)




##Part A: Access tweets using twitter API

# Linking file containing Twitter API's keys
source("API-keys_sinhmar.R")

# Setting up connection to Twitter search API for downloading tweets. 
# The authentication setup fucniton (written below) uses consumer keys and access tokens
# setup_twitter_oauth function is part of {twitteR} library and uses Oauth authentication
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) 




##Part B: Preprocess raw text data and make it suitable for data mining/mathematical analysis

# function TWEET will scrap tweets based on the 'searchstring' and clean them for further use. 
#Using this function tweets can be dynamically retreived and processed. 
#This helps us avoid writng the same piece of code multiple times for each 'searchstring'
tweet=function(searchstring,nt)
{
  # function SearchTwitter searches for all tweets matching with search string provided
  tweets = searchTwitter(searchstring, n=nt,lang="en")
  
  # This step takes a list of tweets returns a data.frame version of the tweets
  tweets_dataframe = twListToDF(tweets)
  
  #a. remove special characters and retain alpha numeric, '//','(quote) and '@' from tweets text
  # using gsub function we can replace all occurance of first argument with the second argument in the tweets
  tweets_data = gsub("[^0-9A-Za-z@///' ]", "", tweets_dataframe$text)
  
  #b. while some of the text is from re-tweets, we would certainly like to get rid of 
  # name of the person whose post was re-tweeted. This will remove word 'RT' and the name of person
  # w=mentioned after RT
  tweets_data = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",tweets_data)
  
  #c. The other form of noise in twitter data is names of people between the text.
  # The step below removes name of people by matching anything that starts with @ symbol as name
  tweets_data = gsub("@\\w+", "", tweets_data)
  
  #d. Since the purpose of sentiment analysis is to analyze words and not numbers 
  # we can remove the numeric digits between the text using function gsub
  tweets_data = gsub("[[:digit:]]", "", tweets_data)
  
  #e. Data obtained from internet usually contains a lot of html entities and links;which gets embedded in the raw  data. 
  # It is important to get rid of these entities as a part of data sanitization process. 
  # The step below matches http links and removes them
  tweets_data = gsub('http\\S+\\s*',"", tweets_data)
  
  #f. I noticed that there are quite a few words with quotations.
  # The step below removes quotation marks
  tweets_data = gsub('"', "", tweets_data)  
  tweets_data <- gsub("'", '', tweets_data)
  
  # Following function converts dataframe to corpus
  tweets_corpus = Corpus(VectorSource(tweets_data))
  
  # To create a consistent pattern we will convert all words in text to lowercase. for example,frequency functions 
  #otherwise would count same word in different case as two different words
  tweets_corpus = tm_map(tweets_corpus, tolower)
  
  # create a plain text document using content in corpus
  tweets_corpus = tm_map(tweets_corpus, PlainTextDocument)
  
  # All the punctuation marks between text should be removed
  tweets_corpus = tm_map(tweets_corpus, removePunctuation)
  
  # While carrying out text analysis driven at the word level, it is required to remove the commonly occurring words 
  # also called stop-words. One approach is to create a create a long list of stop-words and 
  # other is too use a predefined language specific libraries
  # I am applying both approaches: 
  #(i) use common stop words for english language 
  tweets_corpus = tm_map(tweets_corpus, removeWords, stopwords("english"))
  
  # Based on my observation of data, I will create a customized list to be applied on top of the text cleaned by using 
  # predefined stop words library
  #(ii) list of words that will not insightful in wordcloud
  st = c("amp","you'll","you'd","she","yet","i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", 
         "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", 
         "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these",
         "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", 
         "did", "doing", "would","should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're",
         "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll",
         "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", 
         "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's",
         "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and",
         "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", 
         "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", 
         "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why",
         "how", "all", "any", "both", "each", "few", "more", "most", "other",  "some", "such", "no", "nor", "not", "only",
         "own", "same", "so", "than", "too", "very","'","\"", "calumni","compani","speak")
  
  tweets_corpus = tm_map(tweets_corpus, removeWords, st) 
  
  # word stemming means to reduce word to its root form i.e. to remove any tense information or to convert derived words
  # to their root form. TM library functions allows us to stem the words and thus identify words with same meaning which
  # might otherwise look different to machine
  tweets_corpus = tm_map(tweets_corpus,stemDocument)
  
  return(tweets_corpus)
}


# Here, I am extracting data using 'tweet' function and saving the clean corpus of tweets for four keywords:
#Name of 2 main political parties – ‘BJP’ and ‘Congress’
#Name of their respective Prime Minister candidates – ‘Narendra Modi’ and ‘Rahul Gandhi’

# We can increase and decrease the number of tweets scrapped by changing the second argument of 'tweet' function.
# Please note, we can only scrap a maximum of ~3200 tweets for one user at a single instance
namo=tweet("narendramodi",2000)  
papu=tweet("RahulGandhi",2000)
congress=tweet("INCIndia",2000) 
bjp=tweet("BJP4India",2000) 


# Here, I am using ‘Document Term Matrix’ and ‘Data Frame’ for creating a table with unique words (extracted from tweet) 
# and their frequency
namo = Corpus(VectorSource(namo)) 
DTM_namo = DocumentTermMatrix(namo)
bjp = Corpus(VectorSource(bjp)) 
DTM_bjp = DocumentTermMatrix(bjp)
papu = Corpus(VectorSource(papu)) 
DTM_papu = DocumentTermMatrix(papu)
congress = Corpus(VectorSource(congress)) 
DTM_congress = DocumentTermMatrix(congress)


# I am calcualting sum of columns because that will give me number of times each word has been repeated in all the 
# tweets collected for each 'keyword'
freq_namo = colSums(as.matrix(DTM_namo))
freq_bjp = colSums(as.matrix(DTM_bjp))
freq_papu = colSums(as.matrix(DTM_papu))
freq_congress = colSums(as.matrix(DTM_congress))


# Here, I am creating a data frame with two columns: word(extracted from tweet)  and frequency
DTM_namo = data.frame(word=names(freq_namo), freq=freq_namo)
DTM_bjp = data.frame(word=names(freq_bjp), freq=freq_bjp)
DTM_papu = data.frame(word=names(freq_papu), freq=freq_papu)
DTM_congress = data.frame(word=names(freq_congress), freq=freq_congress)

# Getting rid of junk value in first row of dataframe
DTM_namo = DTM_namo[-1,]
DTM_bjp = DTM_bjp[-1,]
DTM_papu = DTM_papu[-1,]
DTM_congress = DTM_congress[-1,]

# Here, I am plotting barplots and wordclouds of the processed final set of words
# Barplots and Wordcloud can help us understand which words are most frequenctly associated with different parties 
# and Prime Miniter candidates

# Minimum frequency can be entered here: what is the minimum count of words, we are plotting using barplots or wordcloud
# We can change the frequency of counts here, but we should be realistic while changing the count
# because it tough to find high frequency words in a corpus of tweets unless they are about a very niche event

# for example I want to plot only words which occur more than 35 times
min_freq = 75

# we can use par option to compare four plots at a time
opar = par()
par(mfrow=c(2,2))


# Here, I am plotting barplots of the processed final set of words, which can help us understand 
# which words are most frequenctly associated with different parties and Prime Miniter candidates
barplot(DTM_namo$freq[DTM_namo$freq>min_freq],names.arg=DTM_namo$word[DTM_namo$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_bjp$freq[DTM_bjp$freq>min_freq],names.arg=DTM_bjp$word[DTM_bjp$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_papu$freq[DTM_papu$freq>min_freq],names.arg=DTM_papu$word[DTM_papu$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_congress$freq[DTM_congress$freq>min_freq],names.arg=DTM_congress$word[DTM_congress$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)



# we can use par option to compare four plots at a time
opar = par()
par(mfrow=c(1,1))

# Here, I am plotting wordcloud of the processed final set of words, which can help us understand 
# which words are most frequenctly associated with different parties and Prime Miniter candidates
wordcloud(words = DTM_namo$word, freq = DTM_namo$freq, min.freq = 50,max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = DTM_bjp$word, freq = DTM_bjp$freq, min.freq = 50,max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = DTM_papu$word, freq = DTM_papu$freq, min.freq = 50,max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(words = DTM_congress$word, freq = DTM_congress$freq, min.freq = 50,max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))





## Part C: Assign polarities to words in each tweet and calculate polarity scores
# For this exercise I have used ‘Bag of Words’ approach to identify polarity associated. 
# This approach compares text against a large collection of negative and positive words databases and 
# assigns polarities to them.
# Polarities are denoted by +1 for positive words and -1 for negative word in the text. 
# Polarity score is calculated for each tweet as sum of polarities.


# Place the given Negative and Positive words files in the working directory, which will be used in the polarity score function
# file with positive words 
positive = scan('positive-words.txt', what='character', comment.char=';')

#file with negative words
negative = scan('negative-words.txt', what='character', comment.char=';') 


# Following function is designed to calculate polarity score.
# for example if a tweet has 2 positive words, 3 negative words and 5 neutral words.
# The polarity assigned to that tweet will be: 1,1,-1,-1,-1,0,0,0,0,0 and the polarity score will be sum of polarites = -1

polarity_score = function(tweets, positive, negative)
{
  score =laply(tweets, function(tweet, positive, negative)
  {
    # before matching words in each tweet we will need to separate multiple attached words
    # for example, "brokelaptop" to "broke laptop"
    words = str_split(tweet,"\\s+")
    
    # unlist function simplifies a list structure i.e. from a sentence into vector which contains
    # all individual components present in tweets
    words = unlist(words)
    
    #function matches the words with positive and negative databases and calculates score 
    positive_overlap = match(words, positive)
    negative_overlap= match(words, negative)
    positive_overlap =!is.na(positive_overlap)
    negative_overlap= !is.na(negative_overlap)
    score =sum(positive_overlap) - sum(negative_overlap)
    
    return(score)
  }, positive, negative)
  scores =data.frame(score=score, text=tweets)
  return(scores)
}


# scrapping data and using 'laply' for each element of a tweet list, to get 'text' of the tweet back
# this text will be passed through polarity score function to match each word of tweet with 
# database of positive and negative words
tweets_namo = searchTwitter("narendramodi", n=2000,lang="en")
Tweets.text_namo = laply(tweets_namo,function(t)t$getText())

tweets_papu = searchTwitter("RahulGandhi", n=2000,lang="en")
Tweets.text_papu = laply(tweets_papu,function(t)t$getText())

tweets_congress = searchTwitter("INCIndia", n=2000,lang="en")
Tweets.text_congress = laply(tweets_congress,function(t)t$getText())

tweets_bjp = searchTwitter("BJP4India", n=2000,lang="en")
Tweets.text_bjp = laply(tweets_bjp,function(t)t$getText())


# calling polarity score function to calcualte polarity score of each tweet by matching text 
# with databsae of positive and negative words
Sentiment_scores_namo = polarity_score(Tweets.text_namo, positive, negative)
Sentiment_scores_bjp = polarity_score(Tweets.text_bjp, positive, negative)
Sentiment_scores_papu = polarity_score(Tweets.text_papu, positive, negative)
Sentiment_scores_congress = polarity_score(Tweets.text_congress, positive, negative)


# Here, I am labelling polarity scores into 3 categories:
# 1. positive
# 2. negative
# 3. neutral
# A score less than zero means a negative polarity and a score greater than zero means positive polarity

# I will create a polarity column which will have one of these 3 values:negative, positive or neutral
Sentiment_scores_namo$polarity=ifelse(Sentiment_scores_namo$score>0,"positive", ifelse(Sentiment_scores_namo$score<0 , "negative", "neutral"))
Sentiment_scores_bjp$polarity=ifelse(Sentiment_scores_bjp$score>0,"positive", ifelse(Sentiment_scores_bjp$score<0 , "negative", "neutral"))
Sentiment_scores_papu$polarity=ifelse(Sentiment_scores_papu$score>0,"positive", ifelse(Sentiment_scores_papu$score<0 , "negative", "neutral"))
Sentiment_scores_congress$polarity=ifelse(Sentiment_scores_congress$score>0,"positive", ifelse(Sentiment_scores_congress$score<0 , "negative", "neutral"))


# Exporting polarity score databases
write.csv(Sentiment_scores_namo, "modi_polarityscore.csv")
write.csv(Sentiment_scores_bjp, "bjp_polarityscore.csv")
write.csv(Sentiment_scores_papu, "gandhi_polarityscore.csv")
write.csv(Sentiment_scores_congress, "congress_polarityscore.csv")

# Here, I am preparing a table with frequency of negative, neutral and positive tweets
# I have used 'table' function to give the summary data and function 't' to transpose
x= data.frame(table(Sentiment_scores_namo$polarity),table(Sentiment_scores_bjp$polarity),table(Sentiment_scores_papu$polarity),table(Sentiment_scores_congress$polarity))
x = x[,c(2,4,6,8)]
colnames(x)<-c( "Modi","BJP","Gandhi","Congress")
x = t(x)  
colnames(x)<-c("Negative","Neutral", "Positive")


# I am plotting bar plots to compare polarities of:
# 2 main political parties – ‘BJP’ and ‘Congress’
# Prime Minister candidates – ‘Narendra Modi’ and ‘Rahul Gandhi’

# we can use par option to compare four plots at a time
opar = par()
par(mfrow=c(1,1))
colors = c("red","green","yellow","blue") 

# I have limited the yaxis based on the number of tweets downloaded, so that the legend don't fall on the bars
barplot(x,beside=T, col=colors,ylim=c(0,1500),ylab="Number of tweets",main="Polarity Comparsion")
legend("top",legend=rownames(x),horiz = TRUE,fill=colors,pt.cex=1, cex=0.8)



