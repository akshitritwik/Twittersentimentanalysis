library (httr)
myapp <- oauth_app("twitter", 
                   key = "9bPvk1vChHApJ3MR32ks9gspI", 
                   secret ="mBB6xU13nWrHDGBuYoXeYabj2Ks1tIIm8y3CM5YY4nceHV9Qcw")
sig <-  sign_oauth1.0(myapp,
                      token = "2497963772-FsDMKL3HPWvLnwvQzNthnZDeJC4WYCEbnt65FxF",
                      token_secret = "ASfBfYOIByy73Mbn2m73BVN9FVs0WdcUzwLcEupVoglnO")
homeTL <- GET ("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
json1 <- content(homeTL)
library(jsonlite)
json2 <-fromJSON(toJSON(json1))
a <- json2[1:5, "text"]
a
FollowerList <- GET ("https://api.twitter.com/1.1/followers/list.json",sig)
json3 <- content(FollowerList)
json4 <- jsonlite::fromJSON(toJSON(json3))
names(df)
df <- as.data.frame(json4) 

df[, "users.followers_count"]

df [1:5,1:5]
library(twitteR)
consumerKey <- "9bPvk1vChHApJ3MR32ks9gspI"
consumerSecret <- "mBB6xU13nWrHDGBuYoXeYabj2Ks1tIIm8y3CM5YY4nceHV9Qcw"
accessToken <- "2497963772-FsDMKL3HPWvLnwvQzNthnZDeJC4WYCEbnt65FxF"
accessTokenSecret <- "ASfBfYOIByy73Mbn2m73BVN9FVs0WdcUzwLcEupVoglnO"
setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessTokenSecret)
tweets_geolocated <- searchTwitter("#Demonetization", n=200, lang="en", 
                                   geocode="20.3588240,85.8332660,100mi",
                                   since="2016-11-08")
tweets_geolocated.df <- twListToDF(tweets_geolocated)
names(tweets_geolocated.df)
tweets_geolocated.df[1:5,c(1,5,8,10)]
tweets <- searchTwitter("#Demonetization", n=400, lang = "en") # top 300 tweets that contain 
tweet_txt = lapply(tweets, function(x) x$getText())
head(tweet_txt)
avg<-function(x)
{
  avg<-sum(x)/length(x)
  return(avg)
}
a<-c(1,5)
avg(a)
tweet_txt1<-twListToDF(tweets)
tweet_txt2<-tweet_txt1[,"text"]
head(tweet_txt2)
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  some_txt <- strsplit(some_txt," ")
  return(some_txt)
}
tweet_clean<-clean.text(tweet_txt)
head(tweet_clean,5)
head(tweet_txt)
a<-list(a=1:6,b=rnorm(100),c=rnorm(10,2,2))
b<-lapply(a,mean)
b

a<-list(b=matrix(1:20,5,4))
a<-as.data.frame(a)
dim(a)
apply(a,2,mean)

positive<-scan(file.choose(),what='character',comment.char = ';')
head(positive)
negative<-scan(file.choose(),what='character',comment.char = ';')
positive[20:30]
negative[500:510]
positive <- c(positive,"cloud")
negative <- negative[negative!="cloud"]
returnpscore <- function(t) {
  pos.match <- match(t,positive)
  pos.match <- !is.na(pos.match)
  pos.score <- sum(pos.match)
  return(pos.score)
}
positive.score <- lapply(tweet_clean,returnpscore)
head(positive.score)
pcount=0;
for(i in 1:length(positive.score))
{
  pcount<-pcount+positive.score[[i]]
}
pcount
returnnscore=function(twet) {
  neg.match=match(twet,negative)
  neg.match=!is.na(neg.match)
  neg.score=sum(neg.match)
  return(neg.score)
}

negative.score <- lapply(tweet_clean,returnnscore)
head(negative.score)
ncount=0;
for(i in 1:length(negative.score))
{
  ncount<-ncount+negative.score[[i]]
}
ncount

poswords <- function(t){
  pmatch <- match(t,positive)
  posw <- positive[pmatch]
  posw <- posw[!is.na(posw)]
  return(posw)
}
negwords=function(t){
  nmatch=match(t,negative)
  negw=negative[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}
words<-NULL
pdatamart<-data.frame(words)
for(t in tweet_clean)
{
  pdatamart=c(pdatamart,poswords(t))
}
head(pdatamart,10)

ndatamart<-data.frame(words)
for(t in tweet_clean)
{
  ndatamart=c(ndatamart,negwords(t))
}
head(ndatamart,10)
library(dplyr)
head(airquality)
dim(airquality)
str(airquality)
names(airquality)
colnames(airquality)
head(select(airquality,Ozone,Day,Month),4)
head(airquality[,c("Month","Day","Ozone")],4)
head(select(airquality,contains("o")),4)
filter(airquality,Day<5,Solar.R>=200)
filter(airquality,Day<5&Solar.R>=200)
filter(airquality,Day %in%c(1,2,3))
#mutate
head(mutate(airquality,Temp_celcius=(Temp-32)*5/9,Temp=NULL))
str(airquality)
head(arrange(airquality,desc(Temp)))

library(tm)

tweetscorpus <- Corpus(VectorSource(tweet_clean))
inspect(tweetscorpus[1:4])
tweetscorpus <- tm_map(tweetscorpus,removeWords,stopwords("english"))

library(wordcloud)                    
wordcloud(tweetscorpus,
          random.order = F,rot.per = 0.20,
          colors = brewer.pal(8,"Accent"),
          max.words = 100)
dtm <- DocumentTermMatrix(tweetscorpus)

#removing sparse terms
dtms<- removeSparseTerms(dtm,.99)

freq<- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
#get some more frequent terms
findFreqTerms(dtm,lowfreq=20)

wf <- data.frame(word=names(freq),freq=freq)
wfh=wf%>%
  filter(freq>=20,!word==tolower("Demonetization"))
library(ggpl
ggplot(wfh,aes(word,freq))+
  geom_bar(stat="identity",fill='lightblue')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(aes(word,freq,label=freq),size=4)+
  labs(x="High Frequency Words ",y="Number of Occurences", 
       title=paste("High Frequency Words and Occurence in \n '","Demonetization","' twitter feeds, n =",300))+
  geom_text(aes(1,max(freq)-20,
                label=paste(" Positive Words:",pcount,"\n","Negative Words:",ncount,"\n",
                            "The Sentiment for Demonetization is positive in the ratio of",
                            round(pcount/ncount,2),":", 1)),size=5, hjust=0)





