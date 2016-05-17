# TwiiterR
R과 Twitter 연동


install.packages("base64enc")
install.packages(c("RCurl","twitterR","ROAuth"))
install.packages("Rcpp")
install.packages("wordcloud")
install.packages("plyr")
install.packages("tm")

library(base64enc)
library(RCurl)
library(twitteR)
library(ROAuth)
library(rJava)
library(KoNLP)
library(wordcloud)
library(plyr)
library(tm)
library(RColorBrewer)

reqURL <- "http://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

#인증정보

consumerKey <- "xd7PQqbcxiqwinYT5LWVh2Wo6"
consumerSecret <- "FK6hKcwQ5KP1xUdvtGUuZZsEzGdxllmkqIwEbRrLZVbm3rNUzk"
accesstoken <- "276564292-PlxV2q0mpah256Jxkg9udQTgPcLd2PKonDQ4xGsN" 	
accesstokensecret <- "jmTe0xYDdSZMa9KBxJzrmD5k0CHVEywVPQlQph3QoisPt"

options(RCurlOptions = list(cainfo = system.file("CurlSSL","cacert.pem",package="RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)

keyword <- "#카스"

search.tweets<-searchTwitter(enc2utf8(keyword),n=100,since='2016-01-01',lang="ko")


result.df <- twListToDF(search.tweets)
result.text <- result.df$text

result.text <- gsub("\n", "", result.text)
result.text <- gsub("\r", "", result.text)
result.text <- gsub("RT", "", result.text)
result.text <- gsub("http", "", result.text)
result.text <- gsub("CO", "", result.text)
result.text <- gsub("co", "", result.text)
result.text <- gsub("ㅋㅋ", "", result.text)
result.text <- gsub("ㅋㅋㅋ", "", result.text)
result.text <- gsub("ㅋ", "", result.text)
result.text <- gsub("ㅠㅠ", "", result.text)
result.text <- gsub("!", "", result.text)



# 문자 분리
result_nouns <- Map(extractNoun, result.text)

 

# 쓸모없는 문자들을 제거. 특히 영문자의 경우 tm의  stopwords를 활용
result_wordsvec <- unlist(result_nouns, use.name=F)
result_wordsvec <- result_wordsvec[-which(result_wordsvec %in% stopwords("english"))]
result_wordsvec <- gsub("[[:punct:]]","", result_wordsvec)
result_wordsvec <- Filter(function(x){nchar(x)>=2}, result_wordsvec)

 

# 문자 카운팅
result_wordcount <- table(result_wordsvec)

# 컬러 세팅
pal <- brewer.pal(12,"Paired")

 

# 폰트 세팅. 띄어쓰기나 대소문자에 민감하다는 점에 주의
# 맑은고딕 : windowsFonts(malgun=windowsFont("맑은 고딕"))
# 나눔고딕 : windowsFonts(malgun=windowsFont("나눔고딕"))
windowsFonts(malgun=windowsFont("Arial"))

 

# 그리기 - min.freq를 너무 크게 설정하면 남겨지는 단어가 대폭 줄어들게 됨. 조절 필요 
wordcloud(names(result_wordcount), freq=result_wordcount,scale=c(4,2), min.freq=3, random.order=F, rot.per=.1,
colors=pal, family="malgun")
