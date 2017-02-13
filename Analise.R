#install and load packages
install.packages('twitteR')
install.packages('tm')
install.packages("SnowballC")
install.packages("/home/guilherme/Downloads/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
install.packages("/home/guilherme/Downloads/sentiment_0.2.tar.gz", repos = NULL, type = "source")

library(twitteR)
library(tm)
library(wordcloud)
library(SnowballC)
library(Rstem)
library(sentiment)
library(plotly)
library(plyr)

#setup twitter
api_key <- 'IpbVirlaHHLkTP8MfkL5Yuavm'
api_secret <- '7fOU01fQj5QBLNxRxo85k9k1Hze7HDlOMtaOBjOKTwynnSCulA'
token <- '830527057423691776-hVh1j10t8yNsX6f98NVwyEAfwFvYtsO'
token_secret <- 'vAwDfU9buhjd7yaFrdyv5zcCo2HWefTd6amN7L5K79LVY'

setup_twitter_oauth(api_key,api_secret,token,token_secret)

#load tweets
tweets <- searchTwitter("#Rio2016",n=500, lang = 'pt')
tweetpt <- twListToDF(tweets)

#clean and preprocessing data
tweetpt <- iconv(tweetpt$text, 'UTF-8', 'latin1', 'byte')
tweetpt = gsub("https://.+","",tweetpt)
tweetpt = gsub("<.+>","",tweetpt)

tweetpt <- paste(tweetpt, collapse = ' ')

s_words <- c("sobre","após","httpstcopeidluqlc","seis","xedxaxbdxedxbxa","well","superb")
tweetpt = Corpus(VectorSource(tweetpt))
tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)
tweetpt = Corpus(VectorSource(tweetpt))
tweetpt <- tm_map(tweetpt, content_transformer(tolower))
tweetpt <- tm_map(tweetpt, removeNumbers)
tweetpt <- tm_map(tweetpt, removePunctuation)
tweetpt = tm_map(tweetpt, removeWords, stopwords('pt'))
tweetpt = tm_map(tweetpt, removeWords, s_words)
tweetpt <- tm_map(tweetpt, PlainTextDocument)

#wordcloud
set.seed(11)
png("Rio2016_WordCloud.png", width = 380, height = 380)
wordcloud(tweetpt, min.freq = 3,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
dev.off()

#Sentiment Analyzer
tweetst <- twListToDF(tweets)
tweetst <- iconv(tweetst$text, 'UTF-8', 'latin1', 'byte')

#clean data
tweetst = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetst)
tweetst = gsub("@\\w+", "", tweetst)
tweetst = gsub("[[:punct:]]", "", tweetst)
tweetst = gsub("[[:digit:]]", "", tweetst)
tweetst = gsub("http\\w+", "", tweetst)
tweetst = gsub("https://.+","",tweetst)
tweetst = gsub("[ \t]{2,}", "", tweetst)
tweetst = gsub("^\\s+|\\s+$", "", tweetst)
tweetst = gsub("<.+>","",tweetst)
tweetst = tolower(tweetst)

tweetst = tweetst[!is.na(tweetst)]
names(tweetst) = NULL

#classify
class_pol = classify_polarity(tweetst, algorithm = "bayes")
polarity = class_pol[,4]

#Adequacy
sent_df = data.frame(text = tweetst, polarity = polarity, stringsAsFactors = FALSE)

sent_df$polarity = gsub("positive","POSITIVO",sent_df$polarity)
sent_df$polarity = gsub("negative","NEGATIVO",sent_df$polarity)
sent_df$polarity = gsub("neutral","NEUTRO",sent_df$polarity)

sent_df <- count(sent_df,~polarity)
sent_df$polarity <- paste(sent_df$polarity,sent_df$freq)

#plot
plot_ly(sent_df, labels = ~polarity, values = ~freq,type = 'pie') %>%
        layout(title = 'Análise de sentimentos sobre as Olimpíadas Rio2016',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

