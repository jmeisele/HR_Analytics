library(tm)
library(wordcloud)
get_sentiments("afinn")
library(janeaustenr)
library(dplyr)
library(stringr)
library(psych)
library(tidytext)

get_sentiments("afinn")
words <- get_sentiments("afinn")[1]
scores <- get_sentiments("afinn")[2]
#################
sylFile <- "C:\\Users\\jmeis\\Desktop\\MLK.txt"
input <- readLines(sylFile)

tail(input, 20)
str(input)

# Use the tm package
# create the word list with counts
words.vec <- VectorSource(input)
words.corpus <- Corpus(words.vec)
words.corpus

tdm <- TermDocumentMatrix(words.corpus)

# explore what we created
str(tdm)
inspect(tdm[1:20, 1:20])

# now create a word cloud
m <- as.matrix(tdm)
wordCounts <- row$Sums(m)
head(wordCounts)

wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)

wordcloud(names(wordCounts), wordCounts)

#now remove stuff we dont care about
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#recreate the wordcount
tdm <- TermDocumentMatrix(words.corpus)
tdm

m <- as.matrix(tdm)
wordCounts <- rom$Sums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)

cloudFrame <- data.frame(word = names(wordCounts), freq=wordCounts)
wordcloud(names(wordCounts), wordCounts)

wordcloud(names(wordCounts), wordCounts, min.freq=3, max.freq=50)
wordcloud(names(wordCounts), wordCounts, min.freq=3, max.freq=50, rot.per=0.5)
wordcloud(names(wordCounts), wordCounts, min.freq=3, max.freq=50, rot.per=0.35)

findAssocs(tdm, "data", 0.6)
findAssocs(tdm, "course", 0.6)
