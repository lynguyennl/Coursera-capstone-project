#w2 milestone report
twitter <- readLines("data/final/en_US/en_US.twitter.txt")
blog <- readLines("data/final/en_US/en_US.blogs.txt")
news <- readLines("data/final/en_US/en_US.news.txt")

## Explore the data: show number of lines, characters & word count
library(stringi)
library(knitr)
library(tm)
list <- list(twitter, blog, news)
stats <- data.frame(t(rbind(sapply(list, stri_stats_general)))[, c(1,3)])
fileName <- data.frame(fileName = c("twitter", "blog", "news"))
words <- data.frame(wordCount = t(rbind(sapply(list, stri_stats_latex)))[, 4])
data <- data.frame(fileName, stats, words)
kable(data)

## Clean the data 
set.seed(781995)
sampleTwitter <- sample(twitter, length(twitter)*0.01, replace=FALSE)
sampleBlog <- sample(blog, length(blog)*0.01, replace=FALSE)
sampleNews <- sample(news, length(news)*0.01, replace=FALSE)

## Build corpus 
corpus <- Corpus(VectorSource(c(sampleTwitter, sampleBlog, sampleNews)))

## Tokenization 
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
corpus.bigram = TermDocumentMatrix(cleanCorpus,
                                control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(corpus.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

findFreqTerms(matrix, lowfreq=50)
findFreqTerms(matrix, lowfreq=100)

## Clean corpus 
transform <- content_transformer(function(x, pattern) 
        {gsub(pattern, "", x)}
        )

cleanCorpus <- tm_map(corpus, PlainTextDocument)
cleanCorpus <- tm_map(cleanCorpus, removeNumbers)
cleanCorpus <- tm_map(cleanCorpus, tolower)
cleanCorpus <- tm_map(cleanCorpus, removePunctuation)
cleanCorpus <- tm_map(cleanCorpus, stripWhitespace)

cleanCorpus.matrix <- as.matrix(cleanCorpus)

findFreqTerms(cleanCorpus, lowfreq = 50)

## Visualize data 
wordcloud(cleanCorpus, max.words=75, random.order=FALSE, scale=c(5, 1))

## Create ngrams 
library(ngram)
word_count_vec <- function(input_string_vec){
        sapply(input_string_vec, wordcount)
}
gram1 <- ngram(cleanCorpus[word_count_vec(cleanCorpus) >= 1], n = 1)
news_phrasetable1 <- get.phrasetable(news_gram1)

twit_gram1 <- ngram(twit_lines[word_count_vec(twit_lines) >= 1], n = 1)
twit_phrasetable1 <- get.phrasetable(twit_gram1)

blog_gram1 <- ngram(blog_lines[word_count_vec(blog_lines) >= 1], n = 1)
blog_phrasetable1 <- get.phrasetable(blog_gram1)
