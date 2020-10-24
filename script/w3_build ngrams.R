#w3 building model
twitter <- readLines("data/final/en_US/en_US.twitter.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)
blog <- readLines("data/final/en_US/en_US.blogs.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)
news <- readLines("data/final/en_US/en_US.news.txt", encoding = "UTF-8", warn = FALSE, skipNul = TRUE)

## Explore the data: show number of lines, characters & word count
library(stringi)
library(ngram)
library(knitr)
library(tm)
library(stringr)
library(reshape)

# Extract total word count, max word / entry & total line count from twitter dataset . 
wordTwitter <- stri_count_words(twitter)
wordBlog <- stri_count_words(blog)
wordNews <- stri_count_words(news)
wordCount <- c(sum(wordTwitter), sum(wordBlog), sum(wordNews))
maxWordPerEntry <- c(max(wordTwitter), max(wordBlog), max(wordNews))
avgWordPerEntry <- c(mean(wordTwitter), mean(wordBlog), mean(wordNews))
lineCount <- c(length(twitter), length(blog), length(news))
data <- c("twitter", "blog", "news")
columnNames <- c("file name", "total word count", "max words per entry", "average words per entry", "total line count")
summary <- data.frame(data, wordCount, maxWordPerEntry, avgWordPerEntry, lineCount)
kable(summary, digits = 1, col.names = columnNames, align = 'c')

# Sample 2% of the data  
set.seed(781995)
sampleTwitter <- sample(twitter, length(twitter)*0.02, replace=FALSE)
sampleBlog <- sample(blog, length(blog)*0.02, replace=FALSE)
sampleNews <- sample(news, length(news)*0.02, replace=FALSE)


# Data cleaning functions (reference from nethika)
stringi_toLower <- function(x) stri_trans_tolower(x)
remove_URL <- function(x) gsub("http:[[:alnum:]]*", "", x)
remove_HashTags <- function(x) gsub("#\\S+", "", x)
remove_TwitterHandles <- function(x) gsub("@\\S+", "", x)
remove_nonAscii <- function(x) gsub("[^\x01-\x7F]", "", x)
fix_whitespaces <- function(x) qdapRegex::rm_white(x)

# Clean the sampled data using the tm package 
# We want to set all the characters to lower case, remove all numbers, punctuations and additional white spaces. 
# Besides, we also want to remove stopwords, which are __ , because it is clear that these words will dominate the data set because of the nature of english grammar. 
# Finally, we want to only retain documents with 3 or more words because later on, we want to be able to analyse some 3-worded phrases. 

cleanData <- function(list_sample){
        corpus_text <- Corpus(VectorSource(list_sample))
        corpus_text <- tm_map(corpus_text, content_transformer(remove_URL))
        corpus_text <- tm_map(corpus_text, content_transformer(remove_HashTags))
        corpus_text <- tm_map(corpus_text, content_transformer(remove_TwitterHandles))
        corpus_text <- tm_map(corpus_text, content_transformer(remove_nonAscii))
        corpus_text <- tm_map(corpus_text, content_transformer(stringi_toLower))
        #corpus_text <- tm::tm_map(corpus_text, removeWords, stopwords("en"))
        #corpus_text <- tm::tm_map(corpus_text, removeWords, profanity_words)
        corpus_text <- tm_map(corpus_text, removePunctuation)
        corpus_text <- tm_map(corpus_text, removeNumbers)
        corpus_text <- tm_map(corpus_text, content_transformer(fix_whitespaces))
        return (corpus_text)
        
}

Encoding(sampleTwitter) <- "UTF-8"
Encoding(sampleBlog) <- "UTF-8"
Encoding(sampleNews) <- "UTF-8"


corpusBlog <- cleanData(sampleBlog)
corpusTwitter <- cleanData(sampleTwitter)
corpusNews <- cleanData(sampleNews)

rm(sampleBlog)
rm(sampleTwitter)
rm(sampleNews)


# Concatenate the sampled strings into one string text file
strBlog <- concatenate (lapply(corpusBlog, "[", 1) )
strTwitter <- concatenate (lapply(corpusTwitter, "[", 1) )
strNews <- concatenate ( lapply (corpusNews, "[", 1) )

strSample <- concatenate(strBlog, strTwitter, strNews)

## Let's create a unigram table of all the unigrams using the ngram package
uniGram <- ngram(strSample, n = 1)
uniGram <- get.phrasetable(uniGram)

# Similarly we can create bigrams, trigrams, tetragrams & pentagrams
biGram <- ngram(strSample, n=2)
biGram <- get.phrasetable(biGram)
biGram = transform(biGram, ngrams = colsplit(ngrams, split = "\\ ", names = c('first', 'second')))

triGram <- ngram(strSample, n = 3)
triGram <- get.phrasetable(triGram)
triGram = transform(triGram, ngrams = colsplit(ngrams, split = "\\ ", names = c('first', 'second', 'third')))

tetraGram <- ngram(strSample, n = 4)
tetraGram <- get.phrasetable(tetraGram)
tetraGram = transform(tetraGram, ngrams = colsplit(ngrams, split = "\\ ", names = c('first', 'second', 'third', 'fourth')))

pentaGram <- ngram(strSample, n = 5)
pentaGram <- get.phrasetable(pentaGram)
pentaGram <- transform(pentaGram, ngrams = colsplit(ngrams, split = "\\ ", names = c('first', 'second','third','fourth','fifth')))

# saving results 
saveRDS(biGram, file="output/bigram.rds")
saveRDS(triGram, file="output/trigram.rds")
saveRDS(tetraGram, file="output/tetragram.rds")
saveRDS(pentaGram, file="output/pentagram.rds")
