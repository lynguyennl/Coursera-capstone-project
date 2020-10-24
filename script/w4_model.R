# Read ngram files
bigram <- readRDS("output/bigram.rds")
trigram <- readRDS("output/trigram.rds")
tetragram <- readRDS("output/tetragram.rds")
pentagram <- readRDS("output/pentagram.rds")

guess_1 = ""
guess_2 = ""
guess_3 = ""

textClean <- function(text){
        
        text <- tolower(text)
        text <- removePunctuation(text)
        text <- removeNumbers(text)
        text <- str_replace_all(text, "[^[:alnum:]]", " ")
        text <- stripWhitespace(text)
        
        return(text)
}

textSplit <- function(text){
        
        text <- textClean(text)
        text <- stylo::txt.to.words(text)
        
        return(text)
}



textPredict <- function(text)
{
        
        text <- textClean(text)
        text <- textSplit(text)
        
        if (length(text)>=5) {
                text = tail(text, 4) 
                
        }
        if (length(text)==4) {
                match <- pentagram[pentagram$ngrams$first==text[1] & 
                                           pentagram$ngrams$second==text[2] & 
                                           pentagram$ngrams$third==text[3] &
                                           pentagram$ngrams$forth==text[4], ]
                guess_1 <- as.character(match$ngrams$fifth[1])
                guess_2 <- as.character(match$ngrams$fifth[2])
                guess_3 <- as.character(match$ngrams$fifth[3])
                if (is.na(guess_1)) {
                        text = tail(text, 3)
                }
                
        } 
        
        if (length(text)==3) {
                match <- tetragram[tetragram$ngrams$first==text[1] & 
                                           tetragram$ngrams$second==text[2] & 
                                           tetragram$ngrams$third==text[3],]
                guess_1 <- as.character(match$ngrams$fourth[1])
                guess_2 <- as.character(match$ngrams$fourth[2])
                guess_3 <- as.character(match$ngrams$fourth[3])
                if (is.na(guess_1)) {
                        text = tail(text, 2)
                }
        } 
        
        if (length(text)==2) {
                match <- trigram[trigram$ngrams$first==text[1] & 
                                         trigram$ngrams$second==text[2], ]
                guess_1 <- as.character(match$ngrams$third[1])
                guess_2 <- as.character(match$ngrams$third[2])
                guess_3 <- as.character(match$ngrams$third[3])
                if (is.na(guess_1)) {
                        text = tail(text, 1)
                }
        } 
        if (length(text)==1) {
                match <- bigram[bigram$ngrams$first==text[1],]
                guess_1 <- as.character(match$ngrams$second[1])
                guess_2 <- as.character(match$ngrams$second[2])
                guess_3 <- as.character(match$ngrams$second[3])
                if (is.na(guess_1)) {
                        print(guess_1 <- "the")
                }
        }
        
        return(c(guess_1,guess_2,guess_3))
        
}

memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=56000)
