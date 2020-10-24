library(shiny)
library(DT)
library(tm)
library(ngram)
library(stringr)
library(reshape)
library(stylo)
library(dplyr)
library(plotly)


#setwd("C:/Users/Ly Nguyen/Learning/Courses/Johns Hopkins - R/10. Capstone Project/capstone_for submit")
#source("./script/w4_model.R", local=TRUE)
source("w4_model.R", local=TRUE)

shinyServer(function(input, output) {
    textPrediction <- reactive({
        text <- input$text
        textPredict <- textPredict(text)
    })
    
    output$guess_1 <- renderText(textPrediction()[1])
    output$guess_2 <- renderText(textPrediction()[2])
    output$guess_3 <- renderText(textPrediction()[3])
    
    output$ngramP <- renderUI({
        selectInput("ngram_p", "Select an ngram:", choices = c("bigram", "trigram", "tetragram", "pentagram"), selected = "bigram")
    })
    
    data <- reactive({
        
        if (is.null(input$ngram_p)) {
            return(data.frame(ngram=NA,freq=NA))
        } else {
            
            if (input$ngram_p == "bigram") {
                df <- data.frame(head(bigram,50))
                df$ngram <- paste0(df$ngrams$first," ",df$ngrams$second)
                df <- df[c("ngram", "freq")]
            }
            if (input$ngram_p == "trigram"){
                df <- head(trigram,50)
                df$ngram <- paste0(df$ngrams$first," ",df$ngrams$second," ",df$ngrams$third)
                df <- df[c("ngram", "freq")]
            }
            
            if (input$ngram_p == "tetragram"){
                df <- head(tetragram,50)
                df$ngram <- paste0(df$ngrams$first," ",df$ngrams$second," ",df$ngrams$third," ",df$ngrams$fourth)
                df <- df[c("ngram", "freq")]
            }
            
            if (input$ngram_p == "pentagram"){
                df <- head(pentagram,50)
                df$ngram <- paste0(df$ngrams$first," ",df$ngrams$second," ",df$ngrams$third," ",df$ngrams$fourth," ",df$ngrams$fifth)
                df <- df[c("ngram", "freq")]
            }
            
            return(df)
        }
    })
    
    output$ngramT <- renderUI({
        selectInput("ngram_t", "Select a ngram:", choices = c('bigram','trigram','tetragram','pentagram'), selected = "bigram")
    })
    
    data2 <- reactive({
        
        
        if (is.null(input$ngram_t)){
            return()
        } else {
            
            if (input$ngram_t == 'bigram') {
                df <- head(bigram,50)
                df$ngram <- paste0(df$ngrams$first," ",df$ngrams$second)
                df <- df[c("ngram", "freq")]
            }
            
            if (input$ngram_t == 'trigram'){
                df <- head(trigram,50)
                df$ngram <- paste0(df$ngrams$first," ", df$ngrams$second," ", df$ngrams$third)
                df <- df[c("ngram", "freq")]
            }
            if (input$ngram_t == 'tetragram'){
                df <- head(tetragram,50)
                df$ngram <- paste0(df$ngrams$first,"  ",df$ngrams$second,"  ",df$ngrams$third,"  ",df$ngrams$fourth)
                df <- df[c("ngram", "freq")]
            }
            
            if (input$ngram_t == 'pentagram'){
                df <- head(pentagram,50)
                df$ngram <- paste0(df$ngrams$first,"  ",df$ngrams$second,"  ",df$ngrams$third,"  ",df$ngrams$fourth,"  ",df$ngrams$fifth)
                df <- df[c("ngram", "freq")]
                
            }
            
            return(df)
        }
    })
    
    output$ngramtable  = DT::renderDataTable({
        data2()
    })
    
    output$ngramPlot <- renderPlotly({
        theme_set(theme_bw())
        
        g <- ggplot(data()[1:input$n_terms,], 
                    aes(x=reorder(ngram,-freq), y=freq), 
                    fill=freq) + 
            geom_point(stat="identity", position="identity", 
                       aes(colour = freq, text=paste("ngram: ",ngram, "\nfrequency: ", freq))) +
            scale_color_continuous(low="#89CFEF", high="#1C2951") +
            labs(title="ngram", 
                 y="frequency",
                 x ="") +
            theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1))
        ggplotly(g, tooltip = "text")
    })
    
})
