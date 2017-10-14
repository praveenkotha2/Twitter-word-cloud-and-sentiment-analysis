library(shiny)
library(shinythemes)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(devtools)
library(quanteda)
library(ggplot2)
library(syuzhet)

# Define UI for app 
ui <- fluidPage(
  
  #Overall theme for shiny app
  theme = shinytheme("slate"),
  
  # App title
  titlePanel("Word Cloud and Sentiment Analysis of Tweets"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #Input: The word to extract tweets
      textInput(inputId = "word",
                label = "Enter the word to search in twitter",
                value = 'Datascience'),
      
      #Input: The no. of tweets to extract
      numericInput(inputId = "no",
                   label = "Number of tweets",
                   value = 300),
      
      # Input: Slider for the frequency of words to plot ----
      sliderInput(inputId = "freq",
                  label = "Minimum Frequency of words",
                  min = 1,
                  max = 100,
                  value = 5),
      
      #Input: Action button to plot word cloud and sentiment analysis
      actionButton("go", "Plot")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      ui <- splitLayout(
        style = "border: 2px solid silver;",
        cellWidths = c("40%", "60%"),
        cellArgs = list(style = "padding: 6px"),
        
        #Output: Plot for word cloud
        plotOutput(outputId = "wordcloud"),
        
        #Output: Plot for sentiment analysis
        plotOutput(outputId = "posneg")
        
      ),
      tags$h4("Mean sentiment score:"),
      verbatimTextOutput(outputId = "score")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #Keys from twitter developer account for specific application
  api_key <- "Cog1fKWcwffPX3b3NI9KSV0Xz"
  api_secret <- "uL1H6WX03KpCrOSbfcBHj4oPIvqtfJjObAf2N8iOsIBP5FL0vY"
  access_token <- "916218481976123393-8ww816m4WNTm0urjfV072niBfqwKCSZ"
  access_token_secret <- "NdqsC0ymBA8vSGgkRKdshlgecRmjIIROKZurYvdAxlxwx"
  
  #twiiter authentication
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  #for direct authentication set to true
  options(httr_oauth_cache=T)
  
  rm(api_key)
  rm(api_secret)
  rm(access_token)
  rm(access_token_secret)
  
  observeEvent(input$go, {  
    tweets_corpus<-reactive({
      
      set.seed(1234)
      
      #Extracting the tweets from twitter api for search word
      tweets = searchTwitter(isolate(input$word), n=isolate(input$no))
      
      #extracting text from the tweets
      tweets_text <- sapply(tweets, function(x) x$getText())
      
      #to remove graphic and non-english characters
      tweets_text<-str_replace_all(tweets_text,"[^[:graph:]]", " ") 
      tweets_text<-iconv(tweets_text, "latin1", "ASCII", sub="")
      
      #to convert text into corpus, since tm package works with corpus
      tweets_text_corpus <- Corpus(VectorSource(tweets_text))
      class(tweets_text_corpus)
      tweets_text_corpus$content[1]
      
      #to remove punctuation
      tweets_text_corpus <- tm_map(tweets_text_corpus, removePunctuation)
      tweets_text_corpus$content[1]
      
      #to lower the text
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(tolower))
      
      #to remove stopword
      tweets_text_corpus <- tm_map(tweets_text_corpus, function(x)
        removeWords(x,stopwords()))
      tweets_text_corpus$content[1]
      
      tweets_text_corpus <- tm_map(tweets_text_corpus, removeWords, c('rt', 'are','that'))
      
      #using regular expressions to clean the text
      removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
      removeHtml<- function(x) gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x)
      removeSym<- function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
      removeDigit<- function(x) gsub("[[:digit:]]", " ", x)
      removeSpace1<-function(x) gsub("[ \t]{2,}", " ", x)
      removeSpace2<- function(x) gsub("^\\s+|\\s+$", "", x)
      removeHash<- function(x) gsub("#\\w+", " ", x)
      removeat<- function(x) gsub("@\\w+", " ", x)
      removePun<- function(x) gsub("[[:punct:]]", " ", x)
      
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeURL))
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeHtml))
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeSym))  
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeDigit)) 
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeSpace1)) 
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeSpace2)) 
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeHash)) 
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removeat)) 
      tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(removePun)) 
      
      
    })
    
    
    
    observeEvent(input$go, {
      output$wordcloud <- renderPlot({
        
        
        #to plot wordcloud
        wordcloud(tweets_corpus(),min.freq=isolate(input$freq),scale=c(3.5,1),rot.per=.5,
                  colors=brewer.pal(8, "Dark2"),random.order=FALSE, 
                  random.color=TRUE)
      })
      
    })
    
    observeEvent(input$go, {
      
      output$posneg<-renderPlot({
        
        
        #to avoid duplicate tweets
        tweets_text1<-unique(tweets_corpus()$content)
        
        #sentiment analysis using 'syuzhet' package
        sentiment = get_sentiment(tweets_text1, method="syuzhet")
        emotions = get_nrc_sentiment(tweets_text1)
        
        tweets_new_df<-data.frame(tweets_text1,sentiment,emotions)
        tweets_new_df$negative<- -tweets_new_df$negative
        tweets_new_df$rating<-tweets_new_df$positive+tweets_new_df$negative
        tweets_new_df$posneg<-ifelse(tweets_new_df$rating<0,'Negative',
                                     ifelse(tweets_new_df$rating>0,'Positive','Neutral'))
        res<-sum(tweets_new_df$positive)-sum(tweets_new_df$negative)
        mean(tweets_new_df$rating)
        str(tweets_new_df)
        
        #plotting sentiment analysis
        ggplot(aes(x=posneg),data=tweets_new_df)+
          geom_bar(aes(y=..count.., fill=posneg))+
          scale_fill_brewer(palette="RdGy") + ggtitle('Polarity Analysis of  Tweets on Twitter')+
          theme(legend.position='right')+ 
          ylab('Number of Tweets')+  
          xlab('Polarity Categories') 
        
      })
      
    })
    
    observeEvent(input$go, {
      
      output$score<-renderPrint({
        
        
        #to avoid duplicate tweets
        tweets_text1<-unique(tweets_corpus()$content)
        
        #sentiment analysis using 'syuzhet' package
        sentiment = get_sentiment(tweets_text1, method="syuzhet")
        emotions = get_nrc_sentiment(tweets_text1)
        
        tweets_new_df<-data.frame(tweets_text1,sentiment,emotions)
        tweets_new_df$negative<- -tweets_new_df$negative
        tweets_new_df$rating<-tweets_new_df$positive+tweets_new_df$negative
        tweets_new_df$posneg<-ifelse(tweets_new_df$rating<0,'Negative',
                                     ifelse(tweets_new_df$rating>0,'Positive','Neutral'))
        res<-sum(tweets_new_df$positive)-sum(tweets_new_df$negative)
        mean(tweets_new_df$rating)
      })
      
    })
    
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
