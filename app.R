library(shiny)
library(tuber)
library(dplyr)
library(tibble)
library(syuzhet)
library(stringi)
library(ggplot2)
library(tidytext)
library(stats)

#OAuth and Connection
app_id <- "830360114408-ev1q4bqv76daq7o0i4b2dj48dehomt36.apps.googleusercontent.com"
app_secret <- "OjLU0iwI5yJjk9axkpwHUjgC"
yt_oauth(app_id, app_secret)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Get YouTube Channel Data and Comments + Sentiment Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "channelID", 
                  label = "Enter Channel ID: ",
                  value = "UCwWhs_6x42TyRM4Wstoq8HA",
                  placeholder = "Channel ID"),
        
        dateRangeInput(inputId = "dateRange", 
                       label = "Enter Date Range"),
        
        actionButton(inputId = "showVideos", 
                     label = "Get Videos"),
        
        actionButton(inputId = "showComments", 
                     label = "Get Comments"),
        
        actionButton(inputId = "showPlot", 
                     label = "Get Sentiment Score")
      ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Channel and Videos", tableOutput("channelStats"), tableOutput('channelVideos')),
                    
                    tabPanel("Comments", tableOutput('videoComments')),

                    tabPanel("Sentiment Analysis", plotOutput('sentimentPlot'))
                    
      )
   )
))

server <- function(input, output) {
  
  #get videos within date range
  selectedData <- reactive({
    req(input$channelID)
    req(input$dateRange)
    videos = yt_search(term="", type="video", channel_id = input$channelID)
    videos = videos %>%
      mutate(date = as.Date(publishedAt)) %>%
      filter(date > input$dateRange[1], date<= input$dateRange[2]) %>%
      arrange(date) %>%
      mutate(Title = title,
             Description = description,
             UploadedOn = publishedAt,
             ID = video_id) %>%
      select(ID, Title, Description, UploadedOn)
  })
  
  #comments data frame
  allComments <- reactive({
    comments = lapply(as.character(selectedData()$ID), function(x){
      get_comment_threads(c(video_id = x), max_results = 1000)
    })
    comments_text = lapply(comments,function(x){
      as.character(x$textOriginal)
    })
    comments_text = tibble(text = Reduce(c, comments_text)) %>%
      mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))
  })
  
  output$channelStats <- 
    renderTable({ 
      req(input$channelID)
      ChannelStats <- get_channel_stats(input$channelID)
      ChannelStats = as.data.frame(ChannelStats) %>%
        mutate(Name = snippet.title, 
               Description = snippet.description,
               Subscribers = statistics.subscriberCount) %>%
        select(Name,Description,Subscribers)
      return(ChannelStats)
    })
  
  output$channelVideos <- 
    renderTable({
      if(input$showVideos>0) {
        selectedData()
      }
    })
  
  output$videoComments <- 
    renderTable({
      if(input$showComments>0) {
        allComments()
      }
    })
  
  output$sentimentPlot <- 
    renderPlot({
      if(input$showPlot>0) {
        req(input$channelID)
        req(input$dateRange)
        custom_stop_words <- bind_rows(data_frame(word = c("white"), #remove "white" from lexicon
                                                  lexicon = c("custom")),
                                       stop_words)
        
        tidyComments <- allComments() %>% #break down sentences into words
          tidytext::unnest_tokens(word, text) %>%
          anti_join(custom_stop_words, by = "word")
        
        commentSentiment <- tidyComments %>% #obtain sentiment scores and order
          inner_join(get_sentiments("nrc"), by = "word") %>% 
          count(word, sentiment, sort = TRUE) %>% 
          group_by(sentiment) %>%
          top_n(10) %>%
          ungroup() %>%
          mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                                  "Positive", "Negative")) 
        
        #Plot number of words/sentiment
        sentiment_Plot <- ggplot(commentSentiment, aes(reorder(sentiment, n), n)) +
          geom_col(aes(fill = pos_neg), show.legend = FALSE) +
          scale_fill_manual(values = c("red2", "green3")) +
          xlab("Sentiment") +
          ylab("Total Number of Words") + 
          labs(title = "Total Number of Words by Sentiment or Emotion") +
          coord_flip()
        
        sentiment_Plot
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

