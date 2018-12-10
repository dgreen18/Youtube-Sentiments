library(shiny)
library(tuber)

#OAuth and Connection
app_id <- "830360114408-ev1q4bqv76daq7o0i4b2dj48dehomt36.apps.googleusercontent.com"
app_secret <- "OjLU0iwI5yJjk9axkpwHUjgC"
yt_oauth(app_id, app_secret)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Get YouTube Channel Data and Comments"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "channelID", 
                  label = "Enter Channel ID: ", 
                  placeholder = "Channel ID"),
        
        dateRangeInput(inputId = "dateRange", 
                       label = "Enter Date Range") 
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput('channelStats'),
         tableOutput('channelVideos')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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
             UploadedOn = publishedAt) %>%
      select(Title, Description, UploadedOn)
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
      selectedData()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

