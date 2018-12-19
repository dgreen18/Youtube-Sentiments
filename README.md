# Youtube-Sentiments

## By Adil Chhabra & David Green

We set out to discover how YouTube communities engage differently with left- and right-leaning content creators, and how that engagement has changed in the two years since the election of Donald Trump. By accessing the YouTube API, we were able to get data such as the view count, likes and dislikes, and the text of all comments from YouTube videos. Since the other data did not provide substantive insights, we performed NRC sentiment analysis upon the comments to gauge the dominant emotions and overall positivity or negativity conveyed by the words used. We created a Shiny app to illustrate our process and allow a user to view the general sentiments in the comments section of any YouTube channel within a given timespan. In comparing the dominant sentiments of the comments sections, we found that right-leaning YouTubers tended to foster a more angry and negative community than left-leaning YouTubers. We also found that since Trump’s election, negative sentiments have increased and positive sentiments have decreased across the board.

The final technical report is Report.pdf.

In order to reproduce results:
1) The most important step is to establish a connection with Youtube Live Data API.
For this, app_id and app_secret are contained in the code.
You will need to ensure however that the file httr-oauth is located in the same folder as the document you wish to run.

2) There are three files containing code: 
The first is GetData.R. 
- This file contains all the code that went into the exploratory process as well as the final code. 
- In here, you will find a great number of plots - showcasing engagement levels, like/dislike ratios for chosen YouTube channels as well as more detailed sentiment analysis. 
- As long as httr-oauth is located within the same folder, you should be able to run the R file.
- Note: The lapply function to fetch comments requires a lot of computational time. 

The second file is a Shiny App, app.R. 
- This can also be accessed using the link: https://adilchhabra.shinyapps.io/stat_231_-_fp/
- This Shiny app showcases the model we used to fetch comments posted on multiple videos of a channel.
- You simply input the channel ID corresponding to the YouTube channel you wish to look at, along with a time frame.
- The Channel ID for a YouTube channel can be located in the URL bar on the browser upon opening a YouTube channel's page on youtube.com.
- The application gets all the videos uploaded by the channel within the selected time frame, and all the comments posted on those videos.
- Finally the application runs sentiment analysis on the text from the comments and returns a plot showcasing number of words/ emotion(or sentiment) of the videos published by the selected channel within the selected time frame.

The third file here is the Report.Rmd file. 
- This is the R Markdown file used to produce the technical report.
- In this file, CSV's that contain comment text have been used for two reasons:
a) Fetching comments requires substantial computational time (upto hours)
b) As the data is live updating, figures and plots you produce using the comment data from today would be different from the ones we produced and analyzed last week.
Thus, CSV's containing comment text utilized by us allow for easy reproducability. 
- The CSV's used in this file are LeftComments.csv, LeftOldComments.csv, RightComments.csv and RightOldComments.csv.