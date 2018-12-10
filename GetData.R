library(tuber)
library(syuzhet)
library(dplyr)
library(tibble)
library(stringi)
library(ggplot2)

#OAuth and Connection
app_id <- "830360114408-ev1q4bqv76daq7o0i4b2dj48dehomt36.apps.googleusercontent.com"
app_secret <- "OjLU0iwI5yJjk9axkpwHUjgC"
yt_oauth(app_id, app_secret)

# Channels Picked
# Two incredibly popular right leaning youtubers:
# Steven Crowder - https://www.youtube.com/channel/UCIveFvW-ARp_B_RckhweNJw
# Ben Shapiro - https://www.youtube.com/channel/UCnQC_G5Xsjhp9fEJKuIcrSw
# Two incredibly popular left leaning youtubers:
# Contrapoints - https://www.youtube.com/user/ContraPoints
# Shaun - https://www.youtube.com/channel/UCJ6o36XL0CpYb6U5dNBiXHQ

crowderID <- "UCIveFvW-ARp_B_RckhweNJw"
shapiroID <- "UCnQC_G5Xsjhp9fEJKuIcrSw"
contraID <- "UCNvsIonJdJ5E4EXMa65VYpA"
shaunID <- "UCJ6o36XL0CpYb6U5dNBiXHQ"

#Get all videos of a channel from the last 3 months
get_videos <- function(channelID){
  videos = yt_search(term="", type="video", channel_id = channelID)
  videos = videos %>%
    mutate(date = as.Date(publishedAt)) %>%
    filter(date > "2018-10-01") %>%
    arrange(date)
  return(videos)
}

# Get statistics of the videos and make data frame

get_video_stats <- function(videos) {
  
  videostats = lapply(as.character(videos$video_id), function(x){
    get_stats(video_id = x)
  })
  
  videostats = do.call(rbind.data.frame, videostats)
  videostats$title = videos$title
  videostats$date = videos$date
  videostats = select(videostats, date, id, title, viewCount, likeCount, dislikeCount, commentCount) %>%
    as.tibble() %>%
    mutate(viewCount = as.numeric(as.character(viewCount)),
           likeCount = as.numeric(as.character(likeCount)),
           dislikeCount = as.numeric(as.character(dislikeCount)),
           commentCount = as.numeric(as.character(commentCount)))
}

# Introduction to YouTubers

crowderStats <- get_channel_stats(crowderID)
shapiroStats <- get_channel_stats(shapiroID)
contraStats <- get_channel_stats(contraID)
shaunStats <- get_channel_stats(shaunID)

crowderStats = do.call(rbind.data.frame, crowderStats["statistics"]) %>%
  as.tibble() %>%
  mutate(subscriberCount = as.numeric(as.character(subscriberCount)),
         viewRatio = as.numeric(as.character(viewCount))/as.numeric(as.character(videoCount)))
         
shapiroStats = do.call(rbind.data.frame, shapiroStats["statistics"]) %>%
  as.tibble() %>%
  mutate(subscriberCount = as.numeric(as.character(subscriberCount)),
         viewRatio = as.numeric(as.character(viewCount))/as.numeric(as.character(videoCount)))

contraStats = do.call(rbind.data.frame, contraStats["statistics"]) %>%
  as.tibble() %>%
  mutate(subscriberCount = as.numeric(as.character(subscriberCount)),
         viewRatio = as.numeric(as.character(viewCount))/as.numeric(as.character(videoCount)))

shaunStats = do.call(rbind.data.frame, shaunStats["statistics"]) %>%
  as.tibble() %>%
  mutate(subscriberCount = as.numeric(as.character(subscriberCount)),
         viewRatio = as.numeric(as.character(viewCount))/as.numeric(as.character(videoCount)))

subCounts <- data.frame(Names = c("Steven Crowder","Ben Shapiro","Contrapoints", "Shaun"), 
                  SubscriberCount = c(crowderStats$subscriberCount,shapiroStats$subscriberCount,contraStats$subscriberCount,shaunStats$subscriberCount))

viewCountRatios <- data.frame(Names = c("Steven Crowder","Ben Shapiro","Contrapoints", "Shaun"), 
                              ViewRatio = c(crowderStats$viewRatio,shapiroStats$viewRatio,contraStats$viewRatio,shaunStats$viewRatio))

subs <- ggplot(subCounts, aes(x = reorder(Names, SubscriberCount), y = SubscriberCount,fill=Names)) +
  geom_bar(stat="identity", width = 0.3) +
  coord_flip() +
  theme_minimal()

viewRatios <- ggplot(viewCountRatios, aes(x = reorder(Names, ViewRatio), y = ViewRatio,fill=Names)) +
  geom_bar(stat="identity", width = 0.3) +
  coord_flip() +
  theme_minimal()

options(scipen=10000) #remove scientific notation
subs
viewRatios

#Get Crowder videos
crowderVideos <- get_videos(crowderID)
crowderVideoStats <- get_video_stats(crowderVideos)

shapiroVideos <- get_videos(shapiroID)
shapiroVideoStats <- get_video_stats(shapiroVideos)

contraVideos <- get_videos(contraID)
contraVideoStats <- get_video_stats(contraVideos)

shaunVideos <- get_videos(shaunID)
shaunVideoStats <- get_video_stats(shaunVideos)

engage <- data.frame(Names = c("Steven Crowder","Ben Shapiro","Contrapoints", "Shaun"),
                     noViews = c(sum(crowderVideoStats$viewCount), sum(shapiroVideoStats$viewCount), sum(contraVideoStats$viewCount), sum(shaunVideoStats$viewCount)),
                     noLikes = c(sum(crowderVideoStats$likeCount), sum(shapiroVideoStats$likeCount), sum(contraVideoStats$likeCount), sum(shaunVideoStats$likeCount)),
                     noDislikes = c(sum(crowderVideoStats$dislikeCount), sum(shapiroVideoStats$dislikeCount), sum(contraVideoStats$dislikeCount), sum(shaunVideoStats$dislikeCount)),
                     politicalSide = c("Right","Right","Left","Left"))

engagePol <- group_by(engage, politicalSide) %>%
  summarise(numViews = sum(noViews), numLikes = sum(noLikes), numDislikes = sum(noDislikes)) %>%
  mutate(reactRatio = (numLikes+numDislikes)/numViews, 
         likeRatio = numLikes/numViews,
         dislikeRatio = numDislikes/numViews)

