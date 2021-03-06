library(tuber)
library(syuzhet)
library(dplyr)
library(tibble)
library(stringi)
library(ggplot2)
library(tidytext)


# OAuth and Connection
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

# Get all videos of a channel from the last 3 months
get_videos <- function(channelID){
  videos = yt_search(term="", type="video", channel_id = channelID) #search by channel
  videos = videos %>% #get videos from past 3 months
    mutate(date = as.Date(publishedAt)) %>%
    filter(date > "2018-10-01") %>%
    arrange(date)
  return(videos)
}

# Get statistics of the videos and make data frame
get_video_stats <- function(videos) {
  
  videostats = lapply(as.character(videos$video_id), function(x){ #get stats for all videos
    get_stats(video_id = x)
  })
  
  videostats = do.call(rbind.data.frame, videostats) #clean data, turn to dataframe
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

# Get basic statistics for all YouTubers - Channel Name, Subscriber count and Views/Video
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

# Put subscriber counts and views/video ratio in dataframes
subCounts <- data.frame(Names = c("Steven Crowder","Ben Shapiro","Contrapoints", "Shaun"), 
                  SubscriberCount = c(crowderStats$subscriberCount,shapiroStats$subscriberCount,contraStats$subscriberCount,shaunStats$subscriberCount))

viewCountRatios <- data.frame(Names = c("Steven Crowder","Ben Shapiro","Contrapoints", "Shaun"), 
                              ViewRatio = c(crowderStats$viewRatio,shapiroStats$viewRatio,contraStats$viewRatio,shaunStats$viewRatio))

# Plot subscriber count
subs <- ggplot(subCounts, aes(x = reorder(Names, SubscriberCount), y = SubscriberCount,fill=Names)) +
  geom_bar(stat="identity", width = 0.3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "No. of Subscribers/ Chosen YouTuber", y = "Number of Subscribers", x= "YouTuber")

# Plot views/video ratio
viewRatios <- ggplot(viewCountRatios, aes(x = reorder(Names, ViewRatio), y = ViewRatio,fill=Names)) +
  geom_bar(stat="identity", width = 0.3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Views/Video for Each Chosen YouTuber", y = "Views/Video", x= "YouTuber")

options(scipen=10000) #remove scientific notation
subs
viewRatios

# Engagement by Political Bent

#Obtain all videos for the channels, and the statistics for those videos
crowderVideos <- get_videos(crowderID)
crowderVideoStats <- get_video_stats(crowderVideos)

shapiroVideos <- get_videos(shapiroID)
shapiroVideoStats <- get_video_stats(shapiroVideos)

contraVideos <- get_videos(contraID)
contraVideoStats <- get_video_stats(contraVideos)

shaunVideos <- get_videos(shaunID)
shaunVideoStats <- get_video_stats(shaunVideos)

# Conducting exploratory analysis - 
# Hard coding left and right engagement statistics
# Right - Steven Crowder + Ben Shapiro
# Left - Contrapoints + Shaun

rightViews <- sum(crowderVideoStats$viewCount) + sum(shapiroVideoStats$viewCount)
rightLikes <- sum(crowderVideoStats$likeCount) + sum(shapiroVideoStats$likeCount)
rightDislikes <- sum(crowderVideoStats$dislikeCount) + sum(shapiroVideoStats$dislikeCount)
rightReacts <- rightLikes+rightDislikes
rightReactsRatio <- trunc((rightReacts/rightViews)*100)
rightLeftover <-  100-rightReactsRatio
rightLikeRatio <- ceiling((rightLikes/rightReacts)*100)
rightDislikeRatio <- floor((rightDislikes/rightReacts)*100)

leftViews <- sum(contraVideoStats$viewCount) + sum(shaunVideoStats$viewCount)
leftLikes <- sum(contraVideoStats$likeCount) + sum(shaunVideoStats$likeCount)
leftDislikes <- sum(contraVideoStats$dislikeCount) + sum(shaunVideoStats$dislikeCount)
leftReacts <- leftLikes+leftDislikes
leftReactsRatio <- trunc((leftReacts/leftViews)*100)
leftLeftover <-  100-leftReactsRatio
leftLikeRatio <- ceiling((leftLikes/leftReacts)*100)
leftDislikeRatio <- floor((leftDislikes/leftReacts)*100)

# Data frame with react ratios for left and right
engage <- data.frame(Names = c("RightReactRatio","RightLeftover","LeftReactRatio","LeftLeftover"),
                     Num = c(rightReactsRatio, rightLeftover, leftReactsRatio,leftLeftover),
                     PoliticalSide = c("Right","Right","Left","Left"),
                     ID = c(2,1,4,3),
                     Type =c("New"),
                     Kind = c("Ratio")) 

# Data frame with like and dislike ratios for left and right
engage2 <- data.frame(Names = c("RightLikeRatio","RightDislikeRatio","LeftLikeRatio","LeftDislikeRatio"),
                      Num = c(rightLikeRatio,rightDislikeRatio,leftLikeRatio,leftDislikeRatio),
                      PoliticalSide = c("Right","Right", "Left","Left"),
                      ID = c(1,2,3,4),
                      Type =c("New"))

# Plotting enagagement - React ratio
bp<- ggplot(engage, aes(x= Kind, y=Num, fill = Names))+
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~PoliticalSide) + 
  geom_text(aes(x = Kind, y = Num-2.3,label = paste0(Num,"%")), size=4) +
  labs(title = "Engagement(% Reactions/Views) for Left & Right Leaning YouTubers", x = "Engagement", y= "Percentage(%)") +
  theme(legend.title = element_blank()) 
bp 

# Plotting engagement - Like/Dislike Ratio
bp2<- ggplot(engage2[which(engage2$Num>0),], aes(x= Names, y=Num, fill = Names))+
  geom_bar(stat = "identity",width=0.5) +
  facet_wrap(~PoliticalSide,scales="free_x") + 
  geom_text(aes(x = Names, y = Num-1.7,label = paste0(Num,"%")), size=4) +
  labs(title = "% Likes(/Reacts) v/s % Dislikes(/Reacts)  for Left & Right Leaning YouTubers", x = "Dislike and Like Ratios", y= "Percentage(%)") +
  theme(legend.title = element_blank())
bp2

# Beginning Sentiment Analysis

# Obtain comments for Youtubers
# Change data type to data frame containing only the comment text 
commentsContra = lapply(as.character(contraVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsContra_text = lapply(commentsContra,function(x){
  as.character(x$textOriginal)
})
commentsContra_text = tibble(text = Reduce(c, commentsContra_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

commentsShaun = lapply(as.character(shaunVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsShaun_text = lapply(commentsShaun,function(x){
  as.character(x$textOriginal)
})
commentsShaun_text = tibble(text = Reduce(c, commentsShaun_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

commentsCrowder = lapply(as.character(crowderVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsCrowder_text = lapply(commentsCrowder,function(x){
  as.character(x$textOriginal)
})
commentsCrowder_text = tibble(text = Reduce(c, commentsCrowder_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

commentsShapiro = lapply(as.character(shapiroVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsShapiro_text = lapply(commentsShapiro,function(x){
  as.character(x$textOriginal)
})
commentsShapiro_text = tibble(text = Reduce(c, commentsShapiro_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

# Combining Left and Right channel comments
leftComments = rbind(commentsContra_text,commentsShaun_text)
rightComments = rbind(commentsCrowder_text,commentsShapiro_text)

# Preparing lexicon for analysis
# Removing  "white" from lexicon due to inappropriate usage of associated sentiment

custom_stop_words <- bind_rows(data_frame(word = c("white"), lexicon = c("custom")),stop_words) 

# Breaking down sentences from comments into tokens (words)
tidy_left_comments <- leftComments %>% 
  tidytext::unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")

tidy_right_comments <- rightComments %>% 
  tidytext::unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")

leftTokenScores <- tidy_left_comments %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% #assign sentiment based on NRC lexicon
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() 
  
rightTokensScores <- tidy_right_comments %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()
  
# Plots faceting most used words by sentiment
facetLeft <- leftTokenScores  %>%
  mutate(word = reorder(word, n))

facetLeftPlot <- ggplot(facetLeft, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Most Common Words faceted by Emotion and Sentiment for Left Leaning YouTubers") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  theme_minimal()
facetLeftPlot

facetRight <- rightTokensScores  %>%
  mutate(word = reorder(word, n))

facetRightPlot <- ggplot(facetRight, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Most Common Words faceted by Emotion and Sentiment for Right Leaning YouTubers") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  theme_minimal()
facetRightPlot

# Number of words associated with each sentiment for left and right

sentimentLeft <- leftTokenScores %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative")) 

sentimentLeftPlot <- ggplot(sentimentLeft, aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab("Sentiment") +
  ylab("Total Number of Words") + 
  labs(title = "Total Number of Words by Sentiment or Emotion for Left Leaning YouTubers") +
  coord_flip() +
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=19,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=19,angle=90,hjust=.5,vjust=.5,face="bold"),
        legend.text=element_text(size=16),
        plot.title = element_text(color="blue",size = 25, face = "bold"))
sentimentLeftPlot

sentimentRight <- rightTokensScores %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative"))

sentimentRightPlot <-ggplot(sentimentRight, aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab("Sentiment") +
  ylab("Total Number of Words") + 
  labs(title = "Total Number of Words by Sentiment or Emotion for Right Leaning YouTubers") +
  coord_flip() +
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=19,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=19,angle=90,hjust=.5,vjust=.5,face="bold"),
        legend.text=element_text(size=16),
        plot.title = element_text(color="red",size = 25, face = "bold"))
sentimentRightPlot

# Plotting most used 5-grams
left5grams <- leftComments %>% #break down sentences into words
  tidytext::unnest_tokens(five_gram, text, token = "ngrams", n = 5) 

right5grams <- rightComments %>% #break down sentences into words
  tidytext::unnest_tokens(five_gram, text, token = "ngrams", n = 5) 

leftTop7_5Grams <- left5grams %>%
  count(five_gram, sort = TRUE) %>%
  top_n(7) %>%
  mutate(five_gram = reorder(five_gram, n)) 

leftTop7_5GramsPlot <- ggplot(leftTop7_5Grams, aes(five_gram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab("5-grams") +
  ylab("Times Mentioned") +
  labs(title = "Most Common 5-grams for Left Leaning YouTubers") +
  coord_flip() +
  theme_minimal()
leftTop7_5GramsPlot

rightTop7_5Grams <- right5grams %>% 
  count(five_gram, sort = TRUE) %>%
  top_n(7) %>%
  mutate(five_gram = reorder(five_gram, n))

rightTop7_5GramsPlot <- ggplot(rightTop7_5Grams, aes(five_gram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab("5-grams") +
  ylab("Times Mentioned") +
  labs(title = "Most Common 5-grams for Right Leaning YouTubers") +
  coord_flip() +
  theme_minimal()
rightTop7_5GramsPlot

# Comparing with Pre-Trump

# Establishing function to get videos posted by chosen channels within 3 months before Trumps election
get_old_videos <- function(channelID){
  videos = yt_search(term="", type="video", channel_id = channelID)
  videos = videos %>%
    mutate(date = as.Date(publishedAt)) %>%
    filter(date > "2016-08-01", date  < "2016-11-01") %>%
    arrange(date)
  return(videos)
}

crowderOldVideos <- get_old_videos(crowderID)
crowderOldVideoStats <- get_video_stats(crowderOldVideos)

shapiroOldVideos <- get_old_videos(shapiroID)
# No videos
# Ben Shapiro's channel did not exist at this time

contraOldVideos <- get_old_videos(contraID)
contraOldVideoStats <- get_video_stats(contraOldVideos)

shaunOldVideos <- get_old_videos(shaunID)
shaunOldVideoStats <- get_video_stats(shaunOldVideos)

# Pre-trump engagement levels

rightOldViews <- sum(crowderOldVideoStats$viewCount) 
rightOldLikes <- sum(crowderOldVideoStats$likeCount) 
rightOldDislikes <- sum(crowderOldVideoStats$dislikeCount) 
rightOldReacts <- rightOldLikes+rightOldDislikes
rightOldReactsRatio <- trunc((rightOldReacts/rightOldViews)*100)
rightOldLeftover <-  100-rightOldReactsRatio
rightOldLikeRatio <- ceiling((rightOldLikes/rightOldReacts)*100)
rightOldDislikeRatio <- floor((rightOldDislikes/rightOldReacts)*100)

leftOldViews <- sum(contraOldVideoStats$viewCount) + sum(shaunOldVideoStats$viewCount)
leftOldLikes <- sum(contraOldVideoStats$likeCount) + sum(shaunOldVideoStats$likeCount)
leftOldDislikes <- sum(contraOldVideoStats$dislikeCount) + sum(shaunOldVideoStats$dislikeCount)
leftOldReacts <- leftOldLikes+leftOldDislikes
leftOldReactsRatio <- trunc((leftOldReacts/leftOldViews)*100)
leftOldLeftover <-  100-leftOldReactsRatio
leftOldLikeRatio <- ceiling((leftOldLikes/leftOldReacts)*100)
leftOldDislikeRatio <- floor((leftOldDislikes/leftOldReacts)*100)

engage3 <- data.frame(Names = c("RightReactRatio","RightLeftover","LeftReactRatio","LeftLeftover"),
                      Num = c(rightOldReactsRatio, rightOldLeftover, leftOldReactsRatio,leftOldLeftover),
                      PoliticalSide = c("Right","Right","Left","Left"),
                      ID = c(2,1,4,3),
                      Type =c("Old"),
                      Kind = c("Ratio")) 

engage4 <- data.frame(Names = c("RightLikeOldRatio","RightDislikeOldRatio","LeftLikeOldRatio","LeftDislikeOldRatio"),
                      Num = c(rightOldLikeRatio,rightOldDislikeRatio,leftOldLikeRatio,leftOldDislikeRatio),
                      PoliticalSide = c("Right","Right", "Left","Left"),
                      ID = c(1,2,3,4),
                      Type =c("Old"))

# Combining now engagement and pre-trump engagement
engageComp = rbind(engage3,engage) %>%
  arrange(ID)

engage2Comp = rbind(engage4,engage2) %>%
  arrange(ID)
engage2Comp$Names <- factor(engage2Comp$Names, levels = engage2Comp$Names)

# Reactions/Video Before and Now 
reactChange <- ggplot(engageComp, aes(x= Type, y=Num, fill = Names))+
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~PoliticalSide) + 
  geom_text(aes(x = Type, y = Num-2.3,label = paste0(Num,"%")), size=4) +
  labs(title = "Engagement(% Reactions/Views) for Left & Right Leaning YouTubers Before Trump Election and Now", x = "Engagement", y= "Percentage(%)") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=19,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=19,angle=90,hjust=.5,vjust=.5,face="bold"),
        legend.text=element_text(size=16),
        plot.title = element_text(color="black",size = 25, face = "bold"))
reactChange

# Likes/Dislikes Percentage Before and Now
likeChange <- ggplot(engage2Comp[which(engage2Comp$Num>0),], aes(x= Names, y=Num, fill = Type))+
  geom_bar(stat = "identity",width=0.5,position = position_dodge()) +
  facet_wrap(~PoliticalSide,scales="free_x") + 
  geom_text(aes(x = Names, y = Num-1.7,label = paste0(Num,"%")), size=4) +
  labs(title = "% Likes(/Reacts) v/s % Dislikes(/Reacts)  for Left & Right Leaning YouTubers Before Trump Election and Now", x = "Dislike and Like Ratios", y= "Percentage(%)") +
  theme(legend.title = element_blank())
likeChange

# Get Old Comments

commentsOldCrowder = lapply(as.character(crowderOldVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsOldCrowder_text = lapply(commentsOldCrowder,function(x){
  as.character(x$textOriginal)
})
commentsOldCrowder_text = tibble(text = Reduce(c, commentsOldCrowder_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

commentsOldContra = lapply(as.character(contraOldVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsOldContra_text = lapply(commentsOldContra,function(x){
  as.character(x$textOriginal)
})
commentsOldContra_text = tibble(text = Reduce(c, commentsOldContra_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

commentsOldShaun = lapply(as.character(shaunOldVideos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
commentsOldShaun_text = lapply(commentsOldShaun,function(x){
  as.character(x$textOriginal)
})
commentsOldShaun_text = tibble(text = Reduce(c, commentsOldShaun_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))

rightOldComments = commentsOldCrowder_text
leftOldComments = rbind(commentsOldContra_text,commentsOldShaun_text)

# Sentiment Analysis of Old Comments
 
tidy_left_old_comments <- leftOldComments %>% #break down sentences into words
  tidytext::unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")

tidy_right_old_comments <- rightOldComments %>% #break down sentences into words
  tidytext::unnest_tokens(word, text) %>%
  anti_join(custom_stop_words, by = "word")

oldSentimentLeft <- tidy_left_old_comments  %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative")) 

oldSentimentLeftPlot <- ggplot(oldSentimentLeft, aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab("Sentiment") +
  ylab("Total Number of Words") + 
  labs(title = "Total Number of Words by Sentiment or Emotion for Left Leaning YouTubers Before Trump Election") +
  coord_flip()
oldSentimentLeftPlot

oldSentimentRight <- tidy_right_old_comments %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative"))

oldSentimentRightPlot <- ggplot(oldSentimentRight, aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab("Sentiment") +
  ylab("Total Number of Words") + 
  labs(title = "Total Number of Words by Sentiment or Emotion for Right Leaning YouTubers Before Trump Election") +
  coord_flip()
oldSentimentRightPlot

# Comparing sentiment pre Trump to Now on Left and Right Channels

# The comparison is between change in share of a particular sentiment
# (number of words associated with a given sentiment/ total number of words)
# on left and right leaning Youtube Channel's comment feeds
# 3 months pre Trump's election to most recent 3 months

# Creating new data frames containing share of sentiment as new column entry
leftCommentsNew <- sentimentLeft %>%
  group_by(sentiment) %>%
  summarize(num = sum(n)) %>%
  mutate(type = "Left",
         percent = num/sum(num))

rightCommentsNew <- sentimentRight %>%
  group_by(sentiment) %>%
  summarize(num = sum(n)) %>%
  mutate(type = "Right",
         percent = num/sum(num)) 

leftCommentsOld <- oldSentimentLeft %>%
  group_by(sentiment) %>%
  summarize(num = sum(n)) %>%
  mutate(type = "Left",
         percent = num/sum(num))

rightCommentsOld <- oldSentimentRight %>%
  group_by(sentiment) %>%
  summarize(num = sum(n)) %>%
  mutate(type = "Right",
         percent = num/sum(num))

# Percentage change in share of sentiment
rightCommentsNew$percent = rightCommentsNew$percent - rightCommentsOld$percent
leftCommentsNew$percent = leftCommentsNew$percent - leftCommentsOld$percent

# Constructing joined Data Frame
masterData <- rbind(leftCommentsNew,rightCommentsNew)

# Ordering
masterData$sentiment = factor(masterData$sentiment, levels=c("negative", "anger","disgust","fear","sadness","positive","joy","trust","anticipation","surprise"))

# Plotting % change in share of sentiment for left and right leaning Youtube Channels

masterPlot<- ggplot(masterData, aes(x= sentiment, y=percent*100, fill = type))+
  geom_bar(stat = "identity",width = 0.5, position = position_dodge()) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Percent Change in Share of Sentiment From Before Trump Election to Now") +
  xlab("Sentiment") +
  ylab("Percent Change") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(colour="grey20",size=17,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=17,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=20,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="bold"),
        legend.text=element_text(size=17),
        plot.title = element_text(size = 27, face = "bold"))

masterPlot

# In the code, there is a lot that can be refined further. We were limited by time, else we would have
# further minimized hand coded data frames and created few additional functions. 
# In my head, I already know exactly how such and such can be implemented. 
# For the most part however, the code can be applied to analyzing sentiment for a chosen YouTube channel,
# between multiple YouTube channels, and between 2 kinds of YouTube channels with minor tweaks. 

