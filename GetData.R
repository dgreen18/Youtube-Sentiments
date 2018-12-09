library(tuber)
library(syuzhet)

#OAuth and Connection
app_id <- "830360114408-ev1q4bqv76daq7o0i4b2dj48dehomt36.apps.googleusercontent.com"
app_secret <- "OjLU0iwI5yJjk9axkpwHUjgC"
yt_oauth(app_id, app_secret)

#Get Video Stats
climateNoah <- "YRGgbcU7FmI"
climNo <- get_stats(video_id=climateNoah)
# https://www.youtube.com/watch?v=YRGgbcU7FmI
# Trump Contradicts His Own Administration’s Climate Change Report | The Daily Show

#Get Comments
res <- get_all_comments(c(video_id=climateNoah))
str(res)
comments <- iconv(res$textOriginal, to = 'UTF-8')
class(comments)

# Get sentiment scores
score <- get_nrc_sentiment(comments)
score$neutral <- ifelse(score$negative+score$positive == 0, 1, 0) #neutral score
score

#Bar Plot
barplot(100*colSums(score)/sum(score),
        las = 2,
        col = rainbow(10),
        ylab = 'Percentage',main = 'Sentiment Scores for Youtube Comments')
