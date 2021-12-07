library(ProjectTemplate)
load.project()


#merging all the video data sets that from all runs
allvideos = merge(merge(merge(merge(
  cyber.security.3_video.stats,
  cyber.security.4_video.stats, all=TRUE),
  cyber.security.5_video.stats, all=TRUE),
  cyber.security.6_video.stats, all=TRUE),
  cyber.security.7_video.stats, all=TRUE)


#getting the unique names of the video titles from my new set
video.names = unique(allvideos$title)

#initializing vectors that will hold the values of video durations and fully watched percentages
all.video.durations = 1:length(video.names)
all.video.fully.viewed = 1:length(video.names)

#extracting the video durations and fully watched percentages averages from all runs
for (i in 1:length(video.names)) {
  all.video.durations[i] = mean(allvideos$video_duration[allvideos$title == video.names[i]])
  all.video.fully.viewed[i] = mean(allvideos$viewed_ninetyfive_percent[allvideos$title == video.names[i]])
}

#Merging the findings of all runs into one single data frame
video.data.merged = data.frame(
  title = video.names,
  duration = all.video.durations,
  fully.viewed.percent = all.video.fully.viewed
)
#reordering the data frame by the video duration
video.data.merged = video.data.merged[order(video.data.merged$duration),]


plot(video.data.merged$duration, video.data.merged$fully.viewed.percent, type="p", xlab = "Video Duration",
     ylab = "Percentage of people that fully watched", col="blue", main = "Effect of video duration on percentage of people that fully watch")
abline(lm(video.data.merged$fully.viewed.percent ~ video.data.merged$duration), col="red")

