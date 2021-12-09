library(ProjectTemplate)
load.project()

video.duration.plot = function(){
  #plotting the relationship of the video duration of each video against the percentage of people that watched the whole video
  plot(video.data.merged$duration, video.data.merged$fully.viewed.percent, type="p", xlab = "Video Duration (minutes)",
       ylab = "Percentage of people that fully watched", col="blue", main = "Effect of video duration on percentage of people that fully watch")
  abline(lm(video.data.merged$fully.viewed.percent ~ video.data.merged$duration), col="red")
  
}

