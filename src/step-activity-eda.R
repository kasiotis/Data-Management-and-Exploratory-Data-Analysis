#load projTemp library
library(ProjectTemplate)
library(dplyr)

load.project()


##############################################################################################################################################
############################  Checking the statistics about the steps and how learners engaged with them   ###################################
##############################################################################################################################################


get.merged.step.data.plot = function(step.activity.dataset) {
  
  par(mfrow=c(3,1))
  
  #plotting the number of people that started vs those that finished each step
  plot(as.numeric(step.activity.dataset$step), step.activity.dataset$step.participants, type = "p", xaxt="n",
       xlab = "Step number", ylab = "Number of people", col="blue", ylim = c(0,max(step.activity.dataset$step.participants)*1.1),
       main = "Number of participations against number of completions in each step")
  lines(as.numeric(step.activity.dataset$step), step.activity.dataset$step.completionists, col="red", type = "p")
  #putting the step values on the X axis
  axis(1, at=1:length(step.activity.dataset$step), labels = step.activity.dataset$step)
  #creating a legend to show what points correspond to what values
  legend("topright", legend = c("Participants","Completionists"), cex = 0.60, fill=c("blue","red"), text.font = 4)
  
  #plotting the percentage of completion for each step
  barplot(step.activity.dataset$step.complete.percent, axisnames = TRUE, names.arg = step.activity.dataset$step,
          ylim = c(0,100),axis.lty = 1, xlab = "Step number", ylab = "Percentage of completions", col = 1:length(step.activity.dataset$step),
          main = "Percentage of Completions out of those that started each step")
  
  #plotting the time it took to complete each step in minutes
  barplot(step.activity.dataset$time.to.complete.in.minutes, axisnames = TRUE, names.arg = step.activity.dataset$step,
          ylim = c(0,max(step.activity.dataset$time.to.complete.in.minutes)),axis.lty = 1, xlab = "Step number", ylab = "Time to complete (minutes)", col = 1:length(step.activity.dataset$step),
          main = "Time it took to complete each step on average")
  
}

#plotting 3 things about the merged step data
#1st:   Number of participations against number of completions in each step
#2nd:   Percentage of Completions out of those that started each step
#3rd:   Time it took to complete each step on average
get.merged.step.data.plot(step.success.data.merged)


#inspecting the relationships of the variables for the steps
pairs(step.success.data.merged)



########################### Cross-run plots #############################

get.combined.step.data.plot = function(all.step.activity.dataset) {
  
  par(mfrow=c(2,1))
  
  #plotting the percentage of completion for runs 2 to 7
  plot(as.numeric(allsteps$step[allsteps$run==3]), allsteps$step.complete.percent[allsteps$run==3],xaxt="n", type = "l", col=3, ylim = c(40,100),
       xlab = "Steps", ylab = "Percentage of Completion", main = "Percentage of Completions out of those that started each step")
  lines(as.numeric(allsteps$step[allsteps$run==4]), allsteps$step.complete.percent[allsteps$run==4], type = "l", col=4)
  lines(as.numeric(allsteps$step[allsteps$run==5]), allsteps$step.complete.percent[allsteps$run==5], type = "l", col=5)
  lines(as.numeric(allsteps$step[allsteps$run==6]), allsteps$step.complete.percent[allsteps$run==6], type = "l", col=6)
  lines(as.numeric(allsteps$step[allsteps$run==7]), allsteps$step.complete.percent[allsteps$run==7], type = "l", col=7)
  axis(1, at=1:length(allsteps$step[allsteps$run==3]), labels = allsteps$step[allsteps$run==3], cex.axis=0.7, cex.names=0.7)
  legend("bottomright", legend = c("run3","run4","run5","run6","run7"), cex = 0.50, fill=3:7, text.font = 4)
  
  
  #plotting the time it took on average to complete each step for runs 2 to 7
  plot(as.numeric(allsteps$step[allsteps$run==3]), allsteps$time.to.complete.in.minutes[allsteps$run==3],xaxt="n", type = "l", col=3, ylim = c(0,8000),
       xlab = "Step number", ylab = "Time to complete (minutes)", main = "Time it took to complete each step on average")
  lines(as.numeric(allsteps$step[allsteps$run==4]), allsteps$time.to.complete.in.minutes[allsteps$run==4], type = "l", col=4)
  lines(as.numeric(allsteps$step[allsteps$run==5]), allsteps$time.to.complete.in.minutes[allsteps$run==5], type = "l", col=5)
  lines(as.numeric(allsteps$step[allsteps$run==6]), allsteps$time.to.complete.in.minutes[allsteps$run==6], type = "l", col=6)
  lines(as.numeric(allsteps$step[allsteps$run==7]), allsteps$time.to.complete.in.minutes[allsteps$run==7], type = "l", col=7)
  axis(1, at=1:length(allsteps$step[allsteps$run==3]), labels = allsteps$step[allsteps$run==3], cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run3","run4","run5","run6","run7"), cex = 0.50, fill=3:7, text.font = 4)
  
}

get.combined.step.data.plot(allsteps)
