#load projTemp library
library(ProjectTemplate)
library(dplyr)

load.project()

##############################################################################################################################################
############################  Checking the statistics about the steps and how learners engaged with them   ###################################
##############################################################################################################################################


get.step.data.plot = function(step.activity.dataset) {
  
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

get.step.data.plot(step.success.data.6)
get.step.data.plot(step.success.data.2)

#inspecting the relationships of the variables for the steps
pairs(step.success.data.1)


