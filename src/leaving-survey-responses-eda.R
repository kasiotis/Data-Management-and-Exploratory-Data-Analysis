library(ProjectTemplate)
#load.project()

##############################################################################################################################################
########################################################## Analysis on the leaving step ######################################################
##############################################################################################################################################



leaving.step.merged.plot = function(){
  #plotting the average number of people that have left at each step (merged dataset version of runs 4-7)
  barplot(leaving.steps.data.merged$frequency, axisnames = TRUE, names.arg = leaving.steps.data.merged$leaving.step, col = 1:42, axis.lty = 1,
          xlab = "Leaving Step", ylab = "Frequency", ylim = c(0,max(leaving.steps.data.merged$frequency)*1.1), cex.axis=0.7, cex.names=0.50,
          main = "Average number of people who left (Runs 4-7 merged)")
}
leaving.step.merged.plot()

#finding the steps with the highest leaving frequency (more that 5) throughout all runs 
most.leaving.steps.crossrun = leaving.steps.data.merged[leaving.steps.data.merged$frequency>4,]

#reordering the steps according to their leaving frequency (from high to low) and showing only the top 4
(most.leaving.steps.crossrun = head(most.leaving.steps.crossrun[order(most.leaving.steps.crossrun$frequency, decreasing = TRUE),],4))



##############################################################################################################################################
########################################################## Analysis on the leaving reason ####################################################
##############################################################################################################################################


leaving.reason.merged.plot = function(){
  #plotting the average number of people that have left for each reason (merged dataset version of runs 4-7)
  barplot(leaving.reason.data.merged$frequency, axisnames = TRUE, names.arg = leaving.reason.data.merged$leaving.reason, col = 1:6, 
          cex.axis=0.7, cex.names=0.50, ylim = c(0,max(leaving.reason.data.merged$frequency)*1.2), xlab = "Leaving Reasons", ylab = "Frequency",
          main = "Average number of people who left (Runs 4-7 merged)")
}
leaving.reason.merged.plot()


#finding the reasons with the highest leaving frequency (more that 5) throughout all runs 
most.leaving.reasons.crossrun = leaving.reason.data.merged[leaving.reason.data.merged$frequency>5,]

#reordering the reasons according to their leaving frequency (from high to low) and showing only the top 4
(most.leaving.reasons.crossrun = head(most.leaving.reasons.crossrun[order(most.leaving.reasons.crossrun$frequency, decreasing = TRUE),],4))

