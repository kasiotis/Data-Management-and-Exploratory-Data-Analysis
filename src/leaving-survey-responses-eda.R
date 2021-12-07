library(ProjectTemplate)
load.project()

##############################################################################################################################################
########################################################## Analysis on the leaving reason ####################################################
##############################################################################################################################################



#merging all the leaving reason data sets that I have created for all runs
allreasons = merge(merge(merge(
  leaving.reason.data.4,
  leaving.reason.data.5, all=TRUE),
  leaving.reason.data.6, all=TRUE),
  leaving.reason.data.7, all=TRUE)
 

#getting the unique names of the age ranges from my new set
reason.names = unique(allreasons$leaving.reasons)

#initializing vectors that will hold the numbers of enrollments and graduations of each country
all.reason.frequencies = 1:length(reason.names)

#extracting the enrollments and graduations of each country from all runs
for (i in 1:length(reason.names)) {
  all.reason.frequencies[i] = mean(allreasons$frequency[allreasons$leaving.reasons == reason.names[i]])
}

#recalculating the the graduate percentage of each country by merging the findings of all runs
leaving.reason.data.merged = data.frame(
  leaving.reason = reason.names,
  frequency = all.reason.frequencies
)

#plotting the merged leaving reasons
barplot(leaving.reason.data.merged$frequency, axisnames = TRUE, names.arg = leaving.reason.data.merged$leaving.reason, col = 1:6)


##############################################################################################################################################
########################################################## Analysis on the leaving step ######################################################
##############################################################################################################################################


#merging all the leaving reason data sets that I have created for all runs
allleavingsteps = merge(merge(merge(
  leaving.step.data.4,
  leaving.step.data.5, all=TRUE),
  leaving.step.data.6, all=TRUE),
  leaving.step.data.7, all=TRUE)


#getting the unique names of the age ranges from my new set
step.names = unique(allleavingsteps$leaving.steps)

#initializing vectors that will hold the numbers of enrollments and graduations of each country
all.step.frequencies = 1:length(step.names)

#extracting the enrollments and graduations of each country from all runs
for (i in 1:length(step.names)) {
  all.step.frequencies[i] = mean(allleavingsteps$frequency[allleavingsteps$leaving.steps == step.names[i]])
}

#recalculating the the graduate percentage of each country by merging the findings of all runs
leaving.steps.data.merged = data.frame(
  leaving.step = as.numeric(as.character(step.names)),
  frequency = all.step.frequencies
)
#reordering the dataset by the leaving step so that it can be plotted nicely
leaving.steps.data.merged =  leaving.steps.data.merged[order(leaving.steps.data.merged$leaving.step),]

barplot(leaving.steps.data.merged$frequency, axisnames = TRUE, names.arg = leaving.steps.data.merged$leaving.step, col = 1:42, axis.lty = 1,
        xlab = "Leaving Step", ylab = "Frequency", ylim = c(0,max(leaving.steps.data.merged$frequency)*1.1), cex.axis=0.7, cex.names=0.7,
        main = "Average number of people that have left at each step (all runs)")


