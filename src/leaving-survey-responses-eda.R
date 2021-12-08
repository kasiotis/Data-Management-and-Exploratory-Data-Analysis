library(ProjectTemplate)
load.project()

##############################################################################################################################################
########################################################## Analysis on the leaving step ######################################################
##############################################################################################################################################


barplot(leaving.steps.data.merged$frequency, axisnames = TRUE, names.arg = leaving.steps.data.merged$leaving.step, col = 1:42, axis.lty = 1,
        xlab = "Leaving Step", ylab = "Frequency", ylim = c(0,max(leaving.steps.data.merged$frequency)*1.1), cex.axis=0.7, cex.names=0.7,
        main = "Average number of people that have left at each step (all runs)")



##############################################################################################################################################
########################################################## Analysis on the leaving reason ####################################################
##############################################################################################################################################


#plotting the merged leaving reasons
barplot(leaving.reason.data.merged$frequency, axisnames = TRUE, names.arg = leaving.reason.data.merged$leaving.reason, col = 1:6,
        ylim = c(0,max(leaving.reason.data.merged$frequency)*1.2))

