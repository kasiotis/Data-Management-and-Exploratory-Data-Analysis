#############################################################################################################################################
######################################################### Cleaning the Data set #############################################################
#############################################################################################################################################

#changing the step column so that it represents the step by multiplying the the week by 100 and adding the step number
clean.step.activity = function(step.activity.dataset){
  step.activity.dataset$step = (step.activity.dataset$week_number*100
                                         + step.activity.dataset$step_number)
  return(step.activity.dataset)
}



#############################################################################################################################################
########################################### Getting participation&completion data for each step #############################################
#############################################################################################################################################


#Function that takes in a step.activity data set and extracts the step data about the number of learner that started and finished each step, their
#completion percentage, and lastly the time it took them on average to complete that step.
get.step.participants = function(step.activity.dataset, myrun){
  
  #extracting the number of participants in each step
  step.participants = as.data.frame(table(step.activity.dataset$step[step.activity.dataset$first_visited_at!=""]))
  
  #extracting the number of completionists in each step
  step.completionists = as.data.frame(table(step.activity.dataset$step[step.activity.dataset$last_completed_at!=""]))
  
  #extracting the percentage of people that completed each step
  step.complete.percent = step.completionists$Freq/step.participants$Freq*100
  
  
  #getting all of the steps entries where the step was actually completed
  steps.all = step.activity.dataset$step[step.activity.dataset$last_completed_at!=""]
  
  #getting all the datetime values for the entries for the time of first learner visit, where the step was completed
  started = as.POSIXlt(step.activity.dataset$first_visited_at[step.activity.dataset$last_completed_at!=""])
  
  #getting all the datetime values for the entries for the time of step completion, where the step was completed
  finished = as.POSIXlt(step.activity.dataset$last_completed_at[step.activity.dataset$last_completed_at!=""])
  
  #calculating the difference between the stating time and finish datetime in minute, to see how much time it took learners to complete each step
  time.to.complete = as.numeric(difftime(finished, started, units = "mins"))
  
  #creating a new data frame with the steps, times starting that step, for finishing it and the duration
  all.times = data.frame(step = steps.all,
                    date.started = started,
                    date.finished = finished,
                    time.taken.to.complete = time.to.complete
  )
  
  #creating a vector with only the unique steps out of those that were completes
  steps = unique(all.times$step)
  
  #initializing a vector that will hold the average duration it took learners to complete each step
  average.time.to.complete = 1:length(steps)
  
  #itereting through all of the unique steps that were completed
  for (i in 1:length(steps)) {
    #for each step I find the average time it took the learners to complete that specific step. Then I add it to my vector
    average.time.to.complete[i] = mean(all.times$time.taken.to.complete[all.times$step == steps[i] ])
  }
  
  #return all of my newly created data in a clean data frame, ready for analysis.
  return(
    data.frame(
      run = rep(myrun, length(step.participants$Var1)),
      step = step.participants$Var1,
      step.participants = step.participants$Freq,
      step.completionists = step.completionists$Freq,
      step.complete.percent = step.complete.percent,
      time.to.complete.in.minutes = average.time.to.complete
    )
  )
  
}

#calling both of my functions 7 times, to clean and preprocess my data sets for all of the runs.
step.success.data.1 = get.step.participants(clean.step.activity(cyber.security.1_step.activity),1)
step.success.data.2 = get.step.participants(clean.step.activity(cyber.security.2_step.activity),2)
step.success.data.3 = get.step.participants(clean.step.activity(cyber.security.3_step.activity),3)
step.success.data.4 = get.step.participants(clean.step.activity(cyber.security.4_step.activity),4)
step.success.data.5 = get.step.participants(clean.step.activity(cyber.security.5_step.activity),5)
step.success.data.6 = get.step.participants(clean.step.activity(cyber.security.6_step.activity),6)
step.success.data.7 = get.step.participants(clean.step.activity(cyber.security.7_step.activity),7)



#############################################################################################################################################
################################################# Doing data combining and data merging  ####################################################
#############################################################################################################################################

get.merged.step.succes.data = function(allsteps){
  
  #getting the unique names of the steps from my new set
  step.names = unique(allsteps$step)
  
  #initializing vectors that will hold the numbers of participants, completionists and times taken to complete each step, for each step
  all.participants.frequencies = 1:length(step.names)
  all.completionists.frequencies = 1:length(step.names)
  all.times.frequencies = 1:length(step.names)
  
  #extracting the numbers of participants, completionists and times taken to complete each step, for each step
  for (i in 1:length(step.names)) {
    all.participants.frequencies[i] = mean(allsteps$step.participants[allsteps$step == step.names[i]])
    all.completionists.frequencies[i] = mean(allsteps$step.completionists[allsteps$step == step.names[i]])
    all.times.frequencies[i] = mean(allsteps$time.to.complete.in.minutes[allsteps$step == step.names[i]])
  }
  
  return(data.frame(
    step = step.names,
    step.participants = all.participants.frequencies,
    step.completionists = all.completionists.frequencies,
    step.complete.percent = all.completionists.frequencies/all.participants.frequencies*100,
    time.to.complete.in.minutes = all.times.frequencies
  )
  )
}

#merging my step.success datasets into on dataframe
step.success.data.allruns = merge(merge(merge(merge(
  step.success.data.3,
  step.success.data.4, all=TRUE),
  step.success.data.5, all=TRUE),
  step.success.data.6, all=TRUE),
  step.success.data.7, all=TRUE)


#reconstructing a merged data frame out of the combined runs
step.success.data.merged = get.merged.step.succes.data(step.success.data.allruns)

