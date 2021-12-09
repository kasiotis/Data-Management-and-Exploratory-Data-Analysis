library(stringi)
#######################################################################################################################################
##################################################### Cleaning the Data set ###########################################################
#######################################################################################################################################


#changing the step column so that it represents the step by multiplying the the week by 100 and adding the step number
clean.leaving.survey.dataset = function(leaving.survey.dataset){
  
  #modifying the step columns
  leaving.survey.dataset$last_completed_step = (leaving.survey.dataset$last_completed_week_number*100
                                + leaving.survey.dataset$last_completed_step_number)
  
  #replacing all non-UTF-8 characters with the "REPLACEMENT CHARACTER"
  leaving.survey.dataset$leaving_reason = stri_enc_toutf8(leaving.survey.dataset$leaving_reason, is_unknown_8bit = TRUE, validate = FALSE)
  
  #replacing the spelling errors that are now marked with the "REPLACEMENT CHARACTER" to be the correct piece of text
  leaving.survey.dataset$leaving_reason[leaving.survey.dataset$leaving_reason == "I don\Ufffd\Ufffd\Ufffdt have enough time"] = 
    "I don't have enough time"
  leaving.survey.dataset$leaving_reason[leaving.survey.dataset$leaving_reason == "The course wasn\Ufffd\Ufffd\Ufffdt what I expected"] = 
    "The course wasn't what I expected"
  leaving.survey.dataset$leaving_reason[leaving.survey.dataset$leaving_reason == "The course won\Ufffd\Ufffd\Ufffdt help me reach my goals"] = 
    "The course won't help me reach my goals"
  
  return(leaving.survey.dataset)
}



#######################################################################################################################################
################################################## Pre-processing the Data set ########################################################
#######################################################################################################################################



##################################################  Working on the Leaving step  ########################################################


########################## Extracting the frequency for each leaving step ###############################


#function that takes in a leaving survey dataset and returns data about the leaving steps
get.leaving.step.data = function(leaving.survey.dataset, myrun){
  
  #finding the frequency of the each step at which students left. Also calling the clean function implemented above, to clean my data
  #before using it to construct new data.
  leaving.step = as.data.frame(table(clean.leaving.survey.dataset(leaving.survey.dataset)$last_completed_step))

  #returning a data frame with all of the steps that were found to be leaving steps along with the number of people that left at each step.
  return(data.frame(
    run = rep(myrun, length(leaving.step$Var1)),
    leaving.steps = leaving.step$Var1,
    frequency = leaving.step$Freq))
}

#calling the function that creates the leaving step data, for all the runs that have such datasets
leaving.step.data.4 = get.leaving.step.data(cyber.security.4_leaving.survey.responses,4)
leaving.step.data.5 = get.leaving.step.data(cyber.security.5_leaving.survey.responses,5)
leaving.step.data.6 = get.leaving.step.data(cyber.security.6_leaving.survey.responses,6)
leaving.step.data.7 = get.leaving.step.data(cyber.security.7_leaving.survey.responses,7)


###################### producing a merged set with averaged step frequencies of all the runs ##############################


get.leaving.steps.data.merged = function(allleavingsteps) {
  
  #getting the unique names of the steps from my new set
  step.names = unique(allleavingsteps$leaving.steps)
  
  #initializing vectors that will hold the frequencies of each leaving step
  all.step.frequencies = 1:length(step.names)
  
  #extracting the average frequencies for each leaving step that was found in all runs
  for (i in 1:length(step.names)) {
    all.step.frequencies[i] = mean(allleavingsteps$frequency[allleavingsteps$leaving.steps == step.names[i]])
  }
  
  #Creating a new data frame with my extracted data
  leaving.steps.data.merged = data.frame(
    leaving.step = as.numeric(as.character(step.names)),
    frequency = all.step.frequencies
  )
  #reordering the dataset by the leaving step (low-High) so that it can be plotted nicely, and returning it
  return(leaving.steps.data.merged[order(leaving.steps.data.merged$leaving.step),])
}

#centralizing all the leaving step data sets that I have created for runs 4-7
leaving.steps.data.allruns = merge(merge(merge(
  leaving.step.data.4,
  leaving.step.data.5, all=TRUE),
  leaving.step.data.6, all=TRUE),
  leaving.step.data.7, all=TRUE)

#calling the function I just created to get the averaged version of runs 4-7
leaving.steps.data.merged = get.leaving.steps.data.merged(leaving.steps.data.allruns)


#################################################  Working on the Leaving Reason  #######################################################


########################## Extracting the frequency for each leaving reason ###############################


#function that takes in a leaving survey dataset and returns data about the leaving reasons
get.leaving.reason.data = function(leaving.survey.dataset, myrun){
  
  #cleaning the dataset in order for the leaving reason to be rid of all non=UTF-8 characters
  leaving.survey.dataset = clean.leaving.survey.dataset(leaving.survey.dataset)
  
  #finding the the frequency of the each leaving reason, while excluding the reasons "other" and "prefer not to say". Also calling the 
  #clean function implemented above, to clean my data before using it to construct new data.
  leaving.reason = as.data.frame(table(clean.leaving.survey.dataset(leaving.survey.dataset)$leaving_reason
                                       [leaving.survey.dataset$leaving_reason!="Other"&
                                        leaving.survey.dataset$leaving_reason!="I prefer not to say"]))

  #returning a data frame with all of the reason that were given for leaving along with the number of people that left because of that reason.
  return(data.frame(
    run = rep(myrun, length(leaving.reason$Var1)),
    leaving.reasons = leaving.reason$Var1,
    frequency = leaving.reason$Freq))
}

#calling the function that creates the leaving reason data, for all the runs that has such datasets
leaving.reason.data.4 = get.leaving.reason.data(cyber.security.4_leaving.survey.responses,4)
leaving.reason.data.5 = get.leaving.reason.data(cyber.security.5_leaving.survey.responses,5)
leaving.reason.data.6 = get.leaving.reason.data(cyber.security.6_leaving.survey.responses,6)
leaving.reason.data.7 = get.leaving.reason.data(cyber.security.7_leaving.survey.responses,7)


###################### producing a averaged set of all the leaving reason sets ############################


get.leaving.reason.data.merged = function(allreasons){
  
  
  #getting the unique names of the age ranges from my new set
  reason.names = unique(allreasons$leaving.reasons)
  
  #initializing vectors that will hold the frequencies of each leaving reason
  all.reason.frequencies = 1:length(reason.names)
  
  #extracting the total frequencies of each leaving reason from all runs
  for (i in 1:length(reason.names)) {
    all.reason.frequencies[i] = mean(allreasons$frequency[allreasons$leaving.reasons == reason.names[i]])
  }
  
  #Creating a new data frame with my extracted data
   data.frame(
    leaving.reason = reason.names,
    frequency = all.reason.frequencies
  )
}

#merging all the leaving reason data sets that I have created for runs 4-7
leaving.reason.data.allruns = merge(merge(merge(
  leaving.reason.data.4,
  leaving.reason.data.5, all=TRUE),
  leaving.reason.data.6, all=TRUE),
  leaving.reason.data.7, all=TRUE)

#calling the function I just created to get the averaged version of runs 4-7
leaving.reason.data.merged = get.leaving.reason.data.merged(leaving.reason.data.allruns)
