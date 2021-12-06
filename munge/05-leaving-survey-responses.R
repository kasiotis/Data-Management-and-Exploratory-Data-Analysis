library(stringi)

##################################################### Cleaning the Data set ###########################################################


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



################################################## Pre-processing the Data set ########################################################


#function that takes in a leaving survey dataset and returns data about the leaving steps
get.leaving.step.data = function(leaving.survey.dataset){
  
  #finding the frequency of the each step at which students left. Also calling the clean function implemented above, to clean my data
  #before using it to construct new data.
  leaving.step = as.data.frame(table(clean.leaving.survey.dataset(leaving.survey.dataset)$last_completed_step))

  #returning a data frame with all of the steps that were found to be leaving steps along with the number of people that left at each step.
  return(data.frame(leaving.steps = leaving.step$Var1,
                    frequency = leaving.step$Freq))
}

#calling the function that creates the leaving step data, for all the runs that has such datasets
leaving.step.data.4 = get.leaving.step.data(cyber.security.4_leaving.survey.responses)
leaving.step.data.5 = get.leaving.step.data(cyber.security.5_leaving.survey.responses)
leaving.step.data.6 = get.leaving.step.data(cyber.security.6_leaving.survey.responses)
leaving.step.data.7 = get.leaving.step.data(cyber.security.7_leaving.survey.responses)



#function that takes in a leaving survey dataset and returns data about the leaving reasons
get.leaving.reason.data = function(leaving.survey.dataset){
  
  #cleaning the dataset in order for the leaving reason to be rid of all non=UTF-8 characters
  leaving.survey.dataset = clean.leaving.survey.dataset(leaving.survey.dataset)
  
  #finding the the frequency of the each leaving reason, while excluding the reasons "other" and "prefer not to say". Also calling the 
  #clean function implemented above, to clean my data before using it to construct new data.
  leaving.reason = as.data.frame(table(clean.leaving.survey.dataset(leaving.survey.dataset)$leaving_reason
                                       [leaving.survey.dataset$leaving_reason!="Other"&
                                        leaving.survey.dataset$leaving_reason!="I prefer not to say"]))

  #returning a data frame with all of the reason that were given for leaving along with the number of people that left because of that reason.
  return(data.frame(leaving.reasons = leaving.reason$Var1,
                    frequency = leaving.reason$Freq))
}

#calling the function that creates the leaving reason data, for all the runs that has such datasets
leaving.reason.data.4 = get.leaving.reason.data(cyber.security.4_leaving.survey.responses)
leaving.reason.data.5 = get.leaving.reason.data(cyber.security.5_leaving.survey.responses)
leaving.reason.data.6 = get.leaving.reason.data(cyber.security.6_leaving.survey.responses)
leaving.reason.data.7 = get.leaving.reason.data(cyber.security.7_leaving.survey.responses)
