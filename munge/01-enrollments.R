####################### recording enrollments and completions ###################################

get.graduate.general.data = function(){
  
  #finding the number of enrolments in each run
  enrollments= c(
    length(cyber.security.1_enrolments$enrolled_at[cyber.security.1_enrolments$enrolled_at!=""]),
    length(cyber.security.2_enrolments$enrolled_at[cyber.security.2_enrolments$enrolled_at!=""]),
    length(cyber.security.3_enrolments$enrolled_at[cyber.security.3_enrolments$enrolled_at!=""]),
    length(cyber.security.4_enrolments$enrolled_at[cyber.security.4_enrolments$enrolled_at!=""]),
    length(cyber.security.5_enrolments$enrolled_at[cyber.security.5_enrolments$enrolled_at!=""]),
    length(cyber.security.6_enrolments$enrolled_at[cyber.security.6_enrolments$enrolled_at!=""]),
    length(cyber.security.7_enrolments$enrolled_at[cyber.security.7_enrolments$enrolled_at!=""])
  )
  #finding the number of graduates in each run
  graduated= c(
    length(cyber.security.1_enrolments$fully_participated_at[cyber.security.1_enrolments$fully_participated_at!=""]),
    length(cyber.security.2_enrolments$fully_participated_at[cyber.security.2_enrolments$fully_participated_at!=""]),
    length(cyber.security.3_enrolments$fully_participated_at[cyber.security.3_enrolments$fully_participated_at!=""]),
    length(cyber.security.4_enrolments$fully_participated_at[cyber.security.4_enrolments$fully_participated_at!=""]),
    length(cyber.security.5_enrolments$fully_participated_at[cyber.security.5_enrolments$fully_participated_at!=""]),
    length(cyber.security.6_enrolments$fully_participated_at[cyber.security.6_enrolments$fully_participated_at!=""]),
    length(cyber.security.7_enrolments$fully_participated_at[cyber.security.7_enrolments$fully_participated_at!=""])
  )
  
  #finding the percentage of people who complete the course in each run
  gradPercent= c(
    graduated[1]/enrollments[1]*100,
    graduated[2]/enrollments[2]*100,
    graduated[3]/enrollments[3]*100,
    graduated[4]/enrollments[4]*100,
    graduated[5]/enrollments[5]*100,
    graduated[6]/enrollments[6]*100,
    graduated[7]/enrollments[7]*100
  )
  
  #creating a data frame of enrollments and completions for the course in each run
  return(data.frame(run = factor(rep(1:7)),
                    enrollments = enrollments,
                    graduations = graduated,
                    graduation.percent = gradPercent)
         )

}

grad.general.data = get.graduate.general.data()



########################################  enrollment and graduation data based on learners gender  ##############################################


get.graduate.gender.data = function(enrolments.dataset){
  
  genders.enrolled = as.data.frame(table(enrolments.dataset$gender))
  genders.graduates = as.data.frame(table(enrolments.dataset$gender[enrolments.dataset$fully_participated_at!=""]))
  
  
  both.data.frames = merge(genders.enrolled, genders.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  both.data.frames[is.na(both.data.frames)] = 0
  
  return(
    data.frame(
      gender = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}

grad.gender.data.1 = get.graduate.gender.data(cyber.security.1_enrolments)
grad.gender.data.2 = get.graduate.gender.data(cyber.security.2_enrolments)
grad.gender.data.3 = get.graduate.gender.data(cyber.security.3_enrolments)
grad.gender.data.4 = get.graduate.gender.data(cyber.security.4_enrolments)
grad.gender.data.5 = get.graduate.gender.data(cyber.security.5_enrolments)
grad.gender.data.6 = get.graduate.gender.data(cyber.security.6_enrolments)
grad.gender.data.7 = get.graduate.gender.data(cyber.security.7_enrolments)




###################################  enrollment and graduation data based on learners detected country  ##########################################


get.graduate.country.data = function(enrolments.dataset){
  
  countries.enrolled = as.data.frame(table(enrolments.dataset$detected_country))
  countries.graduates = as.data.frame(table(enrolments.dataset$detected_country[enrolments.dataset$fully_participated_at!=""]))
  
  
  both.data.frames = merge(countries.enrolled, countries.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  both.data.frames[is.na(both.data.frames)] = 0
  
  return(
    data.frame(
      country = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}


grad.country.data.1 = get.graduate.country.data(cyber.security.1_enrolments)
grad.country.data.2 = get.graduate.country.data(cyber.security.2_enrolments)
grad.country.data.3 = get.graduate.country.data(cyber.security.3_enrolments)
grad.country.data.4 = get.graduate.country.data(cyber.security.4_enrolments)
grad.country.data.5 = get.graduate.country.data(cyber.security.5_enrolments)
grad.country.data.6 = get.graduate.country.data(cyber.security.6_enrolments)
grad.country.data.7 = get.graduate.country.data(cyber.security.7_enrolments)



#######################################  enrollment and graduation data based on learners age group  #############################################


get.graduate.age.data = function(enrolments.dataset){
  
  ages.enrolled = as.data.frame(table(enrolments.dataset$age_range))
  ages.graduates = as.data.frame(table(enrolments.dataset$age_range[enrolments.dataset$fully_participated_at!=""]))
  
  
  both.data.frames = merge(ages.enrolled, ages.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  both.data.frames[is.na(both.data.frames)] = 0
  
  both.data.frames$Var1 = both.data.frames$Var1 <- factor(both.data.frames$Var1, levels = c("<18", "18-25", "26-35", "36-45", "46-55", "56-65", ">65", "Unknown"), ordered = TRUE)
  
  #creating a data frame with all of the newly created information
  result = data.frame(
    age.range = both.data.frames$Var1,
    enrollements = both.data.frames$Freq.x,
    graduates = both.data.frames$Freq.y,
    graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  #returning the newly created data frame ordered by the age.range
  return(result[order(result$age.range),])
}


grad.age.data.1 = get.graduate.age.data(cyber.security.1_enrolments)
grad.age.data.2 = get.graduate.age.data(cyber.security.2_enrolments)
grad.age.data.3 = get.graduate.age.data(cyber.security.3_enrolments)
grad.age.data.4 = get.graduate.age.data(cyber.security.4_enrolments)
grad.age.data.5 = get.graduate.age.data(cyber.security.5_enrolments)
grad.age.data.6 = get.graduate.age.data(cyber.security.6_enrolments)
grad.age.data.7 = get.graduate.age.data(cyber.security.7_enrolments)



#######################################  enrollment and graduation data based on learners employment  ############################################


get.graduate.employment.data = function(enrolments.dataset){
  
  employment.enrolled = as.data.frame(table(enrolments.dataset$employment_status))
  employment.graduates = as.data.frame(table(enrolments.dataset$employment_status[enrolments.dataset$fully_participated_at!=""]))
  
  
  both.data.frames = merge(employment.enrolled, employment.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  both.data.frames[is.na(both.data.frames)] = 0
  
  return(
    data.frame(
      employment.status = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}


grad.employment.data.1 = get.graduate.employment.data(cyber.security.1_enrolments)
grad.employment.data.2 = get.graduate.employment.data(cyber.security.2_enrolments)
grad.employment.data.3 = get.graduate.employment.data(cyber.security.3_enrolments)
grad.employment.data.4 = get.graduate.employment.data(cyber.security.4_enrolments)
grad.employment.data.5 = get.graduate.employment.data(cyber.security.5_enrolments)
grad.employment.data.6 = get.graduate.employment.data(cyber.security.6_enrolments)
grad.employment.data.7 = get.graduate.employment.data(cyber.security.7_enrolments)



#######################################  enrollment and graduation data based on learners education  ############################################


get.graduate.education.data = function(enrolments.dataset){
  
  education.enrolled = as.data.frame(table(enrolments.dataset$highest_education_level))
  education.graduates = as.data.frame(table(enrolments.dataset$highest_education_level[enrolments.dataset$fully_participated_at!=""]))
  
  
  both.data.frames = merge(education.enrolled, education.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  both.data.frames[is.na(both.data.frames)] = 0
  
  return(
    data.frame(
      education.status = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}


grad.education.data.1 = get.graduate.education.data(cyber.security.1_enrolments)
grad.education.data.2 = get.graduate.education.data(cyber.security.2_enrolments)
grad.education.data.3 = get.graduate.education.data(cyber.security.3_enrolments)
grad.education.data.4 = get.graduate.education.data(cyber.security.4_enrolments)
grad.education.data.5 = get.graduate.education.data(cyber.security.5_enrolments)
grad.education.data.6 = get.graduate.education.data(cyber.security.6_enrolments)
grad.education.data.7 = get.graduate.education.data(cyber.security.7_enrolments)
