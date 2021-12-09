##################################################################################################################################################
############################################### recording enrollments and completions ############################################################
##################################################################################################################################################
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



#############################################################################################################################################
########################################  enrollment and graduation data based on learners gender  ##########################################
#############################################################################################################################################


get.graduate.gender.data = function(enrolments.dataset, myrun){
  
  #extracting the numbers of enrollments and graduates for each gender
  genders.enrolled = as.data.frame(table(enrolments.dataset$gender))
  genders.graduates = as.data.frame(table(enrolments.dataset$gender[enrolments.dataset$fully_participated_at!=""]))
  
  #merging the two above created data frames into one
  both.data.frames = merge(genders.enrolled, genders.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  #removing the rows where after the merge, the resulting frequency was 0
  both.data.frames[is.na(both.data.frames)] = 0
  
  #returning my newly created data for enrollments, graduations, run and graduation percentage for each gender
  return(
    data.frame(
      run = rep(myrun, length(genders.enrolled$Var1)),
      gender = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}

#calling the function from above to create 7 new sets for the selected/calculated data
grad.gender.data.1 = get.graduate.gender.data(cyber.security.1_enrolments,1)
grad.gender.data.2 = get.graduate.gender.data(cyber.security.2_enrolments,2)
grad.gender.data.3 = get.graduate.gender.data(cyber.security.3_enrolments,3)
grad.gender.data.4 = get.graduate.gender.data(cyber.security.4_enrolments,4)
grad.gender.data.5 = get.graduate.gender.data(cyber.security.5_enrolments,5)
grad.gender.data.6 = get.graduate.gender.data(cyber.security.6_enrolments,6)
grad.gender.data.7 = get.graduate.gender.data(cyber.security.7_enrolments,7)


############################  merging all gender data sets into one complete set #####################


get.graduate.gender.data.merged = function(allgenders) {
  
  #getting the unique genders from all the runs
  gender.names = unique(allgenders$gender)
  
  #initializing vectors that will hold the numbers of enrollments and graduations of each gender
  all.gender.enrollments = 1:length(gender.names)
  all.gender.graduations = 1:length(gender.names)
  
  #extracting the total number of enrollments and graduations of each gender from all runs
  for (i in 1:length(gender.names)) {
    all.gender.enrollments[i] = sum(allgenders$enrollements[allgenders$gender == gender.names[i]])
    all.gender.graduations[i] = sum(allgenders$graduates[allgenders$gender == gender.names[i]])
  }
  
  #recalculating the the graduate percentage of each gender by merging the findings of all runs
  return(
    data.frame(
      gender = unique(allgenders$gender),
      grad.percent = all.gender.graduations/all.gender.enrollments*100
    )
  )
}

#grouping all the gender graduate data sets that I have created for all runs
grad.gender.data.allruns = merge(merge(merge(merge(merge(merge(
  grad.gender.data.1,
  grad.gender.data.2, all=TRUE),
  grad.gender.data.3, all=TRUE),
  grad.gender.data.4, all=TRUE),
  grad.gender.data.5, all=TRUE),
  grad.gender.data.6, all=TRUE),
  grad.gender.data.7, all=TRUE)

#using the grouped dataset to create the merged run of all runs
grad.gender.data.merged = get.graduate.gender.data.merged(grad.gender.data.allruns)



#############################################################################################################################################
###################################  enrollment and graduation data based on learners detected country  #####################################
#############################################################################################################################################


get.graduate.country.data = function(enrolments.dataset, myrun){
  
  #extracting the numbers of enrollments and graduates for each country
  countries.enrolled = as.data.frame(table(enrolments.dataset$detected_country))
  countries.graduates = as.data.frame(table(enrolments.dataset$detected_country[enrolments.dataset$fully_participated_at!=""]))
  
  #merging the two above created data frames into one
  both.data.frames = merge(countries.enrolled, countries.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  #removing the rows where after the merge, the resulting frequency was 0
  both.data.frames[is.na(both.data.frames)] = 0
  
  #returning my newly created data with enrollments, graduations, run and graduation percentage for each country
  return(
    data.frame(
      run = rep(myrun, length(countries.enrolled$Var1)),
      country = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}

#calling the function from above to create 7 new sets for the selected/calculated data
grad.country.data.1 = get.graduate.country.data(cyber.security.1_enrolments,1)
grad.country.data.2 = get.graduate.country.data(cyber.security.2_enrolments,2)
grad.country.data.3 = get.graduate.country.data(cyber.security.3_enrolments,3)
grad.country.data.4 = get.graduate.country.data(cyber.security.4_enrolments,4)
grad.country.data.5 = get.graduate.country.data(cyber.security.5_enrolments,5)
grad.country.data.6 = get.graduate.country.data(cyber.security.6_enrolments,6)
grad.country.data.7 = get.graduate.country.data(cyber.security.7_enrolments,7)


############################  merging all country data sets into one complete set #####################


get.graduate.country.data.merged = function(allcountries){
  
  #getting the unique names of countries from all runs
  country.names = unique(allcountries$country)
  
  #initializing vectors that will hold the numbers of enrollments and graduations of each country
  all.country.enrollments = 1:length(country.names)
  all.country.graduations = 1:length(country.names)
  
  #extracting the enrollments and graduations of each country from all runs
  for (i in 1:length(country.names)) {
    all.country.enrollments[i] = sum(allcountries$enrollements[allcountries$country == country.names[i]])
    all.country.graduations[i] = sum(allcountries$graduates[allcountries$country == country.names[i]])
  }
  
  #recalculating the the graduate percentage of each country by merging the findings of all runs
  return(
    data.frame(
      country = unique(allcountries$country),
      grad.percent = all.country.graduations/all.country.enrollments*100
    )
  )
}

#grouping all the country graduate data sets that I have created for all runs
grad.country.data.allruns = merge(merge(merge(merge(merge(merge(
  grad.country.data.1,
  grad.country.data.2, all=TRUE),
  grad.country.data.3, all=TRUE),
  grad.country.data.4, all=TRUE),
  grad.country.data.5, all=TRUE),
  grad.country.data.6, all=TRUE),
  grad.country.data.7, all=TRUE)

#using the grouped dataset to create the merged run of all runs
grad.country.data.merged = get.graduate.country.data.merged(grad.country.data.allruns)



#############################################################################################################################################
#######################################  enrollment and graduation data based on learners age group  ########################################
#############################################################################################################################################


get.graduate.age.data = function(enrolments.dataset, myrun){
  
  #extracting the numbers of enrollments and graduates for each age group
  ages.enrolled = as.data.frame(table(enrolments.dataset$age_range))
  ages.graduates = as.data.frame(table(enrolments.dataset$age_range[enrolments.dataset$fully_participated_at!=""]))
  
  #merging the two above created data frames into one
  both.data.frames = merge(ages.enrolled, ages.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  #removing the rows where after the merge, the resulting frequency was 0
  both.data.frames[is.na(both.data.frames)] = 0
  #changing the levels of the factor variable for the age groups and giving it the levels of all age groups
  both.data.frames$Var1 = both.data.frames$Var1 <- factor(both.data.frames$Var1, levels = c("<18", "18-25", "26-35", "36-45", "46-55", "56-65", ">65", "Unknown"), ordered = TRUE)
  
  #creating a data frame with all of the newly created information
  result = data.frame(
    run = rep(myrun, length(ages.enrolled$Var1)),
    age.range = both.data.frames$Var1,
    enrollements = both.data.frames$Freq.x,
    graduates = both.data.frames$Freq.y,
    graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  #returning the newly created data frame ordered by the age.range
  return(result[order(result$age.range),])
}

#calling the function from above to create 7 new sets for the selected/calculated data
grad.age.data.1 = get.graduate.age.data(cyber.security.1_enrolments,1)
grad.age.data.2 = get.graduate.age.data(cyber.security.2_enrolments,2)
grad.age.data.3 = get.graduate.age.data(cyber.security.3_enrolments,3)
grad.age.data.4 = get.graduate.age.data(cyber.security.4_enrolments,4)
grad.age.data.5 = get.graduate.age.data(cyber.security.5_enrolments,5)
grad.age.data.6 = get.graduate.age.data(cyber.security.6_enrolments,6)
grad.age.data.7 = get.graduate.age.data(cyber.security.7_enrolments,7)


############################  merging all age group data sets into one complete set #####################


get.graduate.age.data.merged = function(allages) {
  
  #getting the unique names of the age ranges from my new set
  age.names = unique(allages$age.range)
  
  #initializing vectors that will hold the numbers of enrollments and graduations of each age group
  all.age.enrollments = 1:length(age.names)
  all.age.graduations = 1:length(age.names)
  
  #extracting the enrollments and graduations of each age group from all runs
  for (i in 1:length(age.names)) {
    all.age.enrollments[i] = sum(allages$enrollements[allages$age.range == age.names[i]])
    all.age.graduations[i] = sum(allages$graduates[allages$age.range == age.names[i]])
  }
  
  #recalculating the the graduate percentage of each age group by merging the findings of all runs
   return(
     data.frame(
       age = unique(allages$age.range),
       grad.percent = all.age.graduations/all.age.enrollments*100
     )
   )
}

#grouping all the age data sets that I have created for all runs
grad.age.data.allruns = merge(merge(merge(merge(merge(merge(
  grad.age.data.1,
  grad.age.data.2, all=TRUE),
  grad.age.data.3, all=TRUE),
  grad.age.data.4, all=TRUE),
  grad.age.data.5, all=TRUE),
  grad.age.data.6, all=TRUE),
  grad.age.data.7, all=TRUE)

#producing a merged age group data set out of the sets for all the runs
grad.age.data.merged = get.graduate.age.data.merged(grad.age.data.allruns)



#############################################################################################################################################
#######################################  enrollment and graduation data based on learners employment  #######################################
#############################################################################################################################################


get.graduate.employment.data = function(enrolments.dataset, myrun){
  
  #extracting the numbers of enrollments and graduates for each employment type
  employment.enrolled = as.data.frame(table(enrolments.dataset$employment_status))
  employment.graduates = as.data.frame(table(enrolments.dataset$employment_status[enrolments.dataset$fully_participated_at!=""]))
  
  #merging the two above created data frames into one
  both.data.frames = merge(employment.enrolled, employment.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  #removing the rows where after the merge, the resulting frequency was 0
  both.data.frames[is.na(both.data.frames)] = 0
  #creating and returning a data frame with all of the newly created information
  return(
    data.frame(
      run = rep(myrun, length(employment.enrolled$Var1)),
      employment.status = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}

#calling the function from above to create 7 new sets for the selected/calculated data
grad.employment.data.1 = get.graduate.employment.data(cyber.security.1_enrolments,1)
grad.employment.data.2 = get.graduate.employment.data(cyber.security.2_enrolments,2)
grad.employment.data.3 = get.graduate.employment.data(cyber.security.3_enrolments,3)
grad.employment.data.4 = get.graduate.employment.data(cyber.security.4_enrolments,4)
grad.employment.data.5 = get.graduate.employment.data(cyber.security.5_enrolments,5)
grad.employment.data.6 = get.graduate.employment.data(cyber.security.6_enrolments,6)
grad.employment.data.7 = get.graduate.employment.data(cyber.security.7_enrolments,7)


############################  merging all employment data sets into one complete set #####################


get.graduate.employment.data.merged = function(allemployments){
  
  #getting all the unique employment names
  employment.names = unique(allemployments$employment.status)
  
  #initializing vectors that will hold the numbers of enrollments and graduations of each employment
  all.employment.enrollments = 1:length(employment.names)
  all.employment.graduations = 1:length(employment.names)
  
  #extracting the enrollments and graduations of each employment from all runs
  for (i in 1:length(employment.names)) {
    all.employment.enrollments[i] = sum(allemployments$enrollements[allemployments$employment.status == employment.names[i]])
    all.employment.graduations[i] = sum(allemployments$graduates[allemployments$employment.status == employment.names[i]])
  }
  
  #recalculating the the graduate percentage of each employment by merging the findings of all runs
  return(
    data.frame(
      employment.status = unique(allemployments$employment.status),
      grad.percent = all.employment.graduations/all.employment.enrollments*100
    )
  )
}
#grouping all the employment data sets that I have created for all runs
grad.employment.data.allruns = merge(merge(merge(merge(merge(merge(
  grad.employment.data.1,
  grad.employment.data.2, all=TRUE),
  grad.employment.data.3, all=TRUE),
  grad.employment.data.4, all=TRUE),
  grad.employment.data.5, all=TRUE),
  grad.employment.data.6, all=TRUE),
  grad.employment.data.7, all=TRUE)

#producing a merged employment group data set out of the sets for all the runs
grad.employment.data.merged = get.graduate.employment.data.merged(grad.employment.data.allruns)
  


#############################################################################################################################################
#######################################  enrollment and graduation data based on learners education  ########################################
#############################################################################################################################################


get.graduate.education.data = function(enrolments.dataset, myrun){
  
  #extracting the numbers of enrollments and graduates for each background education
  education.enrolled = as.data.frame(table(enrolments.dataset$highest_education_level))
  education.graduates = as.data.frame(table(enrolments.dataset$highest_education_level[enrolments.dataset$fully_participated_at!=""]))
  
  #merging the two above created data frames into one
  both.data.frames = merge(education.enrolled, education.graduates, by.x = "Var1", by.y = "Var1", all.x = TRUE)
  #removing the rows where after the merge, the resulting frequency was 0
  both.data.frames[is.na(both.data.frames)] = 0
  #creating and returning a data frame with all of the newly created information
  return(
    data.frame(
      run = rep(myrun, length(education.enrolled$Var1)),
      education.status = both.data.frames$Var1,
      enrollements = both.data.frames$Freq.x,
      graduates = both.data.frames$Freq.y,
      graduate.percentage = both.data.frames$Freq.y/both.data.frames$Freq.x*100
    )
  )
}

#calling the function from above to create 7 new sets for the selected/calculated data
grad.education.data.1 = get.graduate.education.data(cyber.security.1_enrolments,1)
grad.education.data.2 = get.graduate.education.data(cyber.security.2_enrolments,2)
grad.education.data.3 = get.graduate.education.data(cyber.security.3_enrolments,3)
grad.education.data.4 = get.graduate.education.data(cyber.security.4_enrolments,4)
grad.education.data.5 = get.graduate.education.data(cyber.security.5_enrolments,5)
grad.education.data.6 = get.graduate.education.data(cyber.security.6_enrolments,6)
grad.education.data.7 = get.graduate.education.data(cyber.security.7_enrolments,7)


############################  merging all background education data sets into one complete set #####################


get.graduate.education.data.merged = function(alleducations){
  
  #getting all the unique names for educational backgrounds
  education.names = unique(alleducations$education.status)
  
  #initializing vectors that will hold the numbers of enrollments and graduations of each educational background
  all.education.enrollments = 1:length(education.names)
  all.education.graduations = 1:length(education.names)
  
  #extracting the enrollments and graduations of each educational background from all runs
  for (i in 1:length(education.names)) {
    all.education.enrollments[i] = sum(alleducations$enrollements[alleducations$education.status == education.names[i]])
    all.education.graduations[i] = sum(alleducations$graduates[alleducations$education.status == education.names[i]])
  }
  
  #recalculating the the graduate percentage of each educational background by merging the findings of all runs
  return(
    data.frame(
      education.status = unique(alleducations$education.status),
      grad.percent = all.education.graduations/all.education.enrollments*100
    )
  )
}

#grouping all the education data sets that I have created for all runs
grad.education.data.allruns = merge(merge(merge(merge(merge(merge(
  grad.education.data.1,
  grad.education.data.2, all=TRUE),
  grad.education.data.3, all=TRUE),
  grad.education.data.4, all=TRUE),
  grad.education.data.5, all=TRUE),
  grad.education.data.6, all=TRUE),
  grad.education.data.7, all=TRUE)

#producing a merged education group data set out of the sets for all the runs
grad.education.data.merged = get.graduate.education.data.merged(grad.education.data.allruns)
