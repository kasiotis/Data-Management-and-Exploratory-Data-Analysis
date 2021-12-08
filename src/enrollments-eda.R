#load projTemp library
library(ProjectTemplate)
library(ggplot2)

load.project()

##################################  Analysis on overall graduates  ##########################################


#percentage of students who finished the course in each run
barplot(grad.general.data$graduation.percent, col="red", main ="Percentage of graduates in each run",
        names =c("Run 1","Run 2","Run 3","Run 4","Run 5","Run 6","Run 7"))



##################################  Analysis on Age range graduates  ########################################


#plotting all the countries of origin of the learners against their respective graduate percentage
(age.grad.average.plot = ggplot(grad.age.data.merged, aes(x = age, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###############################  Analysis on the country of learners  #######################################


allcountries = merge(merge(merge(merge(merge(merge(
      grad.country.data.1,
      grad.country.data.2, all=TRUE),
      grad.country.data.3, all=TRUE),
      grad.country.data.4, all=TRUE),
      grad.country.data.5, all=TRUE),
      grad.country.data.6, all=TRUE),
      grad.country.data.7, all=TRUE)

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
grad.country.data.merged = data.frame(
  country = unique(allcountries$country),
  grad.percent = all.country.graduations/all.country.enrollments*100
)

#plotting all the countries of origin of the learners against their respective graduate percentage
(country.grad.average.plot = ggplot(grad.country.data.merged, aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))

#plotting all the countries of origin of the learners against their respective graduate percentage
#but only displaying the countries where the graduation percentage was greater than or equal to 20%
(country.grad.average.plot.best = ggplot(grad.country.data.merged[grad.country.data.merged$grad.percent>=15,], aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))



###########################  Analysis on the previous education of learners  #######################################

alleducations = merge(merge(merge(merge(merge(merge(
  grad.education.data.1,
  grad.education.data.2, all=TRUE),
  grad.education.data.3, all=TRUE),
  grad.education.data.4, all=TRUE),
  grad.education.data.5, all=TRUE),
  grad.education.data.6, all=TRUE),
  grad.education.data.7, all=TRUE)

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
grad.education.data.merged = data.frame(
  education.status = unique(alleducations$education.status),
  grad.percent = all.education.graduations/all.education.enrollments*100
)

#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(education.grad.average.plot = ggplot(grad.education.data.merged, aes(x = education.status, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###########################  Analysis on the employment of learners  #######################################


allemployments = merge(merge(merge(merge(merge(merge(
  grad.employment.data.1,
  grad.employment.data.2, all=TRUE),
  grad.employment.data.3, all=TRUE),
  grad.employment.data.4, all=TRUE),
  grad.employment.data.5, all=TRUE),
  grad.employment.data.6, all=TRUE),
  grad.employment.data.7, all=TRUE)

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
grad.employment.data.merged = data.frame(
  employment.status = unique(allemployments$employment.status),
  grad.percent = all.employment.graduations/all.employment.enrollments*100
)

#plotting all the employments of the learners against their respective graduate percentage average of all runs
(employment.grad.average.plot = ggplot(grad.employment.data.merged, aes(x = employment.status, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###########################  Analysis on the gender of learners  #######################################


allgenders = merge(merge(merge(merge(merge(merge(
  grad.gender.data.1,
  grad.gender.data.2, all=TRUE),
  grad.gender.data.3, all=TRUE),
  grad.gender.data.4, all=TRUE),
  grad.gender.data.5, all=TRUE),
  grad.gender.data.6, all=TRUE),
  grad.gender.data.7, all=TRUE)

gender.names = unique(allgenders$gender)

#initializing vectors that will hold the numbers of enrollments and graduations of each educational background
all.gender.enrollments = 1:length(gender.names)
all.gender.graduations = 1:length(gender.names)

#extracting the enrollments and graduations of each educational background from all runs
for (i in 1:length(gender.names)) {
  all.gender.enrollments[i] = sum(allgenders$enrollements[allgenders$gender == gender.names[i]])
  all.gender.graduations[i] = sum(allgenders$graduates[allgenders$gender == gender.names[i]])
}

#recalculating the the graduate percentage of each educational background by merging the findings of all runs
grad.gender.data.merged = data.frame(
  gender = unique(allgenders$gender),
  grad.percent = all.gender.graduations/all.gender.enrollments*100
)

#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(gender.grad.average.plot = ggplot(grad.gender.data.merged, aes(x = gender, y = grad.percent))+
    geom_line(aes(group = 1), color=7))
