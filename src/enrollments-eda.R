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


#plotting all the countries of origin of the learners against their respective graduate percentage
(country.grad.average.plot = ggplot(grad.country.data.merged, aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))

#plotting all the countries of origin of the learners against their respective graduate percentage
#but only displaying the countries where the graduation percentage was greater than or equal to 20%
(country.grad.average.plot.best = ggplot(grad.country.data.merged[grad.country.data.merged$grad.percent>=15,], aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))



###########################  Analysis on the previous education of learners  ################################


#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(education.grad.average.plot = ggplot(grad.education.data.merged, aes(x = education.status, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###########################  Analysis on the employment of learners  #######################################


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
