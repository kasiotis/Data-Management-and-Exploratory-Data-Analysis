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


#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(gender.grad.average.plot = ggplot(grad.gender.data.merged, aes(x = gender, y = grad.percent))+
    geom_line(aes(group = 1), color=7))
