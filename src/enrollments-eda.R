#load projTemp library
library(ProjectTemplate)
library(ggplot2)

load.project()

##################################  Analysis on overall graduates  ##########################################


#percentage of students who finished the course in each run
general.grad.merged.plot = barplot(grad.general.data$graduation.percent, col="red", main ="Percentage of graduates in each run",
                                   names =c("Run 1","Run 2","Run 3","Run 4","Run 5","Run 6","Run 7"))



##################################  Analysis on Age range graduates  ########################################


#plotting all the age ranges of the learners against their respective graduate percentage (merged runs)
(age.grad.merged.plot = ggplot(grad.age.data.merged, aes(x = age, y = grad.percent))+
    geom_line(aes(group = 1), color=7))

#plotting all the age ranges of the learners against their respective graduate percentage (all runs)
age.grad.crossrun.plot = function(){
  plot(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==1]),
       grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==1], xaxt="n", type = "l", col=1,
       ylim = c(0,max(grad.gender.data.allruns$graduate.percentage)*2), xlab = "Age-Range", ylab = "Percentage of Graduates",
       main = "Percentage of Graduates for each age-range")
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==2]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==2], type = "l", col=2)
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==3]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==3], type = "l", col=3)
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==4]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==4], type = "l", col=4)
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==5]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==5], type = "l", col=5)
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==6]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==6], type = "l", col=6)
  lines(as.numeric(grad.age.data.allruns$age.range[grad.age.data.allruns$run==7]),
        grad.age.data.allruns$graduate.percentage[grad.age.data.allruns$run==7], type = "l", col=7)
  axis(1, at=1:length(grad.age.data.allruns$age.range[grad.age.data.allruns$run==1]),
       labels = grad.age.data.allruns$age.range[grad.age.data.allruns$run==1], cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run1","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

age.grad.crossrun.plot()




###############################  Analysis on the country of learners  #######################################


#plotting all the countries of origin of the learners against their respective graduate percentage
(country.grad.merged.plot = ggplot(grad.country.data.merged, aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))

#plotting all the countries of origin of the learners against their respective graduate percentage
#but only displaying the countries where the graduation percentage was greater than or equal to 20%
(country.grad.merged.plot.best = ggplot(grad.country.data.merged[grad.country.data.merged$grad.percent>=15,], aes(x = country, y = grad.percent))+
  geom_line(aes(group = 1), color=7))

#plotting all the countries of origin of the learners against their respective graduate percentage (all runs)
country.grad.crossrun.plot = function(){

  plot(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==1]),
       grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==1], xaxt="n", type = "l", col=1,
       ylim = c(0,100), xlab = "Age-Range", ylab = "Percentage of Graduates",
       main = "Percentage of Graduates for each age-range")
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==2]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==2], type = "l", col=2)
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==3]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==3], type = "l", col=3)
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==4]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==4], type = "l", col=4)
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==5]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==5], type = "l", col=5)
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==6]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==6], type = "l", col=6)
  lines(as.numeric(grad.country.data.allruns$country[grad.country.data.allruns$run==7]),
        grad.country.data.allruns$graduate.percentage[grad.country.data.allruns$run==7], type = "l", col=7)
  axis(1, at=1:length(grad.country.data.merged$country), labels = grad.country.data.merged$country, cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run1","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

country.grad.crossrun.plot()

#finding the best graduate percentage throughout all runs according to the country of origin of the learners
best.countries.crossruns = grad.country.data.allruns[(grad.country.data.allruns$graduate.percentage>20 &
                                                        grad.country.data.allruns$enrollements >10),]
#reordering the countries according to their graduation percentage (from high to low) and showing only the top 4
(best.countries.crossruns = head(best.countries.crossruns[order(best.countries.crossruns$graduate.percentage, decreasing = TRUE),],4))



###########################  Analysis on the previous education of learners  ################################


#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(education.grad.merged.plot = ggplot(grad.education.data.merged, aes(x = education.status, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###########################  Analysis on the employment of learners  #######################################


#plotting all the employments of the learners against their respective graduate percentage average of all runs
(employment.grad.merged.plot = ggplot(grad.employment.data.merged, aes(x = employment.status, y = grad.percent))+
    geom_line(aes(group = 1), color=7))



###########################  Analysis on the gender of learners  ###########################################


#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
(gender.grad.merged.plot = ggplot(grad.gender.data.merged, aes(x = gender, y = grad.percent))+
    geom_line(aes(group = 1), color=7))
