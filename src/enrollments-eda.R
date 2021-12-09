#load projTemp library
library(ProjectTemplate)
library(ggplot2)

load.project()


#############################################################################################################
##################################  Analysis on overall graduates  ##########################################
#############################################################################################################


#percentage of students who finished the course in each run
general.grad.merged.plot = barplot(grad.general.data$graduation.percent, col="red", main ="Percentage of graduates in each run",
                                   names =c("Run 1","Run 2","Run 3","Run 4","Run 5","Run 6","Run 7"))


#############################################################################################################
##################################  Analysis on Age range graduates  ########################################
#############################################################################################################


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
  #changing the axis of the plot to represent the actual values of the age groups
  axis(1, at=1:length(grad.age.data.allruns$age.range[grad.age.data.allruns$run==1]),
       labels = grad.age.data.allruns$age.range[grad.age.data.allruns$run==1], cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

age.grad.crossrun.plot()

#producing a plot for the total number of enrollments categorized by age group
age.enrolled.merged.plot = function(){
  barplot(grad.age.data.merged$enrollments, names=grad.age.data.merged$age, xlab = "Age group", ylab = "Number of Enrollments",
          main = "Total number of enrollments categorized by age group")
}
age.enrolled.merged.plot()

#plotting all the age ranges of the learners against their respective graduate percentage (merged version of the 7 runs)
age.grad.merged.plot = function(){
  barplot(grad.age.data.merged$grad.percent, names=grad.age.data.merged$age, xlab = "Age group", ylab = "Percentage of Graduates",
          main = "Percentage of Graduates categorized by age group")
}
age.grad.merged.plot()  



#############################################################################################################
###############################  Analysis on the country of learners  #######################################
#############################################################################################################


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
  #changing the axis of the plot to represent the actual values of the countries
  axis(1, at=1:length(grad.country.data.merged$country), labels = grad.country.data.merged$country, cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

country.grad.crossrun.plot() ### this plot ended up being unfruitful

#producing a plot for the total number of enrollments categorized by country
country.enrolled.merged.plot = function(){
  barplot(grad.country.data.merged$enrollments, names=grad.country.data.merged$country, xlab = "Countries", ylab = "Number of Enrollments",
          main = "Total number of enrollments categorized by country", axis.lty = 1, cex.names = 0.45)
}
country.enrolled.merged.plot()

#plotting all the countries of origin of the learners against their respective graduate percentage.
#doing this for a merged version of all 7 runs to make it easier to plot, but also avoid bias towards
#countries that might have a smaller number of enrollments in a single run
country.grad.merged.plot = function(){
  barplot(grad.country.data.merged$grad.percent, names=grad.country.data.merged$country, xlab = "Country", ylab = "Percentage of Graduates",
          main = "Percentage of Graduates categorized by Country", axis.lty = 1, cex.names = 0.45)
} ## this plot is not very clear to see what is going on
country.grad.merged.plot()

#finding the countries with the most enrollments
best.countries.enrollment = grad.country.data.merged[grad.country.data.merged$enrollments>600,1:2]
#ordering those countries by the number of their enrollments
(best.countries.enrollment = best.countries.enrollment[order(best.countries.enrollment$enrollments, decreasing = TRUE),])


#plotting all the countries of origin of the learners against their respective graduate percentage, for a merged version of all 7 runs
#but only displaying the countries where the graduation percentage was greater than or equal to 15%
(country.grad.merged.plot.best = 
    ggplot(grad.country.data.merged[grad.country.data.merged$grad.percent>=15,], aes(x = country, y = grad.percent))+ geom_line(aes(group = 1), color=7)) +
  labs(y= "Country", x = "Graduate Percentage", title = "Graduate Percentage of countries with more that 15%")  ## this plot is much clearer and understandable

#finding the best graduate percentage (more that 20%) throughout all runs according to the country of origin of the learners
best.countries.crossrun = grad.country.data.merged[(grad.country.data.merged$grad.percent>10 & grad.country.data.merged$enrollments>15),]

#reordering the countries according to their graduation percentage (from high to low) and showing only the top 4
(best.countries.crossrun = head(best.countries.crossrun[order(best.countries.crossrun$grad.percent, decreasing = TRUE),],5))



#############################################################################################################
###########################  Analysis on the previous education of learners  ################################
#############################################################################################################


#plotting the background education of the learners against their respective graduate percentage (all runs)
#each line represents a single run.
education.grad.crossrun.plot = function(){
  
  plot(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==1]),
       grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==1], xaxt="n", type = "l", col=1,
       ylim = c(0,100), xlab = "Education Status", ylab = "Percentage of Graduates",
       main = "Percentage of Graduates for each Education Status")
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==2]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==2], type = "l", col=2)
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==3]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==3], type = "l", col=3)
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==4]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==4], type = "l", col=4)
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==5]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==5], type = "l", col=5)
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==6]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==6], type = "l", col=6)
  lines(as.numeric(grad.education.data.allruns$education.status[grad.education.data.allruns$run==7]),
        grad.education.data.allruns$graduate.percentage[grad.education.data.allruns$run==7], type = "l", col=7)
  #changing the axis of the plot to represent the actual values of the education status
  axis(1, at=1:length(grad.education.data.merged$education.status), labels = grad.education.data.merged$education.status, 
       cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

education.grad.crossrun.plot()

#producing a plot for the total number of enrollments categorized by education
education.enrolled.merged.plot = function(){
  barplot(grad.education.data.merged$enrollments, names=grad.education.data.merged$education.status, xlab = "education", ylab = "Number of Enrollments",
          main = "Total number of enrollments categorized by education", axis.lty = 1, cex.names = 0.65)
}
education.enrolled.merged.plot()

#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
education.grad.merged.plot = function(){
  barplot(grad.education.data.merged$grad.percent, names=grad.education.data.merged$education.status, xlab = "Education", ylab = "Percentage of Graduates",
          main = "Percentage of Graduates categorized by Education", axis.lty = 0.45)
}
education.grad.merged.plot()

#finding the best graduate percentage (more that 20%) throughout all runs according to the background education of the learners
best.education.crossrun = grad.education.data.allruns[(grad.education.data.allruns$graduate.percentage>20 &
                                                       grad.education.data.allruns$enrollements >10),]

#reordering the education according to their graduation percentage (from high to low) and showing only the top 4
(best.education.crossrun = head(best.education.crossrun[order(best.education.crossrun$graduate.percentage, decreasing = TRUE),],4))


#############################################################################################################
###########################  Analysis on the employment of learners  #######################################
#############################################################################################################


#plotting the employment of the learners against their respective graduate percentage (all runs)
employment.grad.crossrun.plot = function(){
  
  plot(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==1]),
       grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==1], xaxt="n", type = "l", col=1,
       ylim = c(0,100), xlab = "Employment Status", ylab = "Percentage of Graduates",
       main = "Percentage of Graduates for each Employment Status")
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==2]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==2], type = "l", col=2)
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==3]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==3], type = "l", col=3)
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==4]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==4], type = "l", col=4)
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==5]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==5], type = "l", col=5)
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==6]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==6], type = "l", col=6)
  lines(as.numeric(grad.employment.data.allruns$employment.status[grad.employment.data.allruns$run==7]),
        grad.employment.data.allruns$graduate.percentage[grad.employment.data.allruns$run==7], type = "l", col=7)
  #changing the axis of the plot to represent the actual values of the employment status
  axis(1, at=1:length(grad.employment.data.merged$employment.status), labels = grad.employment.data.merged$employment.status, 
       cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

employment.grad.crossrun.plot()

#producing a plot for the total number of enrollments categorized by employment
employment.enrolled.merged.plot = function(){
  barplot(grad.employment.data.merged$enrollments, names=grad.employment.data.merged$employment.status, xlab = "employment", ylab = "Number of Enrollments",
          main = "Total number of enrollments categorized by employment", axis.lty = 1, cex.names = 0.65)
}
employment.enrolled.merged.plot()

#plotting all the employments of the learners against their respective graduate percentage average of all runs
employment.grad.merged.plot = function(){
  barplot(grad.employment.data.merged$grad.percent, names=grad.employment.data.merged$employment.status, xlab = "Employment", ylab = "Percentage of Graduates",
          main = "Percentage of Graduates categorized by Employment", axis.lty = 0.45)
}
employment.grad.merged.plot()

#finding the best graduate percentage (more that 20%) throughout all runs according to the employment of the learners
best.employment.crossrun = grad.employment.data.allruns[(grad.employment.data.allruns$graduate.percentage>20 &
                                                       grad.employment.data.allruns$enrollements >10),]

#reordering the employments according to their graduation percentage (from high to low) and showing only the top 4
(best.employment.crossrun = head(best.employment.crossrun[order(best.employment.crossrun$graduate.percentage, decreasing = TRUE),],4))


#############################################################################################################
###########################  Analysis on the gender of learners  ############################################
#############################################################################################################


#plotting the gender of the learners against their respective graduate percentage (all runs)
gender.grad.crossrun.plot = function(){
  
  plot(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==1]),
       grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==1], xaxt="n", type = "l", col=1,
       ylim = c(0,100), xlab = "Gender", ylab = "Percentage of Graduates",
       main = "Percentage of Graduates for each Gender")
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==2]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==2], type = "l", col=2)
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==3]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==3], type = "l", col=3)
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==4]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==4], type = "l", col=4)
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==5]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==5], type = "l", col=5)
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==6]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==6], type = "l", col=6)
  lines(as.numeric(grad.gender.data.allruns$gender[grad.gender.data.allruns$run==7]),
        grad.gender.data.allruns$graduate.percentage[grad.gender.data.allruns$run==7], type = "l", col=7)
  #changing the axis of the plot to represent the actual values of the gender
  axis(1, at=1:length(grad.gender.data.merged$gender), labels = grad.gender.data.merged$gender, 
       cex.axis=0.7, cex.names=0.7)
  legend("topleft", legend = c("run1","run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=1:7, text.font = 4)
}

gender.grad.crossrun.plot()

#producing a plot for the total number of enrollments categorized by gender
gender.enrolled.merged.plot = function(){
  barplot(grad.gender.data.merged$enrollments, names=grad.gender.data.merged$gender, xlab = "gender", ylab = "Number of Enrollments",
          main = "Total number of enrollments categorized by gender", axis.lty = 1, cex.names = 0.65)
}
gender.enrolled.merged.plot()

#plotting all the educational backgrounds of the learners against their respective graduate percentage average of all runs
gender.grad.merged.plot = function(){
  barplot(grad.gender.data.merged$grad.percent, names=grad.gender.data.merged$gender, xlab = "Gender", ylab = "Percentage of Graduates",
          main = "Percentage of Graduates categorized by Gender", axis.lty = 0.45)
}
gender.grad.merged.plot()

#finding the best graduate percentage (more that 20%) throughout all runs according to the gender of the learners
best.gender.crossrun = grad.gender.data.allruns[(grad.gender.data.allruns$graduate.percentage>20 &
                                                       grad.gender.data.allruns$enrollements >10),]

#reordering the gender according to their graduation percentage (from high to low) and showing only the top 4
(best.gender.crossrun = head(best.gender.crossrun[order(best.gender.crossrun$graduate.percentage, decreasing = TRUE),],4))
