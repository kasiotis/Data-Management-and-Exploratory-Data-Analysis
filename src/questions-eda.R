#load projTemp library
library('ProjectTemplate')
load.project()


#plotting the quizzes against their respective graduate percentage (runs 2-7)
quiz.crossrun.plot = function(){
  
  plot(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==2]),
       quiz.data.allruns$success_percentage[quiz.data.allruns$run==2], xaxt="n", type = "l", col=2,
       ylim = c(0,100), xlab = "quizzes", ylab = "Percentage of Graduates", main = "Quizzes success percentage across runs")
  lines(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==3]),
        quiz.data.allruns$success_percentage[quiz.data.allruns$run==3], type = "l", col=3)
  lines(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==4]),
        quiz.data.allruns$success_percentage[quiz.data.allruns$run==4], type = "l", col=4)
  lines(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==5]),
        quiz.data.allruns$success_percentage[quiz.data.allruns$run==5], type = "l", col=5)
  lines(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==6]),
        quiz.data.allruns$success_percentage[quiz.data.allruns$run==6], type = "l", col=6)
  lines(1:length(quiz.data.allruns$quiz[quiz.data.allruns$run==7]),
        quiz.data.allruns$success_percentage[quiz.data.allruns$run==7], type = "l", col=7)
  axis(1, at=1:5, labels = quiz.data.merged$quiz, cex.axis=0.7)
  legend("topleft", legend = c("run2","run3","run4","run5","run6","run7"), cex = 0.50, fill=2:7, text.font = 4)
}

quiz.crossrun.plot()


#checking the success percentage of each quiz for all runs
quiz.merged.plot = barplot(quiz.data.merged$success_percentage, axisnames = TRUE, names.arg = quiz.data.merged$quiz, col = rainbow(5), 
                           ylim = c(0,100), xlab = "Steps", ylab = "Percentage of correct answers given", main = "Quiz/Test success percentage")


#checking the correlation of the number of questions and the percentage of success
quiz.question.num.plot = plot(quiz.data.merged$number_of_questions, quiz.data.merged$success_percentage, xlab = "Number of Questions",
                              ylab = "Succes Percentage", main = "The effect of number of questions on the Success Percentage")
