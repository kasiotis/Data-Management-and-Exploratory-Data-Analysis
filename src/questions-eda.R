#load projTemp library
library('ProjectTemplate')
load.project()


#checking the success percentage of each quiz for all runs
barplot(quiz.data.merged$success_percentage, axisnames = TRUE, names.arg = quiz.data.merged$quiz, col = rainbow(7), ylim = c(0,100),
        xlab = "Steps", ylab = "Percentage of correct answers given", main = "Quiz/Test success percentage")

#checking the correlation of the number of questions and the percentage of success
plot(quiz.data.1$number_of_questions, quiz.data.1$success_percentage)

