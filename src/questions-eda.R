#load projTemp library
library('ProjectTemplate')
load.project()

par(mfrow=c(1,7))
#checking the success percentage of each quiz for all runs
barplot(quiz.data.1$success_percentage, axisnames = TRUE, names.arg = quiz.data.1$quiz, col = rainbow(7))
barplot(quiz.data.2$success_percentage, axisnames = TRUE, names.arg = quiz.data.2$quiz, col = rainbow(7))
barplot(quiz.data.3$success_percentage, axisnames = TRUE, names.arg = quiz.data.3$quiz, col = rainbow(7))
barplot(quiz.data.4$success_percentage, axisnames = TRUE, names.arg = quiz.data.4$quiz, col = rainbow(7))
barplot(quiz.data.5$success_percentage, axisnames = TRUE, names.arg = quiz.data.5$quiz, col = rainbow(7))
barplot(quiz.data.6$success_percentage, axisnames = TRUE, names.arg = quiz.data.6$quiz, col = rainbow(7))
barplot(quiz.data.7$success_percentage, axisnames = TRUE, names.arg = quiz.data.7$quiz, col = rainbow(7))
#the success percentage seems to be approximately the same between the quizzes in all runs 

#checking the correlation of the number of questions and the percentage of success
plot(quiz.data.1$number_of_questions, quiz.data.1$success_percentage)

