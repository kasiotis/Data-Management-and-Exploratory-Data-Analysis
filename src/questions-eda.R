#load projTemp library
library('ProjectTemplate')
load.project()

#merging all the leaving reason data sets that I have created for all runs
allquizzes = merge(merge(merge(merge(merge(
  quiz.data.2,
  quiz.data.3, all=TRUE),
  quiz.data.4, all=TRUE),
  quiz.data.5, all=TRUE),
  quiz.data.6, all=TRUE),
  quiz.data.7, all=TRUE)


#getting the unique names of the age ranges from my new set
question.names = unique(allquizzes$quiz)

#initializing vectors that will hold the numbers of enrollments and graduations of each country
all.answer.frequencies = 1:length(question.names)
all.correct.frequencies = 1:length(question.names)

#extracting the enrollments and graduations of each country from all runs
for (i in 1:length(question.names)) {
  all.answer.frequencies[i] = mean(allquizzes$answers_given[allquizzes$quiz == question.names[i]])
  all.correct.frequencies[i] = mean(allquizzes$answers_correct[allquizzes$quiz == question.names[i]])
}

#recalculating the the graduate percentage of each country by merging the findings of all runs
quiz.data.merged = data.frame(
  quiz = question.names,
  answers_given = all.answer.frequencies,
  answers_correct = all.correct.frequencies,
  success_percentage = all.correct.frequencies/all.answer.frequencies*100,
  number_of_questions = quiz.data.1$number_of_questions
)

#checking the success percentage of each quiz for all runs
barplot(quiz.data.merged$success_percentage, axisnames = TRUE, names.arg = quiz.data.merged$quiz, col = rainbow(7), ylim = c(0,100),
        xlab = "Steps", ylab = "Percentage of correct answers given", main = "Quiz/Test success percentage")

#checking the correlation of the number of questions and the percentage of success
plot(quiz.data.1$number_of_questions, quiz.data.1$success_percentage)

