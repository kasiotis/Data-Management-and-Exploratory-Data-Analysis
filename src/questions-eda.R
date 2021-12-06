#load projTemp library
library('ProjectTemplate')
#set my working dir to my projTemp folder
load.project()

barplot(quiz.data.1$success_percentage, axisnames = TRUE, names.arg = quiz.data.1$quiz) ######### use this later in your analysis
