###################################################################################################################################################
#########################################################      Data Construction      #############################################################
###################################################################################################################################################


################################################################# Quiz Data #######################################################################

#creating a function that extracts information about the performance of the quizzes for a given run 
get.quiz.data = function(question.response.dataset) {
  
  #creating a data frame for the number of questions answered in each quiz (also recording the week number for later use)
  quiz.answers.given = as.data.frame(table(question.response.dataset$week_number,
                                            question.response.dataset$step_number))
  #omitting the rows for the weeks that do not correspond to any of the existing quizzes (each quiz exists only in one week)
  quiz.answers.given = quiz.answers.given[quiz.answers.given$Freq != 0,]
  
  #creating a data frame for the number of questions answered correctly in each quiz (also recording the week number for later use)
  quiz.correct.answers = as.data.frame(table(question.response.dataset$week_number[question.response.dataset$correct=="true"],
                                              question.response.dataset$step_number[question.response.dataset$correct=="true"]))
  #omitting the rows for the weeks that do not correspond to any of the existing quizzes (each quiz exists only in one week)
  quiz.correct.answers = quiz.correct.answers[quiz.correct.answers$Freq != 0,]
  
  #calculating the percentage of correct answers given for each quiz
  quiz.success.percentage = quiz.correct.answers$Freq/quiz.answers.given$Freq*100
  
  #initializing a vector that will hold the estimated number of questions in each quiz
  num.of.questions = 1:length(quiz.answers.given$Var2)
  #iterating from 1 to the number of quizzes that have been found
  for (i in 1:length(quiz.answers.given$Var2)) {
    #inserting the number of questions of each quiz in the corresponding index of my newly created vector
    num.of.questions[i] = 
      #Length is used to count the amount of distinct questions found in each quiz
      length(
        #Function unique is called to narrow down the returned data to only the single instances of each question for each quiz
        unique(
          #finding all instances of question numbers, for when the step number (the quiz) is equal to each of 
          #the discovered quizzes that have been found in my set
          question.response.dataset$question_number[question.response.dataset$step_number==quiz.answers.given$Var2[i]]))
  }
  
  #combining the the week number and the step number to create a vector that represents the step as a whole.
  #The hundreds represent the week number (*100) and the rest of the number represents the step number
  quiz.step = as.numeric(as.character(quiz.answers.given$Var1))*100+as.numeric(as.character(quiz.answers.given$Var2))
  
  #returning a newly created data frame of all the extracted values that will help me address my business goals
  finalset = data.frame(
    quiz=quiz.step,
    answers_given = quiz.answers.given$Freq,
    answers_correct = quiz.correct.answers$Freq,
    success_percentage = quiz.success.percentage,
    number_of_questions = num.of.questions
    )
  
  return(finalset[order(finalset$quiz),])
}

#calling "get.quiz.data" function for run 1
quiz.data.1 = get.quiz.data(cyber.security.1_question.response)
#calling "get.quiz.data" function for run 2
quiz.data.2 = get.quiz.data(cyber.security.2_question.response)
#calling "get.quiz.data" function for run 3
quiz.data.3 = get.quiz.data(cyber.security.3_question.response)
#calling "get.quiz.data" function for run 4
quiz.data.4 = get.quiz.data(cyber.security.4_question.response)
#calling "get.quiz.data" function for run 5
quiz.data.5 = get.quiz.data(cyber.security.5_question.response)
#calling "get.quiz.data" function for run 6
quiz.data.6 = get.quiz.data(cyber.security.6_question.response)
#calling "get.quiz.data" function for run 7
quiz.data.7 = get.quiz.data(cyber.security.7_question.response)

