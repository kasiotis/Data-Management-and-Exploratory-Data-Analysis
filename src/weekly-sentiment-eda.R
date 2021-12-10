library(ProjectTemplate)
#load.project()

#####################################################  sentiment for run 6  ############################################################

#wordcloud for week 1
wordcloud.run6 = function(){
  get.weekly.sentiment.plot(cyber.security.6_weekly.sentiment.survey.responses$reason)
}
wordcloud.run6()


