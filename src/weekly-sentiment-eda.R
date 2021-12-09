library(ProjectTemplate)
load.project()

#####################################################  sentiment for run 6  ############################################################

#wordcloud for week 1
wordcloud.run6.week1 = get.weekly.sentiment.plot(cyber.security.6_weekly.sentiment.survey.responses$reason
                                                 [cyber.security.6_weekly.sentiment.survey.responses$week_number == 1])

#wordcloud for week 2
wordcloud.run6.week2 = get.weekly.sentiment.plot(cyber.security.6_weekly.sentiment.survey.responses$reason
                                                 [cyber.security.6_weekly.sentiment.survey.responses$week_number == 2])

#wordcloud for week 3
wordcloud.run6.week3 = get.weekly.sentiment.plot(cyber.security.6_weekly.sentiment.survey.responses$reason
                                                 [cyber.security.6_weekly.sentiment.survey.responses$week_number == 3])


#####################################################  sentiment for run 7  ############################################################

#wordcloud for week 1
wordcloud.run7.week1 = get.weekly.sentiment.plot(cyber.security.7_weekly.sentiment.survey.responses$reason
                                                 [cyber.security.7_weekly.sentiment.survey.responses$week_number == 1])

#wordcloud for week 2
wordcloud.run7.week2 = get.weekly.sentiment.plot(cyber.security.7_weekly.sentiment.survey.responses$reason
                                                 [cyber.security.7_weekly.sentiment.survey.responses$week_number == 2])

#not enough entries for week 3