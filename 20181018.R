#10/18/18
library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)
library(knitr)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(rmarkdown)
library(tidyr)
library(stringr)
library(xts)
library(zoo)

#schema list
result

#turning off scientific notation
options(scipen=999)

all_HR <- read.table(file="/Users/emiliagrzesiak/Downloads/20180913%2F20180913",
                     header = TRUE, sep = ",")

all_Score <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011",
                        header = TRUE, sep= ",")

all_MissionDetail <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011b",
                        header = TRUE, sep= ",")

all_ScoreDetail <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011c",
                                header = TRUE, sep= ",")
all_Mission <- read.table(file="/Users/emiliagrzesiak/Downloads/results-20181018-230557.csv",
                          header=TRUE, sep=",")

#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Heartrate]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Score]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.ScoreMission]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.MissionDetails]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Mission]

all_HR$userId <- as.character(all_HR$userId)
# grouping HR with userID, then looking at average HR per day for each user
HRvsUser_summary <- group_by(all_HR, userId) %>% 
  summarize(average_HR = mean(value), stan_dev=sd(value))
summary(HRvsUser_summary)

plotAvgHR <- ggplot(
  data= HRvsUser_summary, 
  aes(x=userId, y=average_HR)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_bw() +
  ggtitle("Average HR per User (Pilot)") +
  xlab("User ID") +
  ylab("Average HR") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) +
  geom_errorbar(aes(ymin=average_HR-stan_dev, ymax=average_HR+stan_dev),
                width=.2,position=position_dodge(.9))
print(plotAvgHR)

date_sep <- separate(all_HR, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)

HRvsUser <- date_sep %>% 
  group_by(userId,date) %>%
  summarize(average_HR=mean(value), Standard_Dev=sd(value))

HRvsUser$newdate <- as.Date(HRvsUser$date, '%Y-%m-%d')

plotHR <- ggplot(HRvsUser, aes(x=newdate, y=average_HR, colour=userId, group = userId)) +
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Average HR per day per user") +
  xlab("Date") +
  ylab("Heart rate (beats/min)") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
  # geom_errorbar(aes(ymin=average_HR-Standard_Dev, ymax=average_HR+Standard_Dev),
                # width=.2,position=position_dodge(.9))
print(plotHR)

all.scores <- read.csv(file=
"/Users/emiliagrzesiak/Desktop/Independent\ Study\ 1/User\ Data\ PILOT/Score\ Data/All\ Score\ Data1.csv",
 header = FALSE, sep = ",")
names(all.scores) <- as.matrix(all.scores[2, ])
all.scores <- all.scores[-1:-2, ]

#Editing dates column to separate dates & times
all.scores <- separate(all.scores, col = date, c("date", "time"), 
                       sep = "T", remove = TRUE)

#looking at average score of each user
all.scores$kicks <- as.numeric(as.character(all.scores$kicks))
score_table <- group_by(all.scores, userId) %>% 
  summarize(average_kick = mean(kicks), stan_dev=sd(kicks), 
            points_collected=length(kicks))
avg1 <- score_table$average_kick
sd1 <- score_table$stan_dev
points <- score_table$points_collected
                              
#Summary of score of users
summary(score_table)

#plot of scores vs users
plot2 <- ggplot(
  data = score_table,
  aes(x = userId, y = average_kick)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_classic()+ 
  ggtitle("Average Score per User (Pilot)")+
  xlab("User ID")+
  ylab("Average Score Collected") +
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(plot2)

#calculating and plotting total duration on program(app 1st, then watch)

UservsTime_app <- all.scores %>% 
  group_by(userId) %>%
  summarize(duration=length(unique(date)))
  
plot_duration_app <- ggplot(
  data = UservsTime_app,
  aes(x=userId, y=duration)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue") +
  theme_bw() +
  ggtitle("Duration of Time User Using App") +
  xlab("User")+
  ylab("Days")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(plot_duration_app)

all_HR_date_edit <- separate(all_HR, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)

UservsTime_watch <- all_HR_date_edit %>% 
  group_by(userId) %>%
  summarize(duration=length(unique(date)))

plot_duration_watch <- ggplot(
  data = UservsTime_watch,
  aes(x=userId, y=duration)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue") +
  theme_bw() +
  ggtitle("Duration of Time User Using Watch") +
  xlab("User")+
  ylab("Days")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(plot_duration_watch)

#Type of Exercise Analysis
