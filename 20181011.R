#10/11/18

#calling SQL to R pipeline
#source("Other_pipeline.R")
#pipeline LEAKED so it is out of order!

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

#manually importing CSV files
all_HR <- read.table(file="/Users/emiliagrzesiak/Downloads/20180913%2F20180913",
                     header = TRUE, sep = ",")
all_HR$userId <- format(all_HR$userId, scientific = FALSE)
all_Score <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011",
                        header = TRUE, sep= ",")
all_Score$userId <- format(all_Score$userId, scientific = FALSE)
all_Score$id <- format(all_Score$id, scientific = FALSE)
all_MissionDetail <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011b",
                        header = TRUE, sep= ",")
all_MissionDetail$missionId <- format(all_MissionDetail$missionId, scientific = FALSE)
all_MissionDetail$id <- format(all_MissionDetail$id, scientific = FALSE)
all_ScoreDetail <- read.table(file="/Users/emiliagrzesiak/Downloads/20181011c",
                                header = TRUE, sep= ",")
all_ScoreDetail$missionId <- format(all_ScoreDetail$missionId, scientific = FALSE)
all_ScoreDetail$id <- format(all_ScoreDetail$id, scientific = FALSE)
all_ScoreDetail$scoreId <- format(all_ScoreDetail$scoreId, scientific = FALSE)

#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Heartrate]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Score]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.ScoreMission]
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.MissionDetails]

# grouping HR with userID, then looking at average HR per day for each user
HRvsUser_summary <- group_by(all_HR, userId) %>% 
  summarize(average_HR = mean(value), stan_dev=sd(value))
format(HRvsUser_summary$userId, scientific = FALSE)
summary(HRvsUser_summary)

plotAvgHR <- ggplot(
  data= HRvsUser_summary, 
  aes(x=userId, y=average_HR)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_classic() +
  ggtitle("Average HR per User (Pilot)") +
  xlab("User ID") +
  ylab("Average HR") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
print(plotAvgHR)

date_sep <- separate(all_HR, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)

HRvsUser <- date_sep %>% 
  group_by(userId,date) %>%
  summarize(average_HR=mean(value), Standard_Dev=sd(value))

HRvsUser$newdate <- as.Date(HRvsUser$date, '%Y-%m-%d')

plotHR <- ggplot(HRvsUser, aes(x=newdate, y=average_HR, colour=userId, group = userId)) +
  geom_point() + geom_line() +
  theme_classic() +
  ggtitle("Average HR per day per user") +
  xlab("Date") +
  ylab("Heart rate (beats/min)") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
  # geom_errorbar(aes(ymin=average_HR-Standard_Dev, ymax=average_HR+Standard_Dev),
                # width=.2,position=position_dodge(.9))
print(plotHR)

# HRdays <- xts(HRvsUser_table$value, 
#               as.Date(HRvsUser_table$date))
# apply.daily(HRdays, mean)

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
#id <- score_table$simpler_id
points <- score_table$points_collected
se1 <- sd1/sqrt(points)

#rename User Id to less crazy numbers
#score_table <- score_table %>% mutate(simpler_id = row_number())
                              
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

#calculating and plotting total duration on program

UservsTime <- all.scores %>% 
  group_by(userId) %>%
  summarize(duration=(max(as.Date(date)-min(as.Date(date)))))
  
plot_duration <- ggplot(
  data = UservsTime,
  aes(x=userId, y=duration)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue") +
  theme_classic() +
  ggtitle("Duration of Time User in Study") +
  xlab("User")+
  ylab("Days")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(plot_duration)

#merging data sets to know when an exercise was performed by a user
colnames(all_Score)[1] <- 'scoreId'
ultra_sheet1 <- merge(all_Score, all_ScoreDetail, 
                     by=c("scoreId"))
ULTRA_sheet <- merge(ultra_sheet1, all_MissionDetail, 
                     by= c("missionId"))
ULTRA_sheet$bonusKicks <- NULL
ULTRA_sheet$gpxBlobKey <- NULL
ULTRA_sheet$hasAttachment <- NULL
ULTRA_sheet$transactionId <- NULL
ULTRA_sheet$happyHourMissionId <- NULL

#will work on rest of this this later^^