#10/18/18
library(ggplot2)
library(magrittr)
library(vcdExtra)
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
library(reshape2)
library(tidyr)
library(stringr)
library(xts)
library(zoo)
library(data.table)

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

#calculating and plotting total duration & frequency on program(app 1st, then watch)
  ## app duration
UservsTime_app <- all.scores %>% 
  group_by(userId) %>%
  summarize(duration_on_app=length(unique(date)))

all_HR_date_edit <- separate(all_HR, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)
  ## watch duration
UservsTime_watch <- all_HR_date_edit %>% 
  group_by(userId) %>%
  summarize(duration_on_watch=length(unique(date)))

UservsTime_total <- merge(UservsTime_app, UservsTime_watch, 
                          by="userId", all=TRUE)
UservsTime_total[is.na(UservsTime_total)] <- 0
UservsTime <- melt(UservsTime_total, id.vars = 'userId')

plot_duration <- ggplot(UservsTime, 
                        aes(x=userId, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  theme_bw() +
  ggtitle("Duration of Time User") +
  xlab("User")+
  ylab("Days")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(plot_duration)

  ##app usage frequency
all.scores$date <- as.Date(all.scores$date, format="%Y-%m-%d")
freq_app <- all.scores %>% 
  group_by(userId, date) %>%
  summarize(entries_on_day=n())

freq_app_plot<- ggplot(freq_app,
                       aes(x=date, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of app entires per day per user") +
  xlab("Date") +
  ylab("Number of entries") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
print(freq_app_plot)

##watch usage frequency
all_HR_date_edit$date <- as.Date(all_HR_date_edit$date, format="%Y-%m-%d")
freq_watch <- all_HR_date_edit %>% 
  group_by(userId, date) %>%
  summarize(entries_on_day=n())

freq_watch_plot<- ggplot(freq_watch,
                       aes(x=date, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of watch entires per day per user") +
  xlab("Date") +
  ylab("Number of entries") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
print(freq_watch_plot)

#Type of Exercise Analysis
  ##grouping most important fields together
all_exercise_cat= subset(all_Mission, categoryId==102, 
                         select = c(id,name,typeId))
all_exercise_cat$name=as.character(all_exercise_cat$name)
all_exercise_cat$name[all_exercise_cat$name == "Hreyfing Mjúkur tími"] <- "mission.name.soft_moving"
all_exercise_cat$name[all_exercise_cat$name == "Hreyfing Harður tími"] <- "mission.name.hard_moving"
all_exercise_cat$name=gsub("mission.name.","",all_exercise_cat$name)
colnames(all_exercise_cat)[1] <- "missionId"
colnames(all_Score)[1] <- "scoreId"
exercise_ID <- merge(all_exercise_cat, 
                       all_ScoreDetail[, c("missionId","scoreId")], 
                       by="missionId", all.x = FALSE)
exercise_time <- merge(exercise_ID, all_Score[, c("scoreId", "date", "userId")], 
                       by="scoreId", all.x = FALSE)

  ##Frequency of exercise input in app per user
exercise_time$date <- as.Date(exercise_time$date, format="%Y-%m-%d")
freq_exercise <- exercise_time %>% 
  group_by(userId, date) %>%
  summarize(entries_on_day=n())

freq_exercise$userId <- as.character(freq_exercise$userId)
freq_exercise_plot<- ggplot(freq_exercise,
                       aes(x=date, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of exercise entires per day per user") +
  xlab("Date") +
  ylab("Number of entries") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
print(freq_exercise_plot)

freq_exercise$date <- as.Date(freq_exercise$date)
freq_exercise$yes <- rep(1,nrow(freq_exercise))
freq_exercise<- freq_exercise %>% group_by(userId) %>%
  complete(userId, date = seq.Date(min(date), max(date), by = "day"))
freq_exercise[is.na(freq_exercise)] <- 0


model_exercise<- ggplot(freq_exercise, 
       aes(date, as.integer(yes), color=userId))+
  stat_smooth(method="lm", formula=y~x,
              alpha=0.2, size=2, aes(fill=userId))+
  geom_point(position=position_jitter(height=.03, width=0))+
  xlab("Date") + ylab("Exercised")
print(model_exercise)