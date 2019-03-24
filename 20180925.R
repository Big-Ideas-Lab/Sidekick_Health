#09/25/18

#calling SQL to R pipeline
source("SQL_R_pipeline.R")

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

query_results$userId[is.na(query_results$userId)]<- "Unknown"
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.Heartrate]
# grouping HR with userID, then looking at average HR per day for each user
HRvsUser_summary <- group_by(query_results, userId) %>% 
  summarize(average_HR = mean(value), stan_dev=sd(value))
summary(HRvsUser_summary)


plotAvgHR <- ggplot(
  data= HRvsUser_summary, 
  aes(x=userId, y=average_HR)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_classic() +
  ggtitle("Average HR per User (Pilot)") +
  xlab("User ID") +
  ylab("Average HR")
print(plotAvgHR)

date_sep <- separate(query_results, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)

HRvsUser <- date_sep %>% 
  group_by(userId,date) %>%
  summarize(Average_HR=mean(value), Standard_Dev=sd(value))

plotHR <- ggplot(HRvsUser, aes(x=date, y=Average_HR, colour=userId)) +
  geom_point() + geom_line() +
  ggtitle("Average HR per day per user") +
  xlab("Date") +
  ylab("Heart rate (beats/min") +
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(HRvsUser)

# HRdays <- xts(HRvsUser_table$value, 
#               as.Date(HRvsUser_table$date))
# apply.daily(HRdays, mean)

# df <- firsttry
# heartrate1 <- df$value
# daytime <- as_datetime(df$date)
# df <- df %>% mutate(Point_num = row_number())
# df2 <- data.frame(x = daytime, y = heartrate1)
# 
# # Plot HR over time (will modify to include all time points, these are just 100
# #arbitrary points I chose to work with just to test things out in the beginning)
# plot1 <- ggplot(df2, aes(x = daytime, y = heartrate1))
# plot1 + geom_point() +
#   ggtitle("100 Chronological Measurements of Heartrate for User 3")+
#   xlab("Date + time recorded")+
#   ylab("Heart rate (beats/min)")+
#   theme(axis.text.x = element_text(angle=-90, hjust=1))

all.scores <- read.csv(file=
"/Users/emiliagrzesiak/Desktop/Independent\ Study\ 1/User\ Data\ PILOT/Score\ Data/All\ Score\ Data1.csv",
 header = FALSE, sep = ",")
names(all.scores) <- as.matrix(all.scores[2, ])
all.scores <- all.scores[-1:-2, ]

#Editing dates column to separate dates & times
all.scores <- separate(all.scores, col = date, c("date(Y/M/D)", "time"), 
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
  aes(x = userID, y = average_kick)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_classic()+ 
  ggtitle("Average Score per User (Pilot)")+
  xlab("User ID")+
  ylab("Average Score Collected") +
  geom_errorbar(aes(ymin=avg1-se1, ymax=avg1+se1), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle=-90, hjust=1))