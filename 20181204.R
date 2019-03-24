#12/4/18
library(cowplot)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(vcdExtra)
library(gridExtra)
library(nlme)
library(Hmisc)
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
library(reshape2)
library(scales)
library(rprojroot)
library(outliers)

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
#SQL Query used in pipeline: SELECT * FROM [glmsidekick:HRM.User]

all_HR$userId <- as.character(all_HR$userId)
# grouping HR with userID, then looking at average HR per day for each user
HRvsUser_summary <- group_by(all_HR, userId) %>% 
  dplyr::summarize(average_HR = mean(value), 
            lower_quartile_HR= quantile(value, 0.25), stan_dev=sd(value))
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
  dplyr::summarize(average_HR=mean(value),
            lower_quartile_HR= quantile(value, 0.25),
            Standard_Dev=sd(value))
HRvsUser$newdate <- as.Date(HRvsUser$date, '%Y-%m-%d')
HRvsUser<-data.table(HRvsUser)
HRvsUser[,index:= order(newdate), by="userId"]

plotHR <- ggplot(HRvsUser, aes(x=index, y=average_HR, colour=userId, group = userId)) +
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Average HR per day per user") +
  xlab("Number of days") +
  ylab("Heart rate (beats/min)")
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
  dplyr::summarize(average_kick = mean(kicks), stan_dev=sd(kicks), 
            points_collected=length(kicks))
avg1 <- score_table$average_kick
sd1 <- score_table$stan_dev
points <- score_table$points_collected
score_table <- score_table[score_table$userId %in% all_HR$userId,]
                              
#Summary of score of users
summary(score_table)

#plot of scores vs users
score_plot <- ggplot(
  data = score_table,
  aes(x = userId, y = average_kick)) +
  geom_bar(stat = "identity", width=0.8, fill="steelblue")+
  theme_classic()+ 
  ggtitle("Average Score per User (Pilot)")+
  xlab("User ID")+
  ylab("Average Score Collected") +
  theme(axis.text.x = element_text(angle=-90, hjust=1))
print(score_plot)

#calculating and plotting total duration & frequency on program (app 1st, then watch)
  ## app duration
UservsTime_app <- all.scores %>% 
  group_by(userId) %>%
  dplyr::summarize(duration_on_app=length(unique(date)))

all_HR_date_edit <- separate(all_HR, col = date, c("date", "time"), 
                       sep = " ", remove = TRUE)
  ## watch duration
UservsTime_watch <- all_HR_date_edit %>% 
  group_by(userId) %>%
  dplyr::summarize(duration_on_watch=length(unique(date)))

UservsTime_total <- merge(UservsTime_app, UservsTime_watch, 
                          by="userId", all=TRUE)
UservsTime_total[is.na(UservsTime_total)] <- 0
UservsTime <- melt(UservsTime_total, id.vars = 'userId')
UservsTime <- UservsTime[!(UservsTime$value == 0),]
UservsTime <- UservsTime[UservsTime$userId %in% all_HR$userId,]

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
  dplyr::summarize(entries_on_day=n())
freq_app <- data.table(freq_app)
freq_app[,index:= order(date), by="userId"]
freq_app <- freq_app[freq_app$userId %in% all_HR$userId]

freq_app_plot<- ggplot(freq_app,
                       aes(x=index, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of app entires per day per user") +
  xlab("Days since start day") +
  ylab("Number of entries")
print(freq_app_plot)

##watch usage frequency
all_HR_date_edit$date <- as.Date(all_HR_date_edit$date, format="%Y-%m-%d")
all_HR_date_edit$userId <- as.character(all_HR_date_edit$userId)
freq_watch <- all_HR_date_edit %>% 
  group_by(userId, date) %>%
  dplyr::summarize(entries_on_day=n())
freq_watch <- data.table(freq_watch)
freq_watch[,index:= order(date), by="userId"]

freq_watch_plot<- ggplot(freq_watch,
                       aes(x=index, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of watch entires per day per user") +
  xlab("Days since start day") +
  ylab("Number of entries")
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
exercise_time$day <- as.Date(exercise_time$date, format="%Y-%m-%d")
freq_exercise <- exercise_time %>% 
  group_by(userId, day) %>%
  dplyr::summarize(entries_on_day=n())
freq_exercise <- data.table(freq_exercise)
freq_exercise[,index:= order(day), by="userId"]
freq_exercise$userId <- as.character(freq_exercise$userId)
freq_exercise <- freq_exercise[freq_exercise$userId %in% all_HR$userId]

freq_exercise_plot<- ggplot(freq_exercise,
  aes(x=index, y=entries_on_day, colour=userId, group=userId))+
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Number of exercise entires per day per user") +
  xlab("Days since starting date") +
  ylab("Number of entries") +
  theme(axis.text.x = element_text(angle=-90, hjust=1)) 
print(freq_exercise_plot)

   ##looking at daily app exercise input as a binary value, analysis + plot
freq_exercise$day <- as.Date(freq_exercise$day)
freq_exercise2<- freq_exercise[,c("userId", "day", "entries_on_day")] %>% 
  group_by(userId) %>%
  complete(userId, day = seq.Date(min(day), max(day), by = "day"))
freq_exercise2 <- setDT(freq_exercise2, keep.rownames=TRUE, key=NULL, check.names=FALSE)
freq_exercise2[is.na(freq_exercise2)] <- 0
freq_exercise2$presence_absence <- ifelse(freq_exercise2$entries_on_day!=0, 1, 0)
freq_exercise2[,index:= order(day), by="userId"]
freq_exercise2$userId <- as.character(freq_exercise2$userId)

## unwanted plot
# model_exercise<- ggplot(freq_exercise2, 
#        aes(index, presence_absence, color=userId))+
#   stat_smooth(method="glm", formula=y~x,
#               alpha=0.2, size=2, aes(fill=userId))+
#   geom_point(position=position_jitter(height=.03, width=0))+
#   xlab("Date") + ylab("Exercised") +
#   ggtitle("Exercise app input frequency per day over time")
# print(model_exercise)

    ##looking at daily usage of watch as binary value, analysis + plot
freq_watch2<- freq_watch[,c("userId", "date", "entries_on_day")] %>% group_by(userId) %>%
  complete(userId, date = seq.Date(min(date), max(date), by = "day"))
freq_watch2 <- setDT(freq_watch2, keep.rownames=TRUE, key=NULL, check.names=FALSE)
freq_watch2[is.na(freq_watch2)] <- 0
freq_watch2$presence_absence <- ifelse(freq_watch2$entries_on_day!=0, 1, 0)
freq_watch2[,index:= order(date), by="userId"]
freq_watch2$userId <- as.character(freq_watch2$userId)

##also unwanted plot
# model_watch_wear<- ggplot(freq_watch2, 
#   aes(index, as.integer(presence_absence), color=userId)) +
#   stat_smooth(method="lm", formula=y~x,
#               alpha=0.2, size=2, aes(fill=userId))+
#   geom_point(position=position_jitter(height=.03, width=0))+
#   xlab("Date") + ylab("Wearing watch") + 
#   ggtitle("Watch wearing frequency per day over time")
# print(model_watch_wear)

   ##looking at hourly usage of watch as binary value, analysis + plot (for later)

# Exercise and Heart rate analysis

all_HR$day <- as.Date(all_HR$date, format="%Y-%m-%d")
# all_HR_exercise <- merge(all_HR, 
#       exercise_time[ , c("day", "date", "userId", "name")], 
#       by = c("day", "userId"), all.x=TRUE)
# all_HR_exercise <- all_HR_exercise[complete.cases(all_HR_exercise), ]

exercise_time$userId <- as.character(exercise_time$userId)

all_HR$date <- as.POSIXct(all_HR$date, format="%Y-%m-%d %H:%M:%S")
exercise_time$date <- as.POSIXct(exercise_time$date, format="%Y-%m-%d %H:%M:%S")
z <- lapply(intersect(all_HR$userId, exercise_time$userId),function(ID) {
  HR <- subset(all_HR[, c("value", "date", "userId")],userId==ID)
  EX <- subset(exercise_time[, c("name", "userId", "date", "scoreId")],userId==ID)
  EX$indices <- sapply(EX$date,function(d) which.min(abs(HR$date - d)))
  HR$indices <- 1:nrow(HR)
  merge(HR,EX,by=c('userId','indices'))
})
all_HR_exercise <- do.call(rbind,z)
all_HR_exercise$indices <- NULL
all_HR_exercise$difference<- difftime(all_HR_exercise$date.x, 
                                      all_HR_exercise$date.y, units=c("days"))
all_HR_exercise <- all_HR_exercise[!(all_HR_exercise$difference > 1 | all_HR_exercise$difference < 0),]

exerciseType_HR <- all_HR_exercise %>% 
  group_by(userId, name) %>%
  dplyr::summarize(average_HRperActivity= mean(value))

exerciseType_freq <- all_HR_exercise %>% count(name, userId)
exerciseType_HR <- merge(exerciseType_HR, exerciseType_freq, by=c("userId", "name"))
colnames(exerciseType_HR)[4]<-"times_performed_byUser"

##ask to include or not, very cluttered
#title: Heartrate after exercise for each user
exerciseType_plot <- ggplot(exerciseType_HR, 
                        aes(x=userId, y=average_HRperActivity, fill=name)) +
  geom_bar(stat='identity', position='dodge')+
  theme_bw() +
  xlab("User")+
  ylab("Heartrate (bpm)")+
  #theme(axis.text.x = element_text(angle=-90, hjust=1))+
  scale_x_discrete(labels=as.factor(index(unique(exerciseType_HR$userId))))
print(exerciseType_plot)

#Title: exercise frequency
exerciseFreq_plot <- ggplot(exerciseType_HR, 
                            aes(x=userId, y=times_performed_byUser, fill=name)) +
  geom_bar(stat='identity', position='dodge')+
  theme_bw() +
  xlab("User")+
  ylab("# of times done")+
  #theme(axis.text.x = element_text(angle=-90, hjust=1))+
  scale_x_discrete(labels=as.factor(index(unique(exerciseType_HR$userId))))
print(exerciseFreq_plot)

#from now on, only looking at exercise data that includes HR information
#further exercise & HR analysis

steps_HR <- all_HR_exercise %>%
  filter(name == "step_count")
steps_value <- all_ScoreDetail %>%
  filter(missionId == 5188910565031936)
HR_stepValues <- merge(steps_HR[,c("userId", "value", "date.y", "scoreId")],
                       steps_value[,c("value1", "scoreId")], by = "scoreId")

# resting HR analysis on 3 individuals for three days
  ##function to give me number of measurements per hour, seen on boxplots below
n_fun <- function(x){
  return(data.frame(y = median(x)*1.02, label = paste0("n=",length(x))))
}
mean.n <- function(x){
  return(c(y = median(x)*0.95, label = round(mean(x),2)))
}
individual_HR1 <- subset(all_HR, userId == 1239500066)
individual_HR1 <- individual_HR1 %>% 
  filter(date < as.POSIXct("2018-02-10 00:00:00") & date > as.POSIXct("2018-02-09 00:00:00"))

plot_HR1 <- ggplot(individual_HR1, aes(x=date, y=value)) +
  geom_point() +
  theme_bw() +
  ggtitle("HR of 1239500066 throughout day") +
  xlab(" ") +
  ylab("Heart rate (beats/min)")

boxplot_HR1 <- ggplot(individual_HR1, 
                      aes(x = format(date,format = "%H"), y = value, group = format(date,format = "%H"))) + 
  geom_boxplot(fill = "grey80", colour = "#3366FF") +
  theme_bw()+
  stat_summary(fun.data = n_fun, geom = "text", 
               fun.y = median, colour = "black", size= 2, fontface="bold")+
  #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
  xlab(" ") +
  ylab ("Heart rate (beats/min)")

individual_HR2 <- subset(all_HR, userId == 6755366276696782)
individual_HR2 <- individual_HR2 %>% 
  filter(date < as.POSIXct("2018-02-10 00:00:00") & date > as.POSIXct("2018-02-09 00:00:00"))

plot_HR2 <- ggplot(individual_HR2, aes(x=date, y=value)) +
  geom_point() +
  theme_bw() +
  xlab(" ") +
  ylab(" ") +
  ggtitle("HR of 6755366276696782 throughout day")

boxplot_HR2 <- ggplot(individual_HR2, 
                      aes(x = format(date,format = "%H"), 
                          y = value, group = format(date,format = "%H"))) + 
  geom_boxplot(fill = "grey80", colour = "#3366FF") +
  stat_summary(fun.data = n_fun, geom = "text", 
               fun.y = median, colour = "black", size= 2, fontface= "bold") +
  theme_bw()+
  #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
  xlab(" ") +
  ylab (" ")

individual_HR3 <- subset(all_HR, userId == 6755366276696785)
individual_HR3 <- individual_HR3 %>% filter(date < as.POSIXct("2018-02-12 00:00:00") & date > as.POSIXct("2018-02-11 00:00:00"))

plot_HR3 <- ggplot(individual_HR3, aes(x=date, y=value)) +
  geom_point() +
  theme_bw() +
  ggtitle("HR of 6755366276696785 throughout the day") +
  xlab(" ") +
  ylab(" ")

boxplot_HR3 <- ggplot(individual_HR3, 
  aes(x = format(date,format = "%H"), 
      y = value, group = format(date,format = "%H"))) + 
  geom_boxplot(fill = "grey80", colour = "#3366FF") +
  stat_summary(fun.data = n_fun, geom = "text", 
               fun.y = median, colour = "black", size= 2, fontface = "bold") +
  theme_bw()+
  #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
  xlab(" ") +
  ylab (" ")

#combine plots together in one figure
print(ggdraw() + draw_plot(plot_HR1, x= 0, y=.5, width = .33, height=.5) +
  draw_plot(plot_HR2, x= .33, y=.5, width = .33, height=.5)+
  draw_plot(plot_HR3, x= .66, y=.5, width = .33, height=.5)+
  draw_plot(boxplot_HR1, x= 0, y=0, width = .33, height=.5)+
  draw_plot(boxplot_HR2, x= .33, y=0, width = .33, height=.5)+
  draw_plot(boxplot_HR3, x= .66, y=0, width = .33, height=.5))

  ##lowest quartile HR analysis before outliers removed
plotHR_resting <- ggplot(HRvsUser, aes(x=index, y=lower_quartile_HR, colour=userId, group = userId)) +
  geom_point() + geom_line() +
  theme_bw() +
  ggtitle("Lower Quartile HR per day per user") +
  xlab("Number of days") +
  ylab("Heart rate (beats/min)")
# geom_errorbar(aes(ymin=average_HR-Standard_Dev, ymax=average_HR+Standard_Dev),
# width=.2,position=position_dodge(.9))
print(plotHR_resting)

#steps analysis
steps_user_raw <- inner_join(steps_value[, c("scoreId", "value1")], 
                    all_Score[, c("scoreId", "userId", "date")], 
      by="scoreId", all=TRUE)
steps_user_raw <- steps_user_raw[steps_user_raw$userId %in% all_HR$userId,]
steps_user_raw$day <- as.Date(steps_user_raw$date, format="%Y-%m-%d")
steps_user_raw <- steps_user_raw[steps_user_raw$day %in% all_HR$day,]
step_perDay <- steps_user_raw %>% group_by(userId, day) %>%
  dplyr::summarize(total_steps=sum(value1))
step_perDay$userId <- as.character(step_perDay$userId)

steps_plot <- ggplot(step_perDay, 
      aes(x=day, y=total_steps, colour=userId, group=userId)) +
  geom_point() + geom_line() +
  xlab("Date") +
  ylab("Steps per day") +
  ggtitle("Steps per user over time (only while wearing watch)")
print(steps_plot)

#Histogram HR analysis
    ##Histogram 1: HR + outlier analysis

##code below used for plotting each histogram on one plot, too messy
# plot_Hist_HR <- ggplot(all_HR, aes(x=value, color=userId, fill=userId)) +
#   geom_histogram(aes(y=..density..), position="dodge", alpha=0.5)+
#   geom_density(alpha=0.6) +
#   theme(legend.position = "right")
# print(plot_Hist_HR)

##code beow used for plotting each histogram separately, too many figures
# for (var in unique(all_HR$userId)) {
#   dev.new()
#   print( ggplot(all_HR[all_HR$userId==var,], 
#   aes(x=value)) +
#   geom_histogram(aes(y=..density..), position="dodge", alpha=.5, bins=10)+
#   geom_density(alpha=0.7)
#                 )
# }

melt_HR <- melt(all_HR[,c("userId","value")])
plot_his_HR <- ggplot(melt_HR, aes(x=value, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlab("Heartrate")+
  ggtitle("Histogram: Raw Heart Rate Data for Each User")+
  facet_grid(userId~.)
print(plot_his_HR)

  ##Histogram #2 & #3: N number of occurances
    ##looking at hourly HR N's
N_all_HR <- all_HR %>% mutate(date = ymd_hms(date), 
                  day = floor_date(date, 'day'), 
                  hour = hour(date)) %>%
  group_by(userId, day, hour) %>%
  dplyr::summarize(avg_HR = mean(value), n=length(value))

#correlation between high sampling frequency and high HR for all
cor.test(N_all_HR$avg_HR, N_all_HR$n)

#correlation between hourly HR and sampling frequency per person
N_corr <- by(N_all_HR, N_all_HR$userId, 
             FUN = function(X) cor(X$n, X$avg_HR))
N_corr<- data.frame(userId = dimnames(N_corr)[[1]], corr = as.vector(N_corr))

corr_plot1 <- ggplot(N_corr, aes(x=userId, y=corr, colour=userId))+
  geom_point()+
  xlab("User ID")+
  ylab("Correlation Coeff")+
  #theme(axis.text.x = element_text(angle=-90, hjust=1))+
  scale_color_hue(labels= index(unique(N_corr$userId)))+
  scale_x_discrete(labels=as.factor(index(unique(N_corr$userId))))
print(corr_plot1)

melt_N <- melt(N_all_HR[,c("userId", "n")])
plot_his_N <- ggplot(melt_N, aes(x= value, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlim(0, 100) +
  xlab("Number of HR samples per hour")+
  ggtitle("Histogram: Sampling Frequency for Each User")+
  facet_grid(userId~.)
print(plot_his_N)

    ##looking at daily HR N's
N2_all_HR <- all_HR %>% mutate(date = ymd_hms(date), 
                               day = floor_date(date, 'day')) %>%
                                 group_by(userId, day) %>%
  dplyr::summarize(avg_HR = mean(value), n=length(value))

cor.test(N2_all_HR$avg_HR, N2_all_HR$n)

#correlation between high HR and daily average sampling per person
N2_corr <- by(N2_all_HR, N2_all_HR$userId, 
             FUN = function(X) cor(X$n, X$avg_HR))
N2_corr<- data.frame(userId = dimnames(N2_corr)[[1]], corr = as.vector(N2_corr))

corr_plot2 <- ggplot(N2_corr, aes(x=userId, y=corr, colour=userId))+
  geom_point()+
  xlab("User ID")+
  ylab("Correlation Coeff")+
  #theme(axis.text.x = element_text(angle=-90, hjust=1))  
  scale_color_hue(labels= index(unique(N2_corr$userId)))+
  scale_x_discrete(labels=as.factor(index(unique(N2_corr$userId))))
print(corr_plot2)

# melt_N2 <- melt(N2_all_HR[,c("userId", "n")])
# plot_his_N2 <- ggplot(melt_N2, aes(x= value, fill=userId)) +
#   geom_histogram(aes(y=..density..), position="dodge", alpha=1, binwidth=1)+
#   geom_density(alpha=1)+
#   xlab("Number of HR samples per day")+
#   ggtitle("Histogram: Sampling Frequency for Each User")+
#   facet_grid(userId~.)
# print(plot_his_N2)

#Data cleaning!
melt_HR$variable <- NULL

ll <- split(all_HR[,c("date", "userId", "value","id")], all_HR$userId);

# Flag entries based on 1.5 IQR
ll <- lapply(ll, function(x) {
  x$outlier <- ifelse(
    x$value < quantile(x$value, 0.25) - 1.5 * IQR(x$value) |
      x$value > quantile(x$value, 0.75) + 1.5 * IQR(x$value),
    TRUE,
    FALSE);
  return(x);
})

ll<- do.call(rbind.data.frame, ll)

all_HR_outliers<- ll[ll$outlier == "TRUE",]
no_HR_outliers<- ll[ll$outlier == "FALSE",]

His_noOUT_HR <- ggplot(no_HR_outliers, aes(x= value, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlab("Heart rate (bpm)")+
  ggtitle("Histogram: Outlier-free Raw Heartrate per User")+
  facet_grid(userId~.)+
  scale_fill_hue(labels= index(unique(no_HR_outliers$userId)))
  #scale_x_discrete(labels=as.factor(index(unique(no_HR_outliers$userId))))
print(His_noOUT_HR)

all_out_His <- ggplot(all_HR_outliers, aes(x= value, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlab("Heart rate (bpm)")+
  ggtitle("Histogram: Outliers Raw Heartrate per User")+
  facet_grid(userId~.)
print(all_out_His)

# val1<- 3
# HR_noOUT = melt_HR %>%
#   group_by(userId) %>%
#   filter(!(abs(value - median(value)) > val1*sd(value)))

# His_HR_noOut <- ggplot(HR_noOUT, aes(x=value, fill=userId)) +
#   geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=5)+
#   geom_density(alpha=.64)+
#   xlab("Heartrate")+
#   ggtitle("Histogram: Heart Rate Data for Each User")+
#   facet_grid(userId~.)
# print(His_HR_noOut)

#outlier analysis of sampling frequency
  ##hourly sampling 1st
nn <- split(N_all_HR, N_all_HR$userId);
nn <- lapply(nn, function(x) {
  x$outlier <- ifelse(
    x$n < quantile(x$n, 0.25) - 1.5 * IQR(x$n) |
      x$n > quantile(x$n, 0.75) + 1.5 * IQR(x$n),
    TRUE,
    FALSE);
  return(x);
})

nn <- do.call(rbind.data.frame, nn)

all_nn_outliers<- nn[nn$outlier == "TRUE",]
no_nn_outliers<- nn[nn$outlier == "FALSE",]

His_noOUT_nn <- ggplot(no_nn_outliers, aes(x= n, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)
  xlab("Samples per Hour")+
  ggtitle("Histogram: Outlier-free Sampling Rate per User")+
  facet_grid(userId~.)
print(His_noOUT_nn)

all_out_nn <- ggplot(all_nn_outliers, aes(x= n, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlab("Samples per Hour")+
  ggtitle("Histogram: Outliers Sampling Rate per User")+
  facet_grid(userId~.)
print(all_out_nn)

  ##daily HR sampling 2nd
nn2 <- split(N2_all_HR, N2_all_HR$userId);
nn2 <- lapply(nn2, function(x) {
  x$outlier <- ifelse(
    x$n < quantile(x$n, 0.25) - 1.5 * IQR(x$n) |
      x$n > quantile(x$n, 0.75) + 1.5 * IQR(x$n),
    TRUE,
    FALSE);
  return(x);
})

nn2 <- do.call(rbind.data.frame, nn2)

all_nn2_outliers<- nn2[nn2$outlier == "TRUE",]
no_nn2_outliers<- nn2[nn2$outlier == "FALSE",]

His_noOUT_nn2 <- ggplot(no_nn2_outliers, aes(x= n, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=1)+
  xlab("Samples per Hour")+
  ggtitle("Histogram: Outlier-free Sampling Rate per User")+
  facet_grid(userId~.)
print(His_noOUT_nn2)

all_out_nn2 <- ggplot(all_nn2_outliers, aes(x= n, fill=userId)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=1)+
  xlab("Samples per Hour")+
  ggtitle("Histogram: Outliers Sampling Rate per User")+
  facet_grid(userId~.)
print(all_out_nn2)

#look at resting HR without outliers
#no_HR_outliers <- no_HR_outliers %>% 
  #mutate(date = ymd_hms(date), day = floor_date(date, 'day')) 

no_HR_outliers <- no_HR_outliers %>% mutate(date = ymd_hms(date), 
                                            day = floor_date(date, 'day')) %>%
  group_by(userId, day) %>%
  mutate(HR_level = .bincode(value, quantile(value, c(0, .25, .75, 1)),
                 #labels=c('Low', 'Medium', 'High'),
                 include.lowest = TRUE))
#levels: 1=low, 2=medium, 3=high
quartile1HR <- no_HR_outliers[no_HR_outliers$HR_level == 1,]

quartile1_summary <- group_by(quartile1HR, userId) %>% 
  dplyr::summarize(lower_quartile_HR = mean(value), stan_dev=sd(value))
summary(quartile1_summary)

noOut_avgHR<- quartile1HR %>%
  group_by(userId, day) %>%
  dplyr::summarize(lower_quartile_HR = mean(value), stan_dev = sd(value))

noOut_avgHR <- data.table(noOut_avgHR)
noOut_avgHR[,index:= order(day), by="userId"]

##title: Resting HR per User
HR_noOut_plot <- ggplot(quartile1_summary, 
                        aes(x=userId, y=lower_quartile_HR))+
  geom_bar(stat = "identity", width=0.8,fill="steelblue")+
  theme_bw()+
  xlab("userId")+
  ylab("Resting HR (bpm)")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))+
  geom_errorbar(aes(ymin=lower_quartile_HR-stan_dev, ymax=lower_quartile_HR+stan_dev),
  width=.2,position=position_dodge(.9))
print(HR_noOut_plot)

##title: resting HR per day per user No Outliers (lol needs work)
plotHR_noOut <- ggplot(noOut_avgHR, 
                       aes(x=index, y=lower_quartile_HR, colour=userId, group = userId)) +
  geom_point() + geom_line() +
  theme_bw() +
  xlab("Number of days") +
  ylab("Heart rate (beats/min)")+
# geom_errorbar(aes(ymin=average_HR-Standard_Dev, ymax=average_HR+Standard_Dev),
# width=.2,position=position_dodge(.9))
scale_color_hue(labels= index(unique(noOut_avgHR$userId)))
print(plotHR_noOut)

# title: Relationship Between SD and Resting HR
fit1 <- lm(stan_dev ~ lower_quartile_HR, data = quartile1_summary)
SDvsRestHR <- ggplot(quartile1_summary,
                        aes(x=stan_dev, y=lower_quartile_HR, colour=userId))+
  geom_point()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  theme_bw()+
  xlab("Standard Deviation of HR")+
  ylab("Resting HR (bpm)")+
  scale_color_hue(labels= index(unique(quartile1_summary$userId)))+
  #scale_x_discrete(labels=as.factor(index(unique(quartile1_summary$userId))))
  labs(title = paste("R^2 = ",signif(summary(fit1)$adj.r.squared, 3), ",",
                     " P =",signif(summary(fit1)$coef[2,4], 4)))
print(SDvsRestHR)

cor.test(quartile1_summary$lower_quartile_HR, quartile1_summary$stan_dev)

# USER INFORMATION (gender: 0 = male, 1 = female)
all_User <- read.table(file="/Users/emiliagrzesiak/Downloads/20181127",
                       header = TRUE, sep= ",")
all_User <- all_User[all_User$id %in% all_HR$userId,]
all_User$transactionId <- NULL
all_User$transactionEndDate <- NULL
all_User$createdDate <- NULL
all_User <- all_User %>% mutate(BMI = 10000*(weight/(height*height)))
all_User$`0`<- NULL
UserInfo_HR <- merge(quartile1_summary, all_User, by.x= "userId", by.y = "id")

#title: Trend between BMI and Average Resting HR
fit2 <- lm(BMI ~ lower_quartile_HR, data = UserInfo_HR)
BMIvsRestHR <- ggplot(UserInfo_HR,
                        aes(x=BMI, y=lower_quartile_HR, colour=userId))+
  geom_point()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  theme_bw()+
  xlab("BMI")+
  ylab("Resting HR (bpm)")+
  scale_color_hue(labels= index(unique(UserInfo_HR$userId)))+
  labs(title = paste("R^2 = ",signif(summary(fit2)$adj.r.squared, 4), ",",
                     " P =",signif(summary(fit2)$coef[2,4], 4)))
print(BMIvsRestHR)

#correlation between resting HR and BMI
cor.test(UserInfo_HR$lower_quartile_HR, UserInfo_HR$BMI)

#steps correlations
stepsvsHR <- merge(step_perDay, noOut_avgHR, by=c("userId", "day"))
#stepsvsHRvsN2 <- merge(stepsvsHR, no_nn2_outliers[,c("userId", "day", "n")], 
                       #by=c("userId", "day"), )

step_HR_summary <- stepsvsHR %>% 
  group_by(userId) %>%
  dplyr::summarize(avg_StepsPerDay = sum(total_steps)/max(index))

BMI_steps_HR <- merge(step_HR_summary, 
                      UserInfo_HR[, c("userId", "lower_quartile_HR","stan_dev", "BMI")])

cor.test(BMI_steps_HR$avg_StepsPerDay, BMI_steps_HR$lower_quartile_HR)
cor.test(BMI_steps_HR$avg_StepsPerDay, BMI_steps_HR$BMI)

#title: Trend between average step count and resting HR
fit3 <- lm(avg_StepsPerDay ~ lower_quartile_HR, data = BMI_steps_HR)
stepsvsRestHR <- ggplot(BMI_steps_HR, 
                        aes(x=avg_StepsPerDay, y=lower_quartile_HR, colour=userId))+
  geom_point()+
  theme_bw()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  xlab("Steps per day")+
  ylab("Resting HR (bpm)")+
  scale_color_hue(labels= index(unique(UserInfo_HR$userId)))+
  labs(title = paste("R^2 = ",signif(summary(fit3)$adj.r.squared, 3), ",",
                     " P =",signif(summary(fit3)$coef[2,4], 4)))
print(stepsvsRestHR)