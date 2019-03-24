# playing around in R part 1
# Date: 08/26/18

#SQL Query used: SELECT * FROM [glmsidekick:HRM.Heartrate]
df <- firsttry
heartrate1 <-df$value

# Plot HR over time (testing)
plot(heartrate1)

#SQL Query used: SELECT * FROM [glmsidekick:HRM.Score]
#user score history
user1 <- X1219430030_Score_History_
user2 <- X6755366276694604_Score_History
user3 <- X6755366276694825_score_history
user4 <- X6755366276696842_score_history
user5 <- X1240470048_score_history
user6 <- X6755366276696794_score_history
user7 <- X6755366276696807
user8 <- X1242540528
user9 <- X6755366276696785_score
user10 <- X6755366276696779_scores
user11 <- X6755366276696782
user12 <- X6755366276696842_scorez
user13 <- X1243060177_scoree
user14 <- X6755366276694613_scoreeee
user15 <- X1239500066_results_score
user16 <- X1260190924_score_

#average score/kicks of each user
avg1 <- mean(user1$kicks)
avg2 <- mean(user2$kicks)
avg3 <- mean(user3$kicks)
avg4 <- mean(user4$kicks)
avg5 <- mean(user5$kicks)
avg6 <- mean(user6$kicks)
avg7 <- mean(user7$kicks)
avg8 <- mean(user8$kicks)
avg9 <- mean(user9$kicks)
avg10 <- mean(user10$kicks)
avg11 <- mean(user11$kicks)
avg12 <- mean(user12$kicks)
avg13 <- mean(user13$kicks)
avg14 <- mean(user14$kicks)
avg15 <- mean(user15$kicks)
avg16 <- mean(user16$kicks)

#Let's see which users have most kicks
array1 <- c(avg1, avg2, avg3, avg4, avg5, 
                avg6, avg7, avg8, avg9, avg10, avg11, 
                avg12, avg13, avg14, avg15, avg16)
column.names <- c("User 1", "User 2", "User 3", "User 4", 
                  "User 5", "User 6", "User 7", "User 8",
                  "User 9", "User 10", "User 11", "User 12",
                  "User 13", "User 14", "User 15", "User 16")
barplot(
  names = column.names,
  height = array1,
  main = "Average kicks per user",
  xlab = "User number",
  ylab = "Kicks")
#user5 has most kicks at 30.2 per day, user9 has least at 5.9

#average kicks of whole pilor cohort
wholeavg <- mean(avg1, avg2, avg3, avg4, avg5, avg6, avg7, 
                avg8, avg9,avg10, avg11, avg12, avg13, 
                avg14, avg15, avg16)