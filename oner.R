setwd("G:/vimal/data science/JHU/RepResearch/RepData_PeerAssessment1/")
activity <- read.csv("activity.csv")
head(activity)
summary(activity)

library("dplyr")
library("sqldf")
library("ggplot2")

act_dt <- group_by(activity, date)
sum_act <- summarise(act_dt, tot = sum(steps))
sum_act$date <- as.Date(sum_act$date)
hist(sum_act$tot, breaks = 10, 
     main = 'Histogram of No. of steps taken per day',
     xlab = 'No. of Steps per Day')
act_mean <- as.integer(mean(sum_act$tot, na.rm = T))
act_median<- median(sum_act$tot, na.rm = T)
??as.int
?round
?plot
rm(act_dt)

rm(avt_act)
act_dt <- group_by(activity, interval)
avt_act <- summarise(act_dt, mn = mean(steps, na.rm = T))
max_steps <- avt_act[which.max(avt_act$mn),]
plot(avt_act, type = 'l', 
     main = 'Average Daily Activity Pattern', 
     xlab = 'Interval in 5 Mins',
     ylab = 'Mean')

cnt <- table(is.na(activity$steps))
as.integer(cnt['TRUE'])


activity_new <- activity
for(i in 1:nrow(activity_new)){
     if(is.na(activity_new$steps[i])){
          #replace all NA's with the mean of the same interval
          activity_new$steps[i] <- as.integer(round(avt_act$mn[avt_act$interval == activity_new$interval[i]]))
     }
}

act_dt <- group_by(activity_new, date)
sum_act <- summarise(act_dt, tot = sum(steps))
sum_act$date <- as.Date(sum_act$date)
hist(sum_act$tot, breaks = 10, 
     main = 'Histogram of No. of steps taken per day(w/o NA)',
     xlab = 'No. of Steps per Day')
act_mean <- as.integer(mean(sum_act$tot))
act_median<- median(sum_act$tot)
activity_new$date <- as.Date(activity_new$date)

activity_new$weekday <- weekdays(activity_new$date)
View(activity_new)
actn <- activity_new
sqldf()
query <- "select a.*, (CASE a.weekday WHEN 'Sunday' THEN 'weekend' 
                                      WHEN 'Saturday' THEN 'weekend' 
                                      ELSE 'weekday' END) as iswd 
                    from actn as a";
activity_new <- sqldf(query)
sqldf()

activity_new$iswd <- as.factor(activity_new$iswd)
act_dt <- group_by(activity_new, iswd, interval)
avt_act <- summarise(act_dt, mn = mean(steps, na.rm = T))
max_steps <- avt_act[which.max(avt_act$mn),]
qplot(x = interval, y = mn, data = avt_act,
      geom = c("line"), main = '' ,ylab = 'Number of steps')+
      facet_wrap ( ~ iswd, ncol = 1)

??knit2html()
library(knitr)
?knit2html
knit2html("PA1_template.Rmd");
