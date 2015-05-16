
setwd("C:/PMW/MyWorking.pmw/Coursera/RepData1/RepData_PeerAssessment1")

options(scipen = 1, digits = 0)

df <- read.csv("activity.csv")

daily <- aggregate(df["steps"], by=df[c("date")], FUN=sum);

hist(daily$steps)

avgdaily <- mean(daily$steps, na.rm = TRUE)

print(avgdaily)

mediandaily <- median(daily$steps, na.rm = TRUE)

print(mediandaily)

avginterval <- aggregate(df["steps"], by=df[c("interval")], FUN=mean, na.rm=TRUE)

plot(avginterval$steps ~ avginterval$interval, type = "l")

maxinterval <- avginterval$interval[which.max(avginterval$steps)]

print(maxinterval)

missing <- is.na(df$steps)
dfmiss <- cbind(df, missing)
nas <- subset(dfmiss, dfmiss$miss == TRUE)

nascount <- nrow(nas)


num <- subset(dfmiss[,c(3,2,1)], dfmiss$miss == FALSE)
impval <- merge(nas[,c(2,3)], avginterval , by = "interval")
dfimp <- rbind(impval, num)

dailyimp <- aggregate(dfimp["steps"], by=dfimp[c("date")], FUN=sum);

hist(dailyimp$steps)

avgdailyimp <- mean(dailyimp$steps, na.rm = TRUE)

print(avgdailyimp)

mediandailyimp <- median(dailyimp$steps, na.rm = TRUE)

print(mediandailyimp)

dfimp$date <- as.POSIXct(as.character(dfimp$date),tz="", "%Y-%m-%d")
dfimp$day <- weekdays(dfimp$date)
dfimp[which(dfimp$day == "Sunday"), "day"] <- "weekend"
dfimp[which(dfimp$day == "Saturday"), "day"] <- "weekend"
dfimp[which(dfimp$day != "weekend"), "day"] <- "weekday"

dfweekend <- dfimp[which(dfimp$day == "weekend"),]
avgintwkend <- aggregate(dfweekend["steps"], by=dfweekend[c("interval")], FUN=mean)

dfweekday <- dfimp[which(dfimp$day == "weekday"),]
avgintwkday <- aggregate(dfweekday["steps"], by=dfweekday[c("interval")], FUN=mean)


par(mfcol = c(2,1))
plot(avgintwkend$steps ~ avgintwkend$interval, type = "l", main = "weekend")
plot(avgintwkday$steps ~ avgintwkday$interval, type = "l", main = "weekday")



