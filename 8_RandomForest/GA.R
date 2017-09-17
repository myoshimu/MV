library(randomForest)
dat <- read.csv("GA.csv")
dat <- dat[,-1];dat <- dat[,-1];dat <- dat[,-9]
dat

head(dat)
names(dat) <- c( paste0("V", 1:19),"class")
dat <- transform(dat, class = as.factor(class))
str(dat)


#V1) COUNT
#V2) SUM_totals_pageviews
#V3) SUM_totals_hits
#V4 SUM_totals_timeOnSite
#V5 SUM_hits_hitNumber
#V6 diffdays_latest
#V7 diffdays_oldest
#V8 desktop_flag
#V9 mobile_flag
#V10 OS_Windows_flag
#V11 OS_Macintosh_flag
#V12 SUM_morning_visit
#V13 SUM_daytime_visit
#V14 SUM_evening_visit
#V15 SUM_midnight_visit
#V16 page201404
#V17 page201405
#V18 page201406
#V19 b_hits_customDimensions_value

index <- sample(1:1295, 800)
train <- dat[index,]
test <- dat[-index,]


ga.rf <- randomForest(class ~ ., data=train, importance=T)
ga.rf
ga.rf.pre <- predict(ga.rf, test)
ga.rf.t <- table(ga.rf.pre, test$class)
ga.rf.t
ga.rf.cr <- sum(diag(ga.rf.t))/sum(ga.rf.t)
ga.rf.cr
importance(ga.rf)
