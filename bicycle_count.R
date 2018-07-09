rm(list = ls())

# data
data = read.csv('C:/Users/amogh/Downloads/day.csv')

# look at the data
head(data)

tail(data)

library(dplyr)
# rename the columns
data = rename(data,  dateday =dteday ,
       count = cnt,month = mnth,year=yr)

# dimentions of the data
dim(data)

# missing values

sum(is.null(data))

# discriptive analysis

names(data)

continuous = list('temp', 'atemp', 'hum', 'windspeed',
                  'casual', 'registered', 'count')

summary(data)

# box plot

library(ggplot2)

library(Rmisc)

#d <- density(train$account.length) # returns the density data 
p1 <- ggplot(data, aes(x = "",y=data$temp)) +
  geom_boxplot() + labs(title="box plot on temperature") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$hum)) +
  geom_boxplot() + labs(title="box plot on humidity")
p3 <- ggplot(data, aes(x = "",y=data$atemp)) +
  geom_boxplot() + labs(title="box plot on atemp")
p4 <- ggplot(data, aes(x = "",y=data$windspeed)) +
  geom_boxplot() + labs(title="box plot on windspeed")
multiplot(p1, p2, p3, p4, cols=2)


p1 <- ggplot(data, aes(x = "",y=data$casual)) +
  geom_boxplot() + labs(title="box plot on casual") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$registered)) +
  geom_boxplot() + labs(title="box plot on registered")
p3 <- ggplot(data, aes(x = "",y=data$count)) +
  geom_boxplot() + labs(title="box plot on count")

multiplot(p1, p2, p3, cols=2)

# treating extream values

data$hum = replace(
  data$hum, 
  c(50), 
  c(0.507463)
)

data$windspeed = replace(
  data$windspeed, 
  c(50), 
  c(0.187917)
)

library(dplyr)

d = data %>% filter(between(instant,62,75))
mean(d$hum)

data$hum = replace(
  data$hum, 
  c(69), 
  c(0.572944)
)

# after treating extream values, lets see  box plot again

p1 <- ggplot(data, aes(x = "",y=data$hum)) +
  geom_boxplot() + labs(title="box plot on humidity") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$windspeed)) +
  geom_boxplot() + labs(title="box plot on windspeed")


multiplot(p1, p2, cols=1)

# density plots 

p1 <- ggplot(data, aes(temp)) +
  geom_density() + labs(title="distribution of temperature") # plots the results

p2 <- ggplot(data, aes(data$hum)) +
  geom_density() + labs(title="distribution of humidity")

p3 <- ggplot(data, aes(data$windspeed)) +
  geom_density() + labs(title="distribution of windspeed")

p4 <- ggplot(data, aes(data$count)) +
  geom_density() + labs(title="distribution of count")

multiplot(p1, p2,p2,p3,cols=2)

# categorical variables
weather = table(data$weathersit)

barplot(weather,xlab='weather',main='barplot of weather')

holiday = table(data$holiday)
barplot(holiday,xlab='holiday',main='barplot of holiday')

workinday = table(data$workingday)

barplot(workinday,xlab='wokingday',main='barplot of workingday')

# visualization of casual, registered, count


# 24 days visualization
d = data %>% filter(between(instant,1,24))

ggplot(d, aes(dateday, count,group = 1)) + geom_line() +
   xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))


# casual

data$Month_Yr <- format(as.Date(data$dateday), "%Y-%m")
ggplot(data, aes(Month_Yr,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# registered 

ggplot(data, aes(Month_Yr,data$registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# count

ggplot(data, aes(Month_Yr,data$count,group = 1)) + geom_line() 
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# viz of avg per month
  
avg_use_month <- data %>% select(casual,registered,count,month,year,Month_Yr) %>% group_by(Month_Yr) %>%
  summarise(casual = mean(casual),registered = mean(registered),count = mean(count))

ggplot(avg_use_month, aes(Month_Yr,registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_registered_per_month")

ggplot(avg_use_month, aes(Month_Yr,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_casual_per_month")

ggplot(avg_use_month, aes(Month_Yr,count,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_count_per_month")

# avg use by day

avg_use_day <- data %>% select(casual,registered,weekday) %>% group_by(weekday) %>%
  summarise(casual = mean(casual),registered = mean(registered))

ggplot(avg_use_day, aes(weekday,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_casual_per_day")

ggplot(avg_use_day, aes(weekday,registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_registered_per_day")

## week day for count 

d_weekday_false = data %>% select(Month_Yr,weekday,count,year) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
                      weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == FALSE) %>%
                                                  group_by(Month_Yr) %>% summarise(False = sum(count))


d_weekday_true = data %>% select(Month_Yr,weekday,count,year) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
               weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == TRUE) %>% group_by(Month_Yr) %>% summarise(True = sum(count))

d_week_end_day <- d_weekday_false
d_week_end_day$True <- d_weekday_true$True

ggplot(d_week_end_day, aes(Month_Yr,group=1)) +geom_line(aes(y = True, colour = "True")) + 
  geom_line(aes(y = False, colour = "False"))+
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Date vs Count (weekday)")



# casual 
d_weekday_false = data %>% select(Month_Yr,weekday,casual) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
                                                                          weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == FALSE) %>%
  group_by(Month_Yr) %>% summarise(False = sum(casual))


d_weekday_true = data %>% select(Month_Yr,weekday,casual) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
                                                                         weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == TRUE) %>% group_by(Month_Yr) %>% summarise(True = sum(casual))

d_week_end_day <- d_weekday_false
d_week_end_day$True <- d_weekday_true$True

ggplot(d_week_end_day, aes(Month_Yr,group=1)) +geom_line(aes(y = True, colour = "True")) + 
  geom_line(aes(y = False, colour = "False"))+
  xlab("Date") + ylab("casual")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Date vs casual (weekday)")

library(ggplot2)
# Registered
d_weekday_false = data %>% select(Month_Yr,weekday,registered,year) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
                                                                           weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == FALSE) %>%
  group_by(Month_Yr) %>% summarise(False = sum(registered))


d_weekday_true = data %>% select(Month_Yr,weekday,registered,year) %>% mutate(weekday1 = weekday > 5 & weekday == 0,
                                                                          weekday1 = weekday >=1  & weekday <= 5) %>% filter(weekday1 == TRUE) %>% group_by(Month_Yr) %>% summarise(True = sum(registered))

d_week_end_day <- d_weekday_false
d_week_end_day$True <- d_weekday_true$True

ggplot(d_week_end_day, aes(Month_Yr,group=1)) +geom_line(aes(y = True, colour = "True")) + 
  geom_line(aes(y = False, colour = "False"))+
  xlab("Date") + ylab("registered")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Date vs registered (weekday)")


# violin plot 
data$weathersit = as.factor(data$weathersit)
ggplot(data, aes(x=weathersit, y=count)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal() + ggtitle("violin plot on weathersit")

# scatter plot

# temp vs count
ggplot(data, aes(data$temp,data$count)) + geom_point() +
  xlab("Temp") + ylab("Count")+theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  ggtitle("Temperature vs Count")

# Multiple scatterplot
par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))
with(subset(data, season == 1), plot(temp, count, main = 1))
with(subset(data, season == 2), plot(temp, count, main = 2))
with(subset(data, season == 3), plot(temp, count, main = 3))
with(subset(data, season == 4), plot(temp, count, main = 4))

# humidity vs count
ggplot(data, aes(data$hum,data$count)) + geom_point() +
  xlab("Humidity") + ylab("Count")+theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  ggtitle("Humidity vs Count")

# Multiple scatterplot
par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))
with(subset(data, season == 1), plot(hum, count, main = 1))
with(subset(data, season == 2), plot(hum, count, main = 2))
with(subset(data, season == 3), plot(hum, count, main = 3))
with(subset(data, season == 4), plot(hum, count, main = 4))

## correlation plots 

library(Hmisc)

numeric_index = sapply(data,is.numeric) #selecting only numeric
library(corrplot)
dev.new(width=5, height=4)
M = cor(data[,numeric_index])

corrplot(M)
dev.off()

# linear regression 
library(fastDummies)
results <- dummy_cols(data
                      ,select_columns = c('weathersit','season','weekday'), remove_first_dummy = TRUE)

names(results)
data2 <- subset(results, select = c(6,8,10, 12, 13,18:28,16))

test = data2[710:731, ]
names(test)
train = data2[1:709,]
lm(count~.,data = train)

summary(Reg)
y_pred = predict(Reg,test[,c(1:16)])

error = test[,17] - y_pred

rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(error)

rss <- sum((y_pred - test[,17]) ^ 2)
tss <- sum((test[,17] - mean(test[,17])) ^ 2)
rsq <- 1 - rss/tss

# svm
names(results)
data2 <- subset(results, select = c(3,6:10,12, 13,16))

test = data2[710:731, ]
names(test)
train = data2[1:709,]

library(e1071)
x= tune.svm(count ~ . , data=train,cost=1:20, kernel =
           "linear")

model_svm <- svm(count ~ . , data=train, kernel =
                   "linear",cost=6)
print(model_svm)

#Use the predictions on the data

y_pred <- predict(model_svm, test[,-9])

error = test[,9] - y_pred

rmse(error)

rss <- sum((y_pred - test[,9]) ^ 2)
tss <- sum((test[,9] - mean(test[,9])) ^ 2)
rsq <- 1 - rss/tss

# decision trees

library(rpart)

# grow tree 
x <- tune.rpart(count ~ . , data=train,minsplit = c(1:10),
                  maxdepth = c(1:10))

fit <- rpart(count ~ . , data=train,minsplit = 2,
                maxdepth = 5)

library(caret)
imp = varImp(fit)

imp$names <- rownames(imp)

print(imp)

ggplot(data=imp, aes(x=names, y=Overall)) +
  geom_bar(stat="identity")

summary(fit)
library
fit$variable.importance


#Predict Output 
y_pred= predict(fit,test[,-9])

error = test[,9] - y_pred

rmse(error)

rss <- sum((y_pred - test[,9]) ^ 2)
tss <- sum((test[,9] - mean(test[,9])) ^ 2)
rsq <- 1 - rss/tss

library(Metrics)
metric = 'RMSE'
# define a grid of parameter options to try
control <- trainControl(method="repeatedcv", number=3, repeats=2, search="grid")
# selecting mtry randomly
tunegrid <- expand.grid(.mtry=c(1:15))

rf_gridsearch <- train(count ~ . , data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)


y_pred = predict(rf_gridsearch,test[,-9])

error <- test$count-y_pred

rmse(error)

best_mtry <- rf_gridsearch$bestTune$mtry 
best_mtry

# search for best max nodes

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = c(4))
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(count ~ . , data=train,maxnodes = maxnodes, method="rf", metric=metric, tuneGrid=tuneGrid , trControl=control)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# max node is 15

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550)) {
  set.seed(5678)
  rf_maxtrees <- train(count ~ . , data=train,maxnodes = 15, method="rf", metric=metric, 
                       tuneGrid=tuneGrid , trControl=control,ntree=ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

fit_rf <- train(count ~ . , data=train,maxnodes = 15, method="rf", metric=metric, 
                tuneGrid=tuneGrid , trControl=control,ntree=450,importance = TRUE)

fit_rf
# importance
varImp(fit_rf)

# prediction
y_pred = predict(fit_rf,test[,-9])
error <- test$count-y_pred
rmse(error)

rss <- sum((y_pred - test[,9]) ^ 2)
tss <- sum((test[,9] - mean(test[,9])) ^ 2)
rsq <- 1 - rss/tss

# extra tree 

library(extraTrees)

fit = extraTrees(train[,-9],train$count,ntree = 450,mtry = 4)

library(caret)
imp = varImp(fit)

imp$names <- rownames(imp)

print(imp)

ggplot(data=imp, aes(x=names, y=Overall)) +
  geom_bar(stat="identity")

summary(fit)
library
fit$variable.importance


#Predict Output 
y_pred= predict(fit,test[,-9])

error = test[,9] - y_pred

rmse(error)

rss <- sum((y_pred - test[,9]) ^ 2)
tss <- sum((test[,9] - mean(test[,9])) ^ 2)
rsq <- 1 - rss/tss

# Xgboost 

library(xgboost)

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.7, 1), 
                                colsample_bytree = c(0.6, 0.8,0.9 ,1))
ntrees <- 200

#Build a xgb.DMatrix object
train = as.matrix(train)

test= as.matrix(test)
DMMatrixTrain <- xgb.DMatrix(data = train[,-9], label = train[,9])

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  
  xgboostModelCV <- xgboost(data =  DMMatrixTrain, nrounds = ntrees,nfold = 2, showsd = TRUE, 
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = 3, "eta" = 2/ntrees,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  
  print(xgboostModelCV)
  xvalidationScores <- xgboostModelCV
  #xvalidationScores1 <-  as.data.frame(xgboostModelCV)
  #Save rmse of the last iteration
  rmse <- tail(xvalidationScores$test_rmse_mean, 1)
  
  return(c(rmse, currentSubsampleRate, currentColsampleRate))
  
})

fit =  xgboost(data =  DMMatrixTrain, nrounds = ntrees,nfold = 2, showsd = TRUE, 
                                metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                                "objective" = "reg:linear", "max.depth" = 3, "eta" = 2/ntrees,                               
                                "subsample" = 1, "colsample_bytree" = 0.9)

y_pred = predict(fit,test[,-9])

error = test[,9] - y_pred

rmse(error)

rss <- sum((y_pred - test[,9]) ^ 2)
tss <- sum((test[,9] - mean(test[,9])) ^ 2)
rsq <- 1 - rss/tss

# feature importance
model = xgb.dump(fit, with_stats=TRUE)
# get the feature real names
names = dimnames(train)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=fit)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 