##########################FRA PROJECT


#Setting up working directory
setwd("C:/Users/mchoudhu/Documents/PGP-BABI")

#Installing and loading necessary packages
install.packages("readxl")
install.packages("car")
install.packages("caret")
install.packages("class")
install.packages("devtools")
install.packages("e1071")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("klaR")
install.packages("MASS")
install.packages("nnet")
install.packages("dplyr")
install.packages("plyr")
install.packages("pROC")
install.packages("psych")
install.packages("scatterplot3d")
install.packages("SDMTools")
install.packages("dplyr")
install.packages("ElemStatLearn")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("neuralnet")

library(readxl)
library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
#library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)


data_train = read_excel("raw-data.xlsx",sheet = 1)
data_test = read_excel("validation_data.xlsx")
attach(data_train)
attach(data_test)

#Understanding the dataset using the following commands
head(data_train)
dim(data_train)
names(data_train)
str(data_train)
summary(data_train)

head(data_test)
dim(data_test)
names(data_test)
str(data_test)
summary(data_test)



data_train$Default = ifelse(data_train$'Networth Next Year' < 0 ,1,0)

#Removing companies with Total Assets less than 5
data_train = data_train[!data_train$`Total assets` <= 5, ]

#Missing Value Treatment


data_train<-as.data.frame(data_train)

#Identifying the class of the variables in train
for(i in 1:length(data_train))
  {print(paste(colnames(data_train[i]),class(data_train[,i])))}

#Missing values in each column
colSums(is.na(data_train)) 

#Missing columns, rows and observations
plot_intro(data_train)

install.packages("DataExplorer")
library(DataExplorer)


#Data Transformation
for(i in 1:ncol(data_train))
  {data_train[,i] <- as.numeric(data_train[,i])
  data_train[is.na(data_train[,i]), i] <- median(data_train[,i], na.rm = TRUE)
}

#Removing column with missing values
data_train <- data_train[,-22]

plot_intro(data_train)




#Same data transformation on Test data
data_test<-as.data.frame(data_test)
for(i in 1:length(data_test))
  {print(paste(colnames(data_test[i]),class(data_test[,i])))}
plot_intro(data_test)


for(i in 1:ncol(data_test))
  {
  data_test[,i] <- as.numeric(data_test[,i])
  data_test[is.na(data_test[,i]), i] <- median(data_test[,i], na.rm = TRUE)
  }
plot_intro(data_test)


data_test <- data_test[,-22]



#Outlier Identification and Treatment


for(i in 2:ncol(data_train))
  {
  q = quantile(data_train[,i], c(0.5, 0.95))
  data_train[,i] = squish(data_train[,i], q)
}

#??squish

install.packages("scales")
library(scales)

data_train <- data_train[,-c(1,2)]
data_test <- data_test[,-1]

#Exploratory Data Analysis

plot_str(data_train)
plot_intro(data_train)

#par(par("mar"))
#par(mfrow = c(3,4))
#par(mfrow=c(2,2))
#graphics.off()
dev.off()
plot_histogram(data_train)
plot_qq(data_train)
plot_bar(data_train)



install.packages("corrplot")
library(corrplot)

#Correlation plot
plot_correlation(data_train)
corrplot(cor(data_train),method = "pie", type = "upper")



#Variable Transformation

#Training set
data_train$Profitability = data_train$`Profit after tax`/data_train$Sales
data_train$Pricepershare = data_train$EPS*data_train$`PE on BSE`
data_train$NWCbyTA = data_train$`Net working capital`/data_train$`Total assets`
data_train$TotalEquity = data_train$`Total liabilities`/data_train$`Debt to equity ratio (times)`
data_train$EquityMultiplier = data_train$`Total assets`/data_train$TotalEquity
data_train$NWbyTA <- data_train$`Net worth`/data_train$`Total assets`
data_train$TotalincomebyTA<- data_train$`Total income`/data_train$`Total assets`
data_train$TotalexpensesbyTA <-data_train$`Total expenses`/data_train$`Total assets`
data_train$ProfitaftertaxbyTA <-data_train$`Profit after tax`/data_train$`Total assets`
data_train$PBTbyTA <-data_train$PBT/data_train$`Total assets`
data_train$SalesbyTA <-data_train$Sales/data_train$`Total assets`
data_train$CurrentliabilitiesbyTA <-data_train$`Current liabilities & provisions`/data_train$`Total assets`
data_train$CapitalemployedbyTA <-data_train$`Capital employed`/data_train$`Total assets`
data_train$NetfixedassetsbyTA <-data_train$`Net fixed assets`/data_train$`Total assets`
data_train$InvestmentsbyTA <-data_train$Investments/data_train$`Total assets`
data_train$TotalliabilitiesbyTA <-data_train$`Total liabilities`/data_train$`Total assets`



#Test set
data_test$Profitability <- data_test$`Profit after tax`/data_test$Sales
data_test$Pricepershare <- data_test$EPS*data_test$`PE on BSE`
data_test$NWCbyTA <- data_test$`Net working capital`/data_test$`Total assets`
data_test$TotalEquity <- data_test$`Total liabilities`/data_test$`Debt to equity ratio (times)`
data_test[is.infinite(data_test[,54]), 54] <- 0
data_test$EquityMultiplier <- data_test$`Total assets`/data_test$TotalEquity
data_test[is.infinite(data_test[,55]), 55] <- 0
data_test$NWbyTA <- data_test$`Net worth`/data_test$`Total assets`
data_test$TotalincomebyTA<- data_test$`Total income`/data_test$`Total assets`
data_test$TotalexpensesbyTA <-data_test$`Total expenses`/data_test$`Total assets`
data_test$ProfitaftertaxbyTA <-data_test$`Profit after tax`/data_test$`Total assets`
data_test$PBTbyTA <-data_test$PBT/data_test$`Total assets`
data_test$SalesbyTA <-data_test$Sales/data_test$`Total assets`
data_test$CurrentliabilitiesbyTA <-data_test$`Current liabilities & provisions`/data_test$`Total assets`
data_test$CapitalemployedbyTA <-data_test$`Capital employed`/data_test$`Total assets`
data_test$NetfixedassetsbyTA <-data_test$`Net fixed assets`/data_test$`Total assets`
data_test$InvestmentsbyTA <-data_test$Investments/data_test$`Total assets`
data_test$TotalliabilitiesbyTA <-data_test$`Total liabilities`/data_test$`Total assets`


#Logistic Regression with all variables

alltrainLOGIT = glm(Default~. ,data = data_train, family=binomial)

summary(alltrainLOGIT)

anova(alltrainLOGIT, test = "Chisq")



#Logistic Regression with only important variables
trainLOGITimp <- glm(Default~`Total assets`+`Total income`+`Change in stock`+`Total expenses`
                  +`Profit after tax`+PBDITA+`Cash profit`+`PBDITA as % of total income`+
                    `PBT as % of total income`+`PAT as % of total income`+`Cash profit as % of total income`+
                    `PAT as % of net worth`+`Total capital`+`Reserves and funds`+`Borrowings`+
                    `Current liabilities & provisions`+`Capital employed`+
                    `Total term liabilities / tangible net worth`+`Contingent liabilities`+
                    `Current ratio (times)`+Investments+`Finished goods turnover`+`TOL/TNW`+`PE on BSE` 
                  +`Net fixed assets`+`Debt to equity ratio (times)`+
                    `Cash to average cost of sales per day`+Pricepershare+NWCbyTA+NWbyTA+
                    SalesbyTA+ CapitalemployedbyTA+ InvestmentsbyTA , data= data_train, family = binomial)
summary(trainLOGITimp)
anova(trainLOGITimp, test = "Chisq")

vif(trainLOGITimp)

trainLOGITsig = glm(Default~`Total assets`+`Total income`+`Total expenses`
                     +`Profit after tax`+`Cash profit`+`PBDITA as % of total income`+
                       `PBT as % of total income`+`PAT as % of total income`+
                       `Cash profit as % of total income`+
                       `PAT as % of net worth`+`Total capital`+`Reserves and funds`+`Borrowings`+
                       `Current liabilities & provisions`+`Capital employed`+
                       `Total term liabilities / tangible net worth`+
                       `Current ratio (times)`+`TOL/TNW`+`PE on BSE` 
                     +`Debt to equity ratio (times)`+
                       `Cash to average cost of sales per day`+Pricepershare, 
                     data= data_train, family = binomial)
summary(trainLOGITsig)
anova(trainLOGITsig)


#Model Performance measures

PredLOGIT <- predict.glm(trainLOGITsig, newdata=data_train, type="response")

tab.logit<-confusion.matrix(data_train$Default,PredLOGIT,threshold = 0.5)
tab.logit

accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit


#Precision
precision = tab.logit[2,2]/(tab.logit[2,2]+tab.logit[2,1])
precision

#Sensitivity
sensitivity(tab.logit)

#Specificity
specificity(tab.logit)

roc.logit<-roc(data_train$Default,PredLOGIT)
roc.logit

plot(roc.logit)


#Applying the same model on Test data
PredLOGITtest <- predict.glm(trainLOGITsig, newdata=data_test, type="response")
tab.logittest<-confusion.matrix(data_test$`Default - 1`,PredLOGITtest,threshold = 0.5)
tab.logittest

accuracy.logittest<-sum(diag(tab.logittest))/sum(tab.logittest)
accuracy.logittest

roc.logittest<-roc(data_test$`Default - 1`,PredLOGITtest)
roc.logittest

plot(roc.logittest)

#Recall
recall=tab.logittest[2,2]/(tab.logittest[2,2]+tab.logittest[1,2])
recall

precision
#Precision
precision = tab.logittest[2,2]/(tab.logittest[2,2]+tab.logittest[2,1])

#Sensitivity
sensitivity(tab.logittest)

#Specificity
specificity(tab.logittest)


#Checking accuracy of model using Deciling method

data_train$pred = predict(trainLOGITsig, data_train, type="response")

decile <- function(x)
{
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1))
  {
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

data_train$deciles <- decile(data_train$pred)
temp_DT = data.table(data_train)


rank <- temp_DT[, list(cnt=length(Default),cnt_resp=sum(Default==1),
                      cnt_non_resp=sum(Default==0)), by=deciles][order(-deciles)]

rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
data_trainRank <- rank

View(rank)

data_test$pred = predict(trainLOGITsig, data_test, type="response")

decile <- function(x)
  {
   deciles <- vector(length=10)
   for (i in seq(0.1,1,.1))
   {
   deciles[i*10] <- quantile(x, i, na.rm=T)
   }
   return (
   ifelse(x<deciles[1], 1,
          ifelse(x<deciles[2], 2,
                 ifelse(x<deciles[3], 3,
                        ifelse(x<deciles[4], 4,
                               ifelse(x<deciles[5], 5,
                                      ifelse(x<deciles[6], 6,
                                             ifelse(x<deciles[7], 7,
                                                    ifelse(x<deciles[8], 8,
                                                           ifelse(x<deciles[9], 9, 10
                                                                  ))))))))))
}


data_test$deciles <- decile(data_test$pred)

temp_DTest = data.table(data_test)

rank <- temp_DTest[, list(cnt=length(`Default - 1`),
                      cnt_resp=sum(`Default - 1`==1),
                      cnt_non_resp=sum(`Default - 1`==0)
                      ), by=deciles][order(-deciles)]



rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
data_testRank<-rank

View(rank)


# cut_p returns the cut internal for each observation
cut_ptrain = with(data_train,cut(pred, breaks = quantile(pred, prob=seq(0,1,0.1)), include.lowest = T))
cut_ptest = with(data_test,cut(pred, breaks = quantile(pred, prob=seq(0,1,0.1)), include.lowest = T))
levels(cut_ptrain)
levels(cut_ptest)
data_train$rank = factor(cut_ptrain, labels = 1:10)
data_test$rank = factor(cut_ptest, labels = 1:10)

mean.obs.train = aggregate(Default ~ rank, data = data_train, mean)
mean.pred.train = aggregate(pred ~ rank, data = data_train, mean)

mean.obs.val = aggregate( `Default - 1`~ rank, data = data_test, mean)
mean.pred.val = aggregate(pred ~ rank, data = data_test, mean)

# plot the mean vs deciles
par(mfrow=c(1,2))
plot(mean.obs.train[,2], type="b", col="black", ylim=c(0,0.8), xlab="Decile", ylab="Prob")
lines(mean.pred.train[,2], type="b", col="red", lty=2)
title(main="Training Data")

plot(mean.obs.val[,2], type="b", col="black", ylim=c(0,0.8), xlab="Decile", ylab="Prob")
lines(mean.pred.val[,2], type="b", col="red", lty=2)
title(main="Test Data")


###########################################################################################





















