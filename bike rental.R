
getwd()
setwd("C:/Users/Aswani/Documents/edwisor 1")

rm(list=ls())

df=read.csv("day.csv")
str(df)

# converting variable dteday from factor to date

df$dteday = as.Date(df$dteday,format="%Y-%m-%d")

str(df)

# extracting day from dates

df$day = format(df$dteday,"%d")

df$day

#checking if there is any relationship between day and cnt

plot(df$day,df$cnt)
#therefore we can conclude there is no relationship between day & cnt

#Dropping variables - instant(it represents the index of records),dteday(since mnth & yr are present),day,casual,registered(since cnt will be used as dependent variable)

df= subset(df,select=-c(instant,dteday,casual,registered,day))

# Data Distribution Visualizations
library(ggplot2)

# Visualizing variable temp distribution

ggplot(df,aes_string(x=df$temp))+geom_histogram()+xlab('temp')+ylab('freq')

# Visualizing  atemp distribution

ggplot(df,aes_string(x=df$atemp))+geom_histogram()+xlab('atemp')+ylab('freq')

# Visualizing hum distribution

ggplot(df,aes_string(x=df$hum))+geom_histogram()+xlab('hum')+ylab('freq')

# Visualizing  windspeed distribution

ggplot(df,aes_string(x=df$windspeed))+geom_histogram()+xlab('windspeed')+ylab('freq')

# Visualizing  cnt distribution

ggplot(df,aes_string(x=df$cnt))+geom_histogram()+xlab('cnt')+ylab('freq')

# Boxplots for temp,atemp,hum & windspeed

boxplot(df[,c('temp','atemp','hum','windspeed')])
# hum & windspeed have outliers

# Boxplot for cnt

boxplot(df[,'cnt'],xlab='cnt')

# cnt has no outliers

# Outliers will be replaced with NAs

for (i in c('hum','windspeed')){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df[,i][df[,i] %in% val] = NA
} 

# checking for missing values

as.data.frame(colSums(is.na(df)))


# We will use mean substitution,median substitution and KNN imputation for missing values and determine which method is the best method for missing values replacement.

# Taking first row of df and value of hum as our test value.We will put this value of 0.8058 to NA and use the 3 methods to check which method gives the replacement closest to the original value.

# Checking mean substitution

# making a copy of df for mean substitution

df1 = df

df1[1,'hum'] = NA

head(df1)

df1$hum[is.na(df1$hum)] = mean(df1$hum,na.rm = T)

df1$hum[1]   #0.629

#So,mean substitution has given a value of 0.6291 and original value was 0.8058

# Checking median substitution

df2 = df

df2[1,'hum'] = NA

df2$hum[is.na(df2$hum)] = median(df2$hum,na.rm=T)

df2$hum[1]#0.6270

#So,median substitution has given a value of 0.6270 and original value was 0.8058

# Checking KNN imputation

library(DMwR)

df3 = df

df3$hum[1] = NA

df3 = knnImputation(df3,k=3)

df3$hum[1]#0.7459

# KNN imputation has given the value of 0.7459 which is closest to 0.8058.So,KNN imputation will be used to replace missing values.

df = knnImputation(df,k=3)

as.data.frame(colSums(is.na(df)))

# Correlation Analysis

# Converting season,yr,mnth,holiday,weekday,workingday,weathersit to factor

catcols = c('season','yr','mnth','holiday','weekday','workingday','weathersit')

for(i in catcols){
  df[,i] = as.factor(df[,i])
}

str(df)

# Chi-square test for correlation between factors

pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(df[,i],df[,j])
    pval = c(pval,chi2$p.value)
  }
}

length(pval)#49

#converting pval to matrix m1

m1 = matrix(pval,ncol=7)
m1

#Converting m1 to dataframe chi_df

chi_df = data.frame(m1)

#Setting row names to catcols

row.names(chi_df) = catcols

#Setting column names to catcols

colnames(chi_df) = catcols

chi_df


#workingday vs weekday and holiday vs weekday have p-value < 0.01 so we can drop workingday and holiday.

#Dropping workingday & holiday from df

df= subset(df,select=-c(holiday,workingday))

head(df)

dim(df)

#Correlation between continuous independent variables

cor(df[,6:9])

#temp and atemp have correlation coeff. of 0.99 so we will drop atemp

df[,'atemp'] = NULL

dim(df)

head(df)

# Bikes Count Vs. Season

colList = scales::hue_pal()(4)

ggplot(df, aes_string(x=df$season,y=df$cnt,fill=df$season))+
  geom_boxplot()+
  xlab('Season')+ylab('cnt')+
  ggtitle("Bikes Count By Season")+
  scale_fill_manual('Seasons',labels=c('spring', 'summer', 'fall', 'winter'),values = colList)

#Most bike rentals occur in Fall followed by Summer.

# Bikes Count Vs. Weekday
col1 = scales::hue_pal()(7)

ggplot(df, aes_string(x=df$weekday,y=df$cnt,fill=df$weekday))+
  geom_boxplot()+
  xlab('weekday')+ylab('cnt')+
  ggtitle("Bikes Count By weekdays")+
  scale_fill_manual('Weekdays',labels=c('sun','mon','tue','wed','thu','fri','sat'),values = col1)

# Bikes Count Vs. Weather
ggplot(df, aes_string(x=df$weathersit,y=df$cnt,fill=df$weathersit))+
  geom_boxplot()+
  xlab('Weather')+ylab('cnt')+
  ggtitle("Bikes Count By Weather")+
  scale_fill_manual('Weather',labels=c('clear', 'cloudy', 'thunderstorm & rain'),values = colList)

#Most bikes are rented during clear weather

# Bike Counts Vs. temperature
ggplot(df, aes_string(x=df$temp,y=df$cnt))+
  geom_point()+
  xlab('temp')+ylab('cnt')+
  ggtitle("Bikes Count Vs temperature")+geom_smooth()

# As temp increases,bikes count increases.

# Bike Counts Vs. humidity
ggplot(df, aes_string(x=df$hum,y=df$cnt))+
  geom_point()+
  xlab('hum')+ylab('cnt')+
  ggtitle("Bikes Count Vs humidity")+geom_smooth()

# As humidity increases,bike count decreases.

# Bike Counts Vs. Windspeed
ggplot(df, aes_string(x=df$windspeed,y=df$cnt))+
  geom_point()+
  xlab('windspeed')+ylab('cnt')+
  ggtitle("Bikes Count Vs Windspeed")+geom_smooth()

#As windspeed increases,bike count decreases.


# MODELLING

# Splitting df in train & test sets in the ratio of 80:20

require(caTools)

set.seed(101)

sample = sample.split(df$cnt, SplitRatio = .80)

train = subset(df, sample == TRUE)

test  = subset(df, sample == FALSE)

dim(train) #584   9

dim(test) #147   9

# LINEAR REGRESSION
model_lm = lm(cnt~.,train)

summary(model_lm)
# R-squared is 0.854 which means that about 85% of variance in dependent variable can be explained by variance in independent variables.
# temp has coeff. of 4196.15 which means that temp is having the most(positive) effect on cnt.
# windspeed has coeff. of -2211.15 which means that windspeed is having the second most effect(negative) on cnt.


#Predicting for test set

pred_lm = predict(model_lm,test[,1:8])

pred_lm[1:10] # pred_lm has some negative values.

#Putting negative values to 0.

pred_lm = ifelse(pred_lm < 0,0,pred_lm)

pred_lm[1:10]

#rounding off

pred_lm = ceiling(pred_lm)

# Calculating Mean Absolute Percent Error(MAPE)

library(Metrics)

mape(test[,9],pred_lm) #  1.335392

# Calculating Mean Absolute Error(MAE)

mae(test[,9],pred_lm) # 602.3673

rmse(test$cnt,pred_lm) #880


# DECISION TREE

library(rpart)

model_DT = rpart(cnt~.,train,method='anova')

pred_DT = predict(model_DT,test[,1:8])

pred_DT[1:20]

pred_DT = ceiling(pred_DT)

mape(test[,9],pred_DT)#  1.965942

mae(test[,9],pred_DT)# 678.0204

rmse(test$cnt,pred_DT) 


# RANDOM FOREST

library(randomForest)

model_RF = randomForest(cnt~.,train,importance=T,ntree=500)

pred_RF = predict(model_RF,test[,1:8])

pred_RF = ceiling(pred_RF)

mape(test[,9],pred_RF)#  1.371341

mae(test[,9],pred_RF)# 558.6327

rmse(test$cnt,pred_RF) #817.69


# writing test actual count and predicted count in a dataframe

out = data.frame(test[,9])

colnames(out) = 'Actual'

out$Predicted = pred_RF

head(out)

write.csv(out,"C:/Users/Aswani/Documents/edwisor 1/outputR.csv",row.names = F)



#FROM THE ABOVE IT IS CLEAR THAT AMONG ALL MODELS THAT I USED,RANDOM FOREST IS GIVING BETTER ACCURACY ..
#SOO IT IS USED FOR FURTHER PURPOSE