rm(list=ls())

# set the working directory
setwd("C:/Users/Chaitrali/Downloads/Data/data01s2l1/Edwisor-Project/CabfarePred")
getwd()

#load libraries
x=c("ggplot2", "DMwR", "corrgram", "Hmisc", "rpart", "randomForest", "geosphere")
lapply(x, require, character.only = TRUE)
rm(x)

# Load the input data
train= read.csv("train_cab.csv", header = T)[,-2]

#Check if the inout data is in a proper shape
str(train)

#convert fare amount into proper type
train$fare_amount=as.numeric(as.character(train$fare_amount))
train$passenger_count=as.integer(train$passenger_count)

#Remove all cell where the pickup and fropoff location is same
train=subset(train, !(train$pickup_longitude==train$dropoff_longitude & train$pickup_latitude==train$dropoff_latitude))
#replace all "0" with NA
train[train==0]= NA

#create a function to calculate missing values
missingvalue= function(data){
  mv=data.frame(apply(data, 2 , function(x){sum(is.na(x))}))
  colnames(mv)="Missing_val_count"
  mv$percent=apply(mv , 1 , function(x){x/nrow(train)*100})
  mv=cbind(row.names(mv), mv)
  row.names(mv)=NULL
  colnames(mv)[1]="variables"
  print(mv)
  
  
  #plot Missing Values
  ggplot(data = mv, aes(x=reorder(variables , -percent),y = percent))+
  geom_bar(stat = "identity",fill = "blue")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()
}


#Calculate Missing Values
missingvalue(train)

#Since PAssenger_count is categorical, hence replace it using mode 
#create a function to calculate mode
mode= function(data){
  uniq=unique(data)
  as.numeric(as.character(uniq[which.max(tabulate(match(data,uniq)))]))
  #print(mode_d)
}

mode(train$passenger_count)

#impute with the mode
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)


#Check suitable methods to impute Missing value for numerical data
#train[1,2]=-73.84431
#Mean= -73.91186
#Median=-73.98204
#KNN= -73.84371
df=train
train=train[complete.cases(train[,1]),]
#Since KNN is giving the value closest to the original one, We choose KNN for missing value imputation
train=knnImputation(train, k=1)

missingvalue(train)

#####################Outlier Analysis############################################33
df=train
#outliers in fare_amount
#Remove negative values from 'fare_amount'
train$fare_amount=ifelse(train$fare_amount<0, NA, train$fare_amount)
train$fare_amount=ifelse(train$fare_amount>30,NA, train$fare_amount)

#outliers in passenger_count
#all values greater than 8 are converted to NA
unique(train$passenger_count)

#method 1
#for (i in 1:nrow(train)){
  if (as.integer(train$passenger_count[i]) > 8){
    train$passenger_count[i]=NA
  }
}
#method2
train['passenger_count'][train['passenger_count']<0 | train['passenger_count']>8]= NA

#Outliers in Pickup_latitude

#Get the range of the locations
range(train$pickup_longitude)
range(train$pickup_latitude)
range(train$dropoff_longitude)
range(train$dropoff_latitude)

cnames=colnames(train[,c(2:5)])

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = train)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="y")+
           ggtitle(paste("Box plot of fare amount",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1, gn2, ncol=2)
gridExtra::grid.arrange(gn3, gn4,gn5, ncol=3)

#Replace all outliers with NA and impute
#create NA on outliers
for(i in cnames){
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  print(length(val))
  train[,i][train[,i] %in% val] = NA
}

missingvalue(train)

#replace missinf value with mode
mode(train$passenger_count)
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)
train=train[complete.cases(train[,1]), ]

#replace all other missing value with mean
train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm=T)
train$pickup_longitude[is.na(train$pickup_longitude)] = mean(train$pickup_longitude, na.rm=T)
train$pickup_latitude[is.na(train$pickup_latitude)] = mean(train$pickup_latitude, na.rm=T)
train$dropoff_longitude[is.na(train$dropoff_longitude)] = mean(train$dropoff_longitude, na.rm=T)
train$dropoff_latitude[is.na(train$dropoff_latitude)] = mean(train$dropoff_latitude, na.rm=T)

#now convert Passenger_count into factor
train$passenger_count=as.factor(train$passenger_count)
#########################Feature Selection######################
df=train

#create new variable
library(geosphere)
train$dist= distHaversine(cbind(train$pickup_longitude, train$pickup_latitude), cbind(train$dropoff_longitude,train$dropoff_latitude))
#the output is in metres, Change it to kms
train$dist=as.numeric(train$dist)/1000
df=train
train=df

#correlation analysis
corrgram(train[,-6], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Finding the correlation between the numeric variables
num_cor=round(cor(train[,-6]), 3)


#Remove all cell where the pickup and dropoff location is same (if any)
train=subset(train, !(train$pickup_longitude==train$dropoff_longitude & train$pickup_latitude==train$dropoff_latitude))
#remove unnecessary variables
rm(abc,df,gn1,gn2,gn3,gn4,cnames,i,val)

##########################MODELDEvelopment###################################################3

#create sampling and divide data into train and test
set.seed(123)
train_index = sample(1:nrow(train), 0.8 * nrow(train))

train1 = train[train_index,]#do not add column if already removed
test1 = train[-train_index,]#do not add column if already removed

# Define Mape
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y*100))
}

#Decision Tree
fit = rpart(fare_amount ~. , data = train1, method = "anova", minsplit=5)
summary(fit)
predictions_DT = predict(fit, test1[,-1])
MAPE(test1[,1], predictions_DT)write.csv(predictions_DT, "DT_R_PRed5.csv", row.names = F)
#Error 27.0018


#Random Forest
RF_model = randomForest(fare_amount ~.  , train1, importance = TRUE, ntree=100)
RF_Predictions = predict(RF_model, test1[,-1])
MAPE(test1[,1], RF_Predictions)
importance(RF_model, type = 1)
#error 22.43 for n=100

#Linear Regression
lm_model = lm(fare_amount ~. , data = train1)
summary(lm_model)
predictions_LR = predict(lm_model, test1[,-1])
MAPE(test1[,1], predictions_LR)
#error 25.87

##KNN Implementation
library(class)
#Predict test data
KNN_Predictions = knn(train1[, 2:7], test1[, 2:7], train1$fare_amount, k = 1)
#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))
#Calculate MAPE
MAPE(test1[,1], KNN_Predictions)
#error 32.61

##############FIne Tuning###########################3333

#Random Forest
RF_model = randomForest(fare_amount ~.  , train1, importance = TRUE, ntree=200, mtry=2)
RF_Predictions = predict(RF_model, test1[,-1])
MAPE(test1[,1], RF_Predictions)
importance(RF_model, type = 1)
#error 22.38 for n=100

rm(a, num_cor,pre, i)
###################################Predict VAlues###################3

pred_data=read.csv("test.csv", header= T)[,-1]

#create dist variable
pred_data=subset(pred_data, !(pred_data$pickup_longitude==pred_data$dropoff_longitude & pred_data$pickup_latitude==pred_data$dropoff_latitude))
pred_data[pred_data==0]= NA

# Onvert Data into proper data types
str(pred_data)
pred_data$passenger_count=as.factor(pred_data$passenger_count)

#calculate dist
pred_data$dist= distHaversine(cbind(pred_data$pickup_longitude, pred_data$pickup_latitude), cbind(pred_data$dropoff_longitude,pred_data$dropoff_latitude))

#the output is in metres, Change it to kms
pred_data$dist=as.numeric(pred_data$dist)/1000

# Create the target variable
pred_data$fare_amount=0
pred_data=pred_data[,c(7,1,2,3,4,5,6)]

#Random Forest
RF_model = randomForest(fare_amount ~.  , train, importance = TRUE, ntree=200, mtry=2)
pred_data$fare_amount = predict(RF_model, pred_data[,-1])


