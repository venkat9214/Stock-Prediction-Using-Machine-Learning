#Predictive Assignment 4.2

#Setting the working directory for the project and reading the source data
setwd('/Users/ravitejaayyagari/Documents/Teja/Saint Peters/3 Sem/Predictive')

IWB<-read.csv('IWB_holdings.csv', header = T, sep =',')
IWM<-read.csv('IWM_holdings.csv', header = T, sep =',')
stock_data<-read.csv('data_file_ARQ.csv', header = T, sep =',')

#initializing the returns vector and loading values into it
return_price<-vector();

#Calculating the returnprice for each price value
for(i in 2:length(stock_data[,1])){
  if (identical(stock_data[i,1],stock_data[i-1,1])){
    return_price[i] = (stock_data[i,72]/stock_data[i-1,72]);
  }else{
    return_price[i] = 0;
  }
}

return_price[1]=0;
#combining columns wise 
stock_data<-cbind(stock_data,return_price)

#Removing the Variables with more than 60% of blank values and then performing the Na.Omit
stock_data1<- subset(stock_data, select = -c(`assetsavg`, `assetturnover`, `equityavg`, `invcapavg`, `liabilitiesnc`, `roa`, `roe`, `roic`, `ros`))
stock_data2 <- na.omit(stock_data1)

#We decide on the top 20 indicators
stock_final<-subset(stock_data2,select = c(`ticker`, `calendardate`,`pe1`,`evebit`,`evebitda`,`pe`,`pb`,`price`,`marketcap`,`capex`,`ps`,`taxexp`,`ev`,`ps1`,`cashnequsd`,`fcfps`,`intexp`,`assetsnc`,`eps`,`assetsc`,`ebitdausd`,`netinccmnusd`,`return_price`))
names(stock_final)[1] <- "Ticker"
#Again removing NAs
stock_final <- na.omit(stock_final)

Sector1 <- IWB[IWB$Sector == "Health Care",]
Sector2 <- IWM[IWM$Sector == "Health Care",]

Sector_HC <- rbind(Sector1,Sector2)

Sector_HC <- subset(Sector_HC, select = -c(`Name`, `Asset.Class`,`Weight....`, `Price`, `Shares`, `Market.Value`, `Notional.Value`, `Sector`, `SEDOL`, `ISIN`, `Exchange`))

HC_ARQ <- merge(stock_final,Sector_HC,by="Ticker")

#Converting the calendardate column to date
HC_ARQ$calendardate <- as.Date(HC_ARQ$calendardate, format = "%Y-%m-%d")

#Again, finding the unique date values and converting them to date format
cal_date<-unique(HC_ARQ$calendardate)
cal_date <- as.data.frame(cal_date)

#Removing Outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


for (i in 3:22){
  HC_ARQ[,i] <-remove_outliers(HC_ARQ[,i])
}

#Removing na.s
HC_ARQ <- na.omit(HC_ARQ)

#Normalizing Data
HC_ARQ1 <- as.data.frame(scale(HC_ARQ[,3:22])) 

#Traditional way of doing normalizing
# for(k in 2:21){
#   for(i in 1:dim(stock_final)[1]){
#     y[i,k-1]<-(stock_final[i,k]-mean(stock_final[,k]))/sd(stock_final[,k])
#   }
# }

#Combining the Datasets
HC_Data <- cbind(HC_ARQ1, HC_ARQ$calendardate, HC_ARQ$return_price)

#Renaming Columns
names(HC_Data)[21] <- 'calendardate'
names(HC_Data)[22] <- 'return_price'

#Creating dataframe for storing betas
nms <- sample(LETTERS,sample(1:10))
betas<-as.data.frame(t(matrix(nrow=length(nms),ncol=0,dimnames=list(nms))))

#Unique Calendar Dates
cal_date<-unique(HC_Data$calendardate)
cal_date <- as.data.frame(cal_date)

#Running the for loop for only 15 dates
for (i in 1:15){
  data_model<-subset(HC_Data, HC_Data$calendardate==cal_date[i,])
  colnames(data_model)<-colnames(HC_Data)
  data_model<-subset(data_model, select = -c(`calendardate`))
  model<-lm(log(data_model$return_price)~.,data=data_model)
  print(summary(model))
  for (j in 1:21){
    betas[i,j] <-model$coefficients[j]
  }
}

#Preparing Data for Time Series
data_ts<- HC_Data[order(HC_Data$calendardate),]
cal_date<-unique(HC_Data$calendardate)
cal_date<-as.data.frame(cal_date)
cal_date<-cal_date[order(cal_date),]

#Time Series
tr <- betas
tr$date <- cal_date[1:15]
tr$date <- as.Date(tr$date, format = "%Y-%m-%d")

#Performing Arima Time Series and Predicting betas for dates 16-19
library(forecast)
for (j in 1:21){
  for (i in 16:19){
    coeff <- ts(tr[,j], start = c(2011,03), end = c(2014,09), frequency = 15+(i-15))
    print(tr[,j])
    fit <- arima(coeff, order = c(1,0,2))
    pr <- predict(fit,n.ahead = 1)
    tr[i, j] <- as.numeric(pr$pred)
  }
}

library(sqldf)

cal_date<-as.data.frame(cal_date[16:19])
data_ts<-sqldf('select * from data_ts where calendardate in cal_date')

#Predicting Expected Log Returns - Named as Target
nms <- sample(LETTERS,sample(1:10))
target_return<-as.data.frame(t(matrix(nrow=length(nms),ncol=0,dimnames=list(nms))))
target<-0
gg <-0
data_ts$target <-c()
for (i in 1:dim(data_ts)[1]){
  a <- ifelse(data_ts$calendardate[i] == "2014-12-31", 16, ifelse(data_ts$calendardate[i] == "2015-03-31",17, ifelse(data_ts$calendardate[i] == "2015-06-30", 18, 19)))
  for (j in 1:20){
    gg<- data_ts[i,j]*tr[a,(j)]
    target = target + gg
  }
  gg= 0
  data_ts$target[i] <- log(target + tr[a,1])
}

#Storing the dataset into data_test
data_test <- data_ts
data_test <- na.omit(data_test)
data_test$log_return_price <- log(data_test$return_price)

data_test <- data_test[order(data_test$target),]
row.names(data_test)= (1:nrow(data_test))

#Dividing the Dataset into 5 buckets

#Before that removing na's
data_test <- na.omit(data_test)

#Creating the Column grade for creating buckets based on the order
#of epected return Values i.e. target and initializing with a value
data_test$grade <- "A"

#Now creating the buckets
data_test$grade[1:(dim(data_test)[1]/5)] <- "A"
data_test$grade[(dim(data_test)[1]/5)+1:((dim(data_test)[1]/5)*2)] <- "B"
data_test$grade[(((dim(data_test)[1]/5)*2)+1):((dim(data_test)[1]/5)*3)] <- "C"
data_test$grade[(((dim(data_test)[1]/5)*3)+1):((dim(data_test)[1]/5)*4)] <- "D"
data_test$grade[(((dim(data_test)[1]/5)*4)+1):dim(data_test)[1]] <- "E"

#Creating New DataFrame df_grade for computing the mean

df_grade <- as.data.frame(t(matrix(nrow=length(nms),ncol=0,dimnames=list(nms))))

names(df_grade)[1] <- "Flag"
names(df_grade)[2] <- "Mean_Return"


df_grade[1,1] <- "A"
df_grade[2,1] <- "B"
df_grade[3,1] <- "C"
df_grade[4,1] <- "D"
df_grade[5,1] <- "E"

#Calculating mean for Actual Return
df_grade$Mean_Return <-  c(mean(data_test$log_return_price[data_test$grade == "A"]),
                           mean(data_test$log_return_price[data_test$grade == "B"]),
                           mean(data_test$log_return_price[data_test$grade == "C"]),
                           mean(data_test$log_return_price[data_test$grade == "D"]),
                           mean(data_test$log_return_price[data_test$grade == "E"])
)

#Calculating mean for Expected Return
df_grade$Mean_Target <-  c(mean(data_test$target[data_test$grade == "A"]),
                           mean(data_test$target[data_test$grade == "B"]),
                           mean(data_test$target[data_test$grade == "C"]),
                           mean(data_test$target[data_test$grade == "D"]),
                           mean(data_test$target[data_test$grade == "E"])
)

#Keeping only the Grade, Mean_Return and Mean_Target
df_grade <- subset(df_grade, select = c(Flag, Mean_Return, Mean_Target))
print(df_grade)
