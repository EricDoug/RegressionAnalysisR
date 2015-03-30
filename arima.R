# =====================================================================
# CSE487/587
# Author: Soumasish Goswami
# Email: soumasis@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data


library(forecast)
library(fpp)
library(ggplot2)

# need to read the stocklist, and loop all files
### TO DO

# just read one file
#filename = "/gpfs/courses/cse587/spring2015/data/hw2/data/AAPL.csvD
#filename = "~/Desktop/r_source/AAPL.csv"  ....
files <- list.files(path="/gpfs/courses/cse587/spring2015/data/hw2/data", pattern="*.csv", full.names=TRUE, recursive=FALSE)
#files <- list.files(path="~/Desktop/small", pattern="*.csv", full.names=TRUE, recursive=FALSE)
# if file is not empty

A = matrix(NA,length(files),2)

for(k in 1:length(files)){
  filename = files[k]
 
  if(file.info(filename)[1]>=0) {
    name = basename(files[k])
    modname = sub('\\.csv', '', name)
    # read one csv file into variable (DO NOT EDIT)
    result = tryCatch({
      textData=read.csv(file=filename, header=T)
    }, warning = function(w) {
      
    }, error = function(e) {
      
    })
    #textData=read.csv(file=filename, header=T)
    if(nrow(textData)>= 754){
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE = matrix(NA,1,length(testData))
      
      # apply ARIMA model (DO NOT EDIT)
      fitData = auto.arima(trainData, seasonal= F, lambda= NULL, approximation= T)
     
      ### TO DO
      
      #Linear regression :
      fitData=tslm(trainData~trend+season)
      #fit=lm(trainData~time(trainData),data=trainData)
      #plot(trainData,time(trainData)
      #abline(fit,col="red")
      #plot(fit)
      
      #Holt Winters Model:
      #fitData = (HoltWinters(trainData,beta = FALSE,gamma = FALSE))
      
      # apply forecast(DO NOT EDIT)
      #forecastData = forecast(fitData, h=length(testData))
      
      # print variable and see what is in the result data set
      #print(forecastData)
      
      # calculate Mean Absolute Error
      for(i in 1:length(testData))
      {
        MAE[1,i] = abs(forecastData$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      
      
      # print(sum(MAE[1,1:10]))
      A[k,1] = sum(MAE[1,1:10])
      # plot the top 10 minimum sum of MAE in 3 models respectively
      #plot(MAE[1,1:10], col = "blue")
      #lines(MAE[1,1:10], lw = 2, col = "red")
      ### TO DO
      #FM =
      A[k,2]=modname
    }#end of if, rows are more than 754
    
    
  }#end of if, testing file contains data
  
  
}#end of looping through directory
df <- as.data.frame(A)
colnames(df)<- c("Sum of MAE", "Company")
df <- na.omit(df)
df2 <- df[order(df[,1]),]
#df$V1 <- as.numeric(df$V1)
#df <- df[with(df, order(V1)),]

df2 <- head(df2, n=10)
#ggplot(df2, aes_string("Sum", "Company", group=1) + geom_line("red"))
#lines(df2)
#print(pl)
print(df2) #df2 is sorted now
df3 <- data.frame(df2)
names(df3)
# [1] "Sum.of.MAE" "Company" 
plo <- ggplot(df3, aes(Company, Sum.of.MAE, group=1)) + 
  geom_line() + 
  xlab("Linear Regression Model")
print(plo)