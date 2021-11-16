
  #Name : Adrija Mukhopadhyay
  #Reg No: 19BDS0159
  #DATA SET: BENGALURU HOUSE DATA SET 
  #url: 'https://github.com/Addy0908/DATASET/blob/main/Bengaluru_House_Data.csv'



library(utils)
library(base)
library(VIM)   #for using kNN algorithm
library(dplyr)
#read the dataset and replace the missing values by NA
df <- read.csv("D:/jupiter/dataset/Bengaluru_House_Data.csv",header=T, na.strings=c(""," ","NA"))

#display the top datas of the dataset
head(df)

#find the total number of missing values
sum(is.na(df))

## Remove columns and rows with more than 60% NA
data <- df[which(rowMeans(!is.na(df)) > 0.6), which(colMeans(!is.na(df)) > 0.6)]
data 
sum(is.na(data))
head(data)

##KNN is an algorithm that is useful for matching a point with its closest k neighbors.It can be used for data that are continuous, discrete, ordinal and categorical which makes it particularly useful for dealing with all kind of missing data.
str(data)
head(data)
summary(data)



data3 <- kNN(data)  #k=5 by default if we don't mention it 
summary(data3)
sum(is.na(data3))
#finally all the Na values are removed 
head(data3)   # got some extra variables that KNN used for filling those missing values
# now  we shall remove the extra variables

data3 <- subset(data3,select =area_type:price)
head(data3)
#finally our dataset is free from missing values aswell as extra variables 

#select one numerical col to perform the operations of LAB ACTIVITY-4
df1<-select(data3,price)

df1

#1)calculate the mean of the column
Mean1 = function(X)   #user define funciton
{M = sum(X)/length(X); 
M} 
Mean1(data3$price) 

#predefine funtion
mean(data3$price)

#we got the same values

#2)calculate the meadian of the column
Median1 <- function(x){
  # Order Vector ascending
  x <- sort(x)
  # For even length average the value to the  surrounding numbers
  if((length(x) %% 2) == 0){
    return((x[length(x)/2] + x[length(x)/2 + 1]) / 2)
  }
  # For odd length take the value that is in the center
  else{
    return(x[(length(x)/2) + 0.5])
  }
}
Median1(data3$price)
#predefine funtion
median(data3$price)
#we got the same values


#3)calculate the mode of the column
Mode1 <- function(x) {
    X <- unique(x)
    X[which.max(tabulate(match(x, X)))]
}
Mode1(data3$price)


#4)calculate the IQR of the col

iqr1 <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq
  print(iqr)
}
iqr1(data3$price)
#predefine function
IQR(data3$price)
#we got the same values

#5)calculate the standard deviation
SD1 <- function(x) {
sd1<-sqrt(sum(((x)-Mean1(x))^2/(length(x)-1)))
print(sd1)
}
SD1(data3$price)
#predefine function
sd(data3$price)
#we got the same values

#6)Find Probability values on Empirical  Rule 
emp1 <- function(x){
  a<-Mean1(x)
  b<-SD1(x)
  c<- c(((a)-(1*b)),
        ((a)+(1*b)),((a)-(2*b)),
        ((a)+(2*b)),((a)-(3*b)),
        ((a)+(3*b)))
  (c)
}
emp1(data3$price)

#7)Plot the Graph/Histogram/Normal Distribution  and Compare your functions return value with predefined functions in R for mean, median, iqr, and sd
hist(data3$price)
table(data3$price)
#Normal Distribution Curve
m<-Mean1(data3$price) ;
std<-SD1



#8)Formulate the Null Hypothesis and Alternative Hypothesis for your data set and prove it based on the p-value.
#One Sample t-test
t.test(data3$price, mu=5)

str(data3)
x <- (data3$price)
y <- (data3$balcony)
result <-t.test(x,y, var.equal = TRUE)
result
#Ploting t-test
library(webr)
plot(result)
