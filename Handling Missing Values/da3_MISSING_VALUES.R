
  #Name : Adrija Mukhopadhyay
  #Reg No: 19BDS0159
  #DATA SET: BENGALURU HOUSE DATA SET 
  #url: 'https://github.com/Addy0908/DATASET/blob/main/Bengaluru_House_Data.csv'



library(utils)
library(base)
library(VIM)   #for using kNN algorithm

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
## Replace the rest NA values by col means
data2 <- data                                              
for(i in 1:ncol(data)) {                                  
  data2[ , i][is.na(data2[ , i])] <- mean(data2[ , i], na.rm = TRUE)
}
data2

summary(data2)
#all the numerical na values are replaced by col means.

#Apply any other standard algorithm for imputing missing values (for at least one column).

#Missing Value Imputation using KNN algorithm

#When we filled the missing values above by col means we saw that there where still few char NA values left in the dataset which didnt got replaced.
#The best way to replace them is by using kNN algorithm which comes under package VIM.
##KNN is an algorithm that is useful for matching a point with its closest k neighbors.It can be used for data that are continuous, discrete, ordinal and categorical which makes it particularly useful for dealing with all kind of missing data.
str(data)
head(data)
summary(data)


# we saw that only 2 col have missing values namely bath and balcony so we remove only those cols Na values
data1 <-kNN(data, variable = c("bath","balcony"),k=5)

summary(data1)
sum(is.na(data1))
#we now see that all the Na values from the above two cols are removed , yet there are still 14 Na values left so to remove all the Na values we took all the variables in the dataset.

data3 <- kNN(data)  #k=5 by default if we don't mention it 
summary(data3)
sum(is.na(data3))
#finally all the Na values are removed 
head(data3)   # got some extra variables that KNN used for filling those missing values
# now  we shall remove the extra variables

data3 <- subset(data3,select =area_type:price)
head(data3)
#finally our dataset is free from missing values aswell as extra variables 

#How KNN works
#step-1: Load the data set
#Step-2: Select the number of neighbors you want = k
#Step-3: In the presence of missing coordinates, find the Euclidean distance .  
#Step-4: Sort the  distances in ascending order.
#Step-5: Take the K nearest neighbors as per the sorted collection.
#Step-6: Among these k neighbors, count the number of the data points in each category.
#Step-7: Assign the missing values with the maximum number of data points in that category.
#Step-8: Finally we have filled all the missing values .


#Simple Linear Regression
head(data)
#Step-1: Find the most correlated variable
cor(data, "total_sqft","bath", use="complete.obs")

# we cant use linear regression method for our dataset because our dataset has numeric aswell as character value.

