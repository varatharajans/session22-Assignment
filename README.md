                                         # session22-Assignment
                    DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 22 ASSIGNMENT 
                                  session_22_segment___clusterings.R
                                              Seshan
                                     Mon Aug 20 14:22:42 2018

2. Perform the below given activities: 
a. apply K-means clustering to identify similar recipes 
b. apply K-means clustering to identify similar attributes 
c. how many unique recipes that people order often 
d. what are their typical profiles

setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 22")
library(readr)
epi_r <- read.csv("epi_r.csv")
View(epi_r)
df<-epi_r
df[df==""] <- NA
df1<-na.exclude(df)
View(df1)
str(df1)
library(factoextra)
library("factoextra")
df <- df1[1:1000, 1:6]
na.exclude(df)
View(df)
head(df[, 1:6])
# Prepare Data
df <- na.omit(df) # listwise deletion of missing
#df <- scale(df) # standardize variables
View(df)
set.seed(1234)
ind = sample(1:nrow(df),0.8*nrow(df),replace = F)
df_train =df[ind,-1]
df_test = df[-ind,-1]
summary(df)
dim(df)
# outlier definition
# x > Q3+1.5*IQR - positive side outlier
# x < Q1-1.5*IQR - negative or lower side outlier
par(mfrow=c(2,3))
(boxplot(df1$rating)$out);(boxplot(df1$calories)$out);(boxplot(df1$protein)$out);(boxplot(df1$fat)$out);(boxplot(df1$sodium)$out)
apply(df,2,range)
apply(df,2,summary)
# KMeans - comes from Rcmdr library
# Kmeans- from amap library
# kmeans- from stats library
# steps in k-means clustering
#1- preprocessing the data (impute missing values, remove outliers, feature trasnformation)
#2- scaling or standardization of data set
#3- decide the number of clusters (value of K)
#4- iterate over the samples to create clusters
#5- decide the distance measure
#6- calculate the group accuracy
# scaling of data
df_train1 <- scale(df_train)
head(df_train1)
class(df_train1)
# screeplot approach to decide the number of clusters
km = kmeans(df_train1,1)
km$withinss
km$tot.withinss
km = kmeans(df_train1,2)
km$withinss
km$tot.withinss
km = kmeans(df_train1,3)
km$withinss
km$tot.withinss
km = kmeans(df_train1,4)
km$withinss
km$tot.withinss
km = kmeans(df_train1,5)
km$withinss
km$tot.withinss
km = kmeans(df_train1,6)
km$withinss
km$tot.withinss
km = kmeans(df_train1,7)
km$withinss
km$tot.withinss
km = kmeans(df_train1,8)
km$withinss
km$tot.withinss
km = kmeans(df_train1,9)
km$withinss
km$tot.withinss
km = kmeans(df_train1,10)
km$withinss
km$tot.withinss
dev.off()
sumsq=NULL
for (i in 1:25)
  sumsq[i] = sum(kmeans(df_train,centers=i, iter.max = 1000, nstart=i,                     algorithm='Forgy')$withinss)
plot(1:25,sumsq,type='b', main='Screeplot showing within group sum of squares')
km = kmeans(df_train1,3)
km$withinss
km$tot.withinss
class(km$cluster)
summary(km)
km$centers
as.numeric(km$cluster)
length(km$cluster)
dim(df_train)
class(df_train)
df_train$cl <- km$cluster
head(df_train)
# profiles of clusters
aggregate(df_train[,1:5],list(df_train[,6]),mean)
table(df1$rating)
table(df1$calories)
table(df1$X22.minute.meals)
table(df1$sodium)
library(cluster)
clusplot(df_train,df_train$cl,cex=0.9,color=T,shade=T, labels=4,lines=0)
#HC clustering or Hierarchical Clustering
# distance (euclidean, manhattan, cosine distance)
# Divisive method (top down)
# Agglomorative method (bottom up)
df_train = df_train[,-5]
head(df_train)
str(df_train)
# compute the distance metrix
d1 <- dist(df_train,method='euclidean')
summary(d1)
# HC
fit <- hclust(d1,method = 'ward.D2')
plot(fit)
# single, double, average, ward, ward.D2
# agglomorative method
fit <- agnes(d1,metric='euclidean',method = 'ward')
plot(fit)
# divisive method
fit <- diana(d1,metric='euclidean')
plot(fit)


                                       session22_assignment_modified.R
                                                     Seshan
                                         Fri Aug 24 05:49:15 2018
2. Perform the below given activities: 
c. how many unique recipies that people order often 
d. what are their typical profiles

Based on First model 

head(df_train)
##      rating calories protein fat sodium cl
## 146   3.750       67       2   4     66  1
## 785   5.000      228       3  14     67  1
## 769   4.375      441       7  22     29  1
## 1252  0.000      145       0   0     10  2
## 1074  3.125      757      54  36    936  1
# profiles of clusters
aggregate (df_train [, 1:5], list(df_train[,6]),mean)
  Group.1 rating    calories   protein    fat    sodium
1       1 4.1360029 371.4242 14.251082 20.58586  478.3218
2       2 0.1588983 300.1525 7.711864 16.27119   341.8305
3       3 4.1015625 1928.0833 98.333333 127.50000 6324.0208

                                             Based on 2nd moel
Discussion:-
Based on the assumption that o and 1 are indication of people order recipes, we have modified the data base , based on higher no of 1 and sorted then named it as epir_1 and the cluster analysis is performed.

Based on the analysis the aggregate group and cluster are given below.
head(df_train)
##      rating calories protein fat sodium cl
## 175   3.125      259       3  22    164  3
## 868   3.750      619       3   9    255  3
## 850   5.000      587       7  26    172  3
## 1369  3.750      203       6  11   1040  3
## 1185  4.375      408       9  20    461  3
## 889   4.375      188       2   1     10  3
# profiles of clusters
aggregate(df_train[,1:5],list(df_train[,6]),mean)
##   Group.1    rating  calories   protein       fat    sodium
## 1       1 0.8088235  214.7353  3.647059   8.50000  205.0588
## 2       2 3.4134615 1891.3462 81.346154 108.53846 2303.0769
## 3       3 4.1368626  315.8584  9.114731  16.76204  280.6119



setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 22/epicurious")
library(readr)
epi_r1 <- read.csv("epi_r1.csv")
View(epi_r1)
df<-epi_r1
df[df==""] <- NA
df1<-na.exclude(df)
View(df1)
