#Data Manipulation in R - dplyr

# Dplyr - is used to transform and summarize  tabular data with rows and columns 
 getwd()
 setwd("C:/Users/HP/Documents")
 getwd()
install.packages("dplyr")
library(dplyr)
install.packages('nycflights13')
library('nycflights13')
View(flights)
head(flights)
View(head(flights))
View(tail(flights))

#Subset data set using filter()

f1 <- filter(flights,month == 07)
View(f1)

f2 <- filter(flights,month == 07,day == 3)
View(f2)

View(filter(flights,month == 09,day == 2, origin == 'LGA'))

# 0R 

head(flights[flights$month == 09 & flights$day == 2 & flights$origin == 'LGA',])
View(head(flights[flights$month == 09 & flights$day == 2 & flights$origin == 'LGA',]))

# slice() is used to add new column 
?slice
slice_head(flights)
View(slice(flights,1:5))
View(slice(flights,5:10))

# Mutate() is used to add new column 
?mutate
over_delay <- mutate(flights,overall_delay = arr_delay-dep_delay)
View(over_delay)
head(over_delay)
View(head(over_delay))

# Transmute() function is used to show only new column 

over_delay<-transmute(flights,overall_delay = arr_delay - dep_delay)
View(over_delay)

# summarize() used to find descriptive statistics 
summarise(flights,avg_air_time=mean(air_time,na.rm=T))
summarise(flights,tot_air_time=sum(air_time,na.rm=T))
summarise(flights,stdev_air_time=sd(air_time,na.rm=T))
summarise(flights,avg_air_time=mean(air_time,na.rm=T),tot_air_time=sum(air_time,na.rm=T))


# Using Titanic dataset 
data()
View(Titanic)
View(iris)
slice(iris,1:5)
slice(iris,5:10)

library(readxl)
Data_for_Practice <- read_excel("Data for Practice.xlsx")
View(Data_for_Practice)

f4 <-filter(Data_for_Practice, username == 'atsb_mon01')
f4
View(f4)

data()

View(ChickWeight)

f4<-filter(ChickWeight,weight == 106)
View(f4)

slice(ChickWeight,1:5)
slice(ChickWeight,5:10)


# select function 

library(dplyr)
P1 <- select(ChickWeight, weight,Time)
P1
View(P1)

# use of piping operator 

df <- mtcars
df 
View(df)

# nesting 
result <- arrange(sample_n(filter(df,mpg>20),size = 5),desc(mpg))
View(result)
 
# multiple assignment 

a<-filter(df,mpg>20)
b<-sample_n(a,size = 5)
result<- arrange(b,desc(mpg))
result
View(result)

# Same Using pipe operator

# Syntax- data %>% op1 %>% op2 %>% op3

result<- df %>% filter(mpg>20) %>% sample_n(size=10) %>% arrange(desc(mpg))
result
View(result)

df
#select()
df_mpg_hp_cy1 =View( df %>% select(mpg,hp,cyl))

## 


# DATA MANIPULATION USING Tidyr

install.packages(tidyr)
library('tidyr')

n=10

wide <-data.frame(
  ID = c(1:n),
  Face.1 = c(411,723,325,456,576,612,709,513,527,379),
  Face.2 =c(123,300,400,500,600,654,789,906,413,567),
  Face.3 =c(1457,1000,569,896,956,2345,780,599,1023,678)
)

View(wide)

# Gather() -Reshaping data from wide format to long format

long <- wide %>% gather(Face,ResponseTime,Face.1:Face.3)
View(long)

# Separate()- splits a single column into multiple columns 

long_separate <- long %>% separate(Face,c("Target","Number"))
View(long_separate)

# unite()-combines multiple columns into a single column

long_unite<-long_separate %>% unite(Face,Target,Number,sep = ".")
View(long_unite)


# spread()- takes two columns (key & value) and spreads in to multiple columns
# it makes "long" data wider

back_to_wide <- long_unite %>% spread(Face,ResponseTime)
View(back_to_wide)


## DATA VISUALIZATION IN R 
#EDA
View(ChickWeight)
plot(ChickWeight)

#Base graphics 
library(MASS)
plot(UScereal$sugars,UScereal$calories)
title("plot(UScereal$sugars,UScereal$calories)")

x<-UScereal$sugars
y<-UScereal$calories
library(grid)

# grind graphics

pushViewport(plotViewport())
pushViemport(dataViewport(x,y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x,y)
grid.text("UScereal$calories",x=unit(-3,"lines"),rot =90)
grid.text("UScereal$sugars",y=unit (-3,"lines"),rot =0)
popviewpoint(2)

# Pie chart for different products and units sold 

# create data for the graph 

x<- c(33,45,70,110)
labels <- c("Soap","Detergent","Oil","Shampoo")

# plot the chart 

pie(x,labels)
pie(x,labels,main = "City pie chart",col = rainbow(length(x)))
legend("topright",c("Soap","Shampoo","Oil","Grocery"),cex=0.8,
       fill = rainbow(length(x)))


piechart <- round(100*x/sum(x),1)


# 3D pie chart
install.packages("plotrix")
library(plotrix)

x <- c(33,45,70,110)
lbl <- c("Soap","Detergent","Oil","Shampoo")

# plot the chart 

pie3D(x,labels = lbl,explode = 0.1,main = "Pie Chart of Countries")


# Creating data for the graph

v <- c(9,13,21,8,36,22,12,41,31,33,19)

# creating the histogram 

hist(v,xlab = "Weight",col = "green",border = "red")

hist(v,xlab = "Weight",col = "green",border = "red",xlim = c(0,40),ylim = c(0,5),
     breaks = 5)

data("airquality")
View(airquality)

# use the plot function to draw scatter plots

# plot a graph between the ozone and wind values 

plot(airquality$Ozone,airquality$Wind)
plot(airquality$Ozone,airquality$Wind,col = 'red')
plot(airquality$Ozone,airquality$Wind,type = 'h',col = 'blue') # histogram

plot(airquality)


# Assign labels to the plot

plot(airquality$Ozone,xlab = "ozone concentration",ylab = 'No of Instances',main = 'ozone levels in NY city',col = 'green')


# Histogram 
hist(airquality$Solar.R)
hist(airquality$Solar.R,main = "Solar Radiation values in air",xlab = 'Solar radiation',ylab = 'Airquality')

Temperature <- airquality$Temp
hist(Temperature)

# Histogram with labels
h <- hist(Temperature,ylim = c(0,40))
text(h$mids,h$counts,labels = h$counts,adj = c(0.5,-0.5))


# Histogram with non-uniform width
hist(Temperature,
     main = 'Maximum daily temperature at La Guardia Airport',
     xlab = 'Temperature in degrees Fahrenheit',
     xlim = c(50,100),
     col = "chocolate",
     border = 'brown',
     breaks =  c(55,60,70,75,80,100)
     )

# Box plot 
boxplot(airquality$Solar.R)

# Multiple box plots 
boxplot(airquality[,0:4],main = 'Multiple Box plots')

# using ggplot2 library to analyze mtcars data set

install.packages("ggplot2")
library(ggplot2)
attach(mtcars)
View(mtcars)

pl <- ggplot(mtcars,aes(factor(cyl),mpg))
pl + geom_boxplot()

pl + geom_boxplot() + coord_flip()

pl + geom_boxplot(aes(fill = factor(cyl)))


# create factors with value labels 

mtcars$gear <- factor(mtcars$gear,levels = c(3,4,5),
                      labels = c('3gears','4gears','5gears'))
mtcars$sam <- factor(mtcars$am,levels = c(0,1),
                     labels = c('Automatic','Manual'))
mtcars$cyl <- factor(mtcars$cyl,levels = c(4,6,8),
                     labels = c('4cyl','6cyl','8cyl'))

# Scatter plot 

ggplot(data = mtcars,mapping = aes(x = wt),y = mpg) + geom_point()
  
 # scatter plot by factors

  
# visualization using mpg data set 
  
ggplot2::mpg

View(ggplot2::mpg)


# Bar plot 

ggplot(data = ggplot2::mpg ,aes(class)) + geom_bar()

# stacked bar chart 

ggplot(data = ggplot2::mpg,aes(class)) + geom_bar(aes(fill = drv))

# using dodge 
ggplot(data = ggplot2::mpg ,aes(class)) + geom_bar(aes(fill = drv),position = 'dodge')

ggplot(data = ggplot2::mpg ) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = ggplot2::mpg)+
  geom_point(mapping = aes(x = displ,y = hwy ,color = class))

# using plotly library 

install.packages('plotly')
library(plotly)

p <- plot_ly(data = mtcars,x = ~hp ,y = ~wt , marker = list(size = 10))
p

p <- plot_ly(data = mtcars, x = ~hp , y = ~wt, color =  ~hp ,size = ~hp)
p

############################

# Linear Regression 
 
library(readr)
sales <- read_csv("sales.csv")
View(sales)

# splitting the data into training and test data 
set.seed(2)
library(caTools)
split<- sample.split(sales,SplitRatio = 0.7)
split
train <- subset(sales,split = 'TRUE')
test <- subset(sales,split = 'FALSE')
train
test

# creating the model

Model <- lm(Revenue~.,data = train)
summary(Model)

# prediction 
pred <- predict(Model,test)
pred

# comparing predicted vs actual values 

plot(test$Revenue,type = 'l',lty = 1.8,col = 'red')
lines(pred,type = 'l',col = 'blue')
plot(pred,type = 'l',lty = 1.8,col = 'blue')

# Finding Accuracy

rmse <- sqrt(mean(pred-sales$Revenue)^2)
rmse


#using cars data  
  
head(cars)
View(cars)
str(cars)
plot(cars)
plot(cars$speed,cars$dist)
cor(cars)
cor(cars$speed,cars$dist)

linearMod <- lm(speed~dist,data = cars)
summary(linearMod)


## Training 

set.seed(100)
TrainingRowIndex <- sample(1:nrow(cars),0.8*nrow(cars))
TrainingData <- cars[TrainingRowIndex,]
TestData <- cars[-TrainingRowIndex,]

lmMod <- lm(dist~speed,data = TrainingData)
distpred <- predict(lmMod,TestData)
summary(lmMod)

actuals_preds <- data.frame(cbind(actuals = TestData$dist,predicteds = distpred))

correlation_accuracy <- cor(actuals_preds)

head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

install.packages('DMwR2')
library(DMwR2)

DMwR2::regr.eval(actuals_preds$actuals,actuals_preds$predicteds)


############################

# Logistic Regression 
# Define the problem
# Load the libraries
#Acquire the data 
#Ingest the data
#  set the working directory 
#Explore the data
#Munge the data (if necessary)
#prepare the data
#   scale the data (if necessary)
#   split the data into train and test data sets 
#Train the model using the training data
#Run the test data through the model
#Validate the model - accuracy,precision,etc


# Load libraries

library(caTools)


# ingest the data
getwd()  # working directory 

library(readr)
binary <- read_csv("binary.csv")
View(binary)

# split the data set 
split <- sample.split(binary,SplitRatio = 0.8)
split

train <- subset(binary,split = 'TRUE')
test <- subset(binary,split = 'FALSE')

# Munge the data

# These are really categorical variables so lets tell R to convert them to factors

binary$admit <- as.factor(binary$admit)
binary$rank <- as.factor(binary$rank)

# Train the model using the training data 
# Use glm,the general linear model function
# Dependent variable is admit,indpt variable are gpa and rank  
#The family argument should be binomial to indicate logistic regression 


mymodel <- glm(admit~gpa+rank,data = train,family = 'binomial')
summary(mymodel)

# Run the test data through the model

res <- predict(mymodel,test,type = 'response')
res

res <- predict(mymodel,train,type = 'response')
res


# Validate the model - confusion matrix 

confmatrix <- table(Actual_value = train$admit,predicted_value = res>10)
confmatrix


# Accuracy
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)


######################################

## DECISION TREES IN R 

attach(mtcars)
set.seed(2)

library(caTools)
library(ploty)
library(tidyverse)

#  loading packages


install.packages("FSelector")
install.packages('rpart')
install.packages(caret,dependencies = TRUE)
install.packages('dplyr')
install.packages('rpart.plot')
install.packages('xlsx')
install.packages('data.tree')
install.packages('caTools')
install.packages('e1071')

# Libraries

library(FSelector)
library(rpart)
library(rpart.plot)
library(xlsx)
library(openxlsx)
library(caTools)
library(data.tree)
library(caret)

# Loading data set 

library(caTools)
library(readr)
tested <- read_csv("tested.csv")
View(tested)

# Selecting only the meaningful columns for prediction 

df <- select(tested,Survived,Pclass,Sex,Age)
df
View(df)
df <- mutate(df,Survived = factor(Survived),Pclass = as.factor(Pclass),Age = as.numeric(Age))

# Splitting the data set into training and testing
set.seed(123)

sample <- sample.split(df$Survived, SplitRatio = 0.70)
train <- subset(df,sample == TRUE)
test <- subset(df,sample == FALSE)

 
# Training the Decision Tree Classifier

library(rpart)
tree <- rpart(Survived~.,data = train)

# Predictions 

tree.survived.predicted <- predict(tree,test,type = 'Pclass')

# confusion matrix for evaluating the model

confusionMatrix(tree.survived.predicted,test$Survived)


# Visualizing the decision tree 

prp(tree)


# IRIS Data

library(rpart)
library(rpart.plot)
data('iris')
head(iris)
str(iris)
set.seed(9850)

g<- runif(nrow(iris))
iris_ran <- iris[order(g),]

# Model building 

model <- rpart(Species~.,data = iris_ran[1:100,],method = 'class')
model

rpart.plot(model,type = 4,fallen.leaves = T,extra = 104)

# Model validation 

model_pred <- predict(model,iris_ran[101:150,], type = 'class')
model_pred


# Installing packages required to use confusion matrix

install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

# Confusionmatrix

confusionMatrix(iris_ran[101:150,5],reference = model_pred)



#############################

# RANDOM FOREST IN R 

#loading libraries 

install.packages('randomForest')
library(randomForest)

# Loading data sets 

library(readr)
data <- read_csv("WineQT.csv")
View(data)

data$quality <- as.factor(data$quality)

data_set_size = floor(nrow(data)*0.80)
index <- sample(1:nrow(data), size = data_set_size)
training <- data [index,]
testing <- data [-index,]

rf <- randomForest( quality ~., data = training , mtry = 4, ntree = 2001,importance = TRUE)
rf

plot(rf)

result <- data.frame(testing$quality,predict(rf,testing[,1:11],type = 'response'))
result

plot(result)


#################################


# SUPPORT VECTOR MACHINE (SVM)

library(e1071)

# create data frame for weight and height

mydata <- data.frame(Height = c(44.0,52.1,57.1,33.0,27.8,27.2,32.0,45.1,56.7,56.9,122.1,123.9,122.9,101.1,128.9,137.1,127.0,103.0,141.6,102.4),
         Weight = c(126.3,136.9,109.2,148.3,110.4,107.8,128.4,120.2,140.2,139.2,154.1,170.8,183.1,164.0,193.6,181.7,164.8,174.6,185.8,176.9)
                     )
View(mydata)


# create an ishorse indicator variable

ishorse <- c(rep(-1,10),rep(+1,10))
library(e1071)

# create data frame for performing svm

my.data <- data.frame(Height = mydata['Height'],
                      Weight = mydata['Weight'],
                      animal=as.factor(ishorse))

View(my.data)

# Plot the data 

plot(my.data[,-3],col = (3)/2,pch = 19); abline(h=0,v=0,lty = 3)

# perform svm by calling the svm method and passing the parameters 

svm.model <- svm(animal ~.,
                  data = my.data,
                  type = 'C-classification',
                  kernel = 'linear',
                  scale = FALSE)

# display summary of svm values 
summary(svm.model)

# show the support vectors 

points(my.data[svm.model$index,c(1,2)],col = 'orange',cex = 2)

# Get parameters of the hyper plane

w <- t(svm.model$coefs) %*% svm.model$SV
b <- svm.model$rho

# in this 2D case the hyper line is the line w[1,1]*x1 + w[1,2]*2 +b =0

abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col = 'blue',lty = 3)

# new data - mule,horse,mule

observations <- data.frame(Height =c(67,121,100),Weight = c(121,190,100))

# plot the new data

plot(my.data[,-3],col = (ishorse+3)/2,pch = 19,xlim = c(0,250),ylim = c(0,250))
abline(h=0,v=0,lty = 3)
points(observations[1,],col = 'green',pch = 19)
points(observations[2,],col = 'blue',pch = 19)
points(observations[3,],col= 'dark orange',pch = 19)
abline(a = -b/w[1,2], b = -w[1,1]/w[1,2],col = 'blue',lty = 3)

# verify the results 

predict(svm.model,observations)

##################################

# CLUSTERING in R 

# loading data sets 

library(readxl)
US_sales <- read_excel("US sales.xlsx")
View(US_sales)

# cluster analysis 

str(US_sales)
head(US_sales)

# scatter plot 

plot(US_sales)

plot(Fuel_cost~Sales,US_sales)

with(US_sales,text(Fuel_cost~Sales,labels = City,pos = 4 ,cex = .3))

plot(RoR~Sales,US_sales)
with(US_sales,text(RoR~Sales,labels = City,pos = 4,cex = .3))

# Normalization 

z <- US_sales[,-c(1,1)]
m <- apply(z,2,mean)
s<-  apply(z,2,sd)
z <- scale(z,m,s)

# Calculate the Euclidean distance 

distance <- dist(z)
distance
print(distance,digits = 3)
print(distance ,digits = 2)


# clustering dindrogram
hc.1 <- hclust(distance)
plot(hc.1)

plot(hc.1,labels = US_sales$City,hang = -1)

# clustering Dendogram(Average)

hc.1 <- hclust(distance,method = 'average')
plot(hc.1,labels = US_sales$City,hang = -1)

# clustering Membership 

member.1 <- cutree(hc.1,3)
aggregate(z,list(member.1),mean)

# Actual values 

aggregate(US_sales[,-c(1,1)],list(member.1),mean)


# TIME SERIES IN R 

# Project 

#create a data frame from our dataset






