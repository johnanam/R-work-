# Setting working directory 
getwd()
setwd("C:/Users/HP/OneDrive/Desktop/R")

# Installing packages in  R
#(libraries - directory where packages are stored )

# Used for importing the foreign data sets 
install.packages('foreign')
install.packages('haven')
install.packages('rio')
install.package("Crayon")
install.packages("readxl")

# libraries 

library(foreign)
library(haven)
library(rio)
library(crayon)

# Importing DTA data sets 

library(haven)
stidata_unclean <- read_dta("stidata_unclean.dta")
View(stidata_unclean)

attach(stidata_unclean)
class(a1age)
str(a1age)
names(stidata_unclean)

# Descriptive statistics 
#summary

summary(stidata_unclean)
summary(idnumber)
summary(sex)

# Data Management 
# Explore categorical data 

table(a1age)
table(casestatus)
table(sex)

# Using table1 package 

install.packages("table1")
library(table1)

table1(~factor(a1age))
table1(~factor(sex))
table1(~factor(casestatus))
table1(~factor(sex))
# Data Manipulation 
library(dplyr)

# checking inconsistencies 
View(stidata_unclean[stidata_unclean$casestatus == 3,])

# correcting inconsistency 
# only if you didn't attach data set 
stidata_unclean$casestatus[stidata_unclean$idnumber == 31] = 1
stidata_unclean$casestatus[stidata_unclean$idnumber == 1] = 1
View(stidata_unclean$casestatus)

# For attached data set 

stidata_unclean$casestatus[idnumber == 31] = 1
stidata_unclean$casestatus[idnumber == 1] = 1
f1 <- filter(stidata_unclean,idnumber == 1)
View(f1)
table(stidata_unclean$casestatus)
table(casestatus)
####
casestatus[idnumber == 31] = 1
casestatus[idnumber ==  1] = 1
table(casestatus)

# piping operator 
casestatus[idnumber == 31 | idnumber == 1 ] = 1

# Creating new a variable 
# Generate a numeric variable called sexN Where 1 = male and 2 = Female 

stidata_unclean$sexN <- NA
View(stidata_unclean)

# Assigning variables 

stidata_unclean$sexN[sex == 'Male'] = 1
stidata_unclean$sexN[sex == 'Female'] = 2
View(View(idnumber[is.na(stidata_unclean$sexN)],))

# checking for missing values

View(sexN[is.na(stidata_unclean$sexN)],)
View(idnumber[is.na(stidata_unclean$sexN)],)
View(idnumber[is.na(sexN)],)

# Writing a command to correct the 2 missing data for sexN using idnumber 

stidata_unclean$sexN[stidata_unclean$idnumber == 48] = 'Male'
stidata_unclean$sexN[stidata_unclean$idnumber == idnumber] = 'Female'
table(sexN)

# Handling duplicates 
# true and false duplicates 

# 1 .Report of duplicates 

dups <- duplicated(stidata_unclean$idnumber)
dups
table(dups)

# list the id number of duplicated records 

stidata_unclean$idnumber[duplicated(stidata_unclean$idnumber)]

# view duplicated record in the data set 

View(stidata_unclean[stidata_unclean$idnumber == 51,])

# Removing the duplicates 

stidata_unclean2 <- stidata_unclean %>% slice(-c(227))
