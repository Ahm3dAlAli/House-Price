
install.packages("data.table")
library(data.table)
library(readr)
library(plyr)
library(tidyverse)
library(dplyr)
install.packages("tidytext")
library(tidytext)
library(lubridate)
install.packages("ngram")
library(ngram)
library(e1071)
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)

test<- data.table(read_csv("House prices 2/project/volume/data/raw/Stat_380_test.csv"))
train<- data.table(read_csv("House prices 2/project/volume/data/raw/Stat_380_train.csv"))

################################################################################################################################
#Master cleaning #
##################

#Master
##############

test$train<-0
train$train<-1
test$SalePrice<-0
master_data<-rbind(test,train)



#Explore data 
###############

################
# Lot Frontage #
################

#form regression for lot frontage
lotfrontage_testtdata<-master_data[is.na(LotFrontage)]
lotfrontage_traintdata<-master_data[!is.na(LotFrontage)]
model_lot<-glm(LotFrontage~LotArea+BldgType+FullBath+HalfBath+TotRmsAbvGrd+YearBuilt+TotalBsmtSF+GrLivArea,data=lotfrontage_traintdata,family="gaussian")
pred<-predict(object=model_lot,newdata =lotfrontage_testtdata)
lotfrontage_testtdata$LotFrontage<-pred
master_data<-rbind(lotfrontage_traintdata,lotfrontage_testtdata)
master_data[order(Id)]
#Log helps normality
hist(master_data$LotFrontage)
master_data$LotFrontage<-log(master_data$LotFrontage+1)


############
# Lot Area #
############
#Fill Na's with zero 
master_data$LotArea<-nafill(master_data$LotArea,fill=0)
#Log helps normality
hist(master_data$LotArea)
master_data$LotArea<-log(master_data$LotArea+1)


############
# BldgType #
############
#No Na's, check unique
unique(master_data$BldgType)
#Turn into binary 
master_data$Fam<-ifelse(master_data$BldgType=="1Fam",yes=1,no=0)
master_data$Duplex<-ifelse(master_data$BldgType=="Duplex",yes=1,no=0)
master_data$fmCon<-ifelse(master_data$BldgType=="2fmCon",yes=1,no=0) 
master_data$TwnhsE<-ifelse(master_data$BldgType=="TwnhsE",yes=1,no=0)
master_data$Twnhs<-ifelse(master_data$BldgType=="Twnhs",yes=1,no=0)



###############
# OverallQual #
###############
#Check unique values
unique(master_data$OverallQual)
#Check skewness, no need to log transform 
hist(master_data$OverallQual)


###############
# OverallCond #
###############
#Check unique values
unique(master_data$OverallCond)
#Check skewness, no need to log transform 
hist(master_data$OverallCond)

###############
# FullBath#
###############
#Check unique values
unique(master_data$FullBath)
#Check skewness, no need to log transform 
hist(master_data$FullBath)


###############
# HalfBath #
###############
#Check unique values
unique(master_data$HalfBath)
#Check skewness, no need to log transform 
hist(master_data$HalfBath)

################
# TotRmsAbvGrd #
################
#Check unique values
unique(master_data$TotRmsAbvGrd)
#Check skewness,  need to undersqaure transform 
hist((master_data$TotRmsAbvGrd)^(1/2))
master_data$TotRmsAbvGrd<-((master_data$TotRmsAbvGrd)^(1/2))

#############
# YearBuilt #
#############
#Check unique values
hist(master_data$YearBuilt)
#each binary variable will either have 1 or 0 if the house built in that period
# master_data$period_built<-cut2(master_data$YearBuilt,m=2500,g=6)
# master_data$S1880_E1941<-ifelse(master_data$period_built=="[1880,1941)",yes=1,no=0)
# master_data$S1941_E1961<-ifelse(master_data$period_built=="[1941,1961)",yes=1,no=0)
# master_data$S1961_E1974<-ifelse(master_data$period_built=="[1961,1974)",yes=1,no=0)
# master_data$S1974_E1996<-ifelse(master_data$period_built=="[1974,1996)",yes=1,no=0)
# master_data$S1996_E2005<-ifelse(master_data$period_built=="[1996,2005)",yes=1,no=0)
# master_data$S2005_E2010<-ifelse(master_data$period_built=="[2005,2010]",yes=1,no=0)






###############
# TotalBsmtSf #
###############
#Check unique values
unique(master_data$TotalBsmtSF)
#Check skewness,  need to undersquare transform 
hist((master_data$TotalBsmtSF)^(1/2))
master_data$TotalBsmtSF<-((master_data$TotalBsmtSF)^(1/2))


################
# BedroomAbvGr #
################
#Check unique values
unique(master_data$BedroomAbvGr)
#Check skewness, no need to log transform 
hist((master_data$BedroomAbvGr)^(1/2))
master_data$BedroomAbvGr<-((master_data$BedroomAbvGr)^(1/2))

###########
# Heating #
###########
#Check unique values
unique(master_data$Heating)
#Turn Into Binary
master_data$GasA<-ifelse(master_data$Heating=="GasA",yes=1,no=0)
master_data$Grav<-ifelse(master_data$Heating=="Grav",yes=1,no=0)
master_data$GasW<-ifelse(master_data$Heating=="GasW",yes=1,no=0) 
master_data$Wall<-ifelse(master_data$Heating=="Wall",yes=1,no=0)



##############
# CentralAir #
##############
#Check unique values
unique(master_data$CentralAir)
#Turn Into Binary
master_data$CentralAir<-ifelse(master_data$CentralAir=="Y",yes=1,no=0)


#############
# GrLivArea #
#############
#Check unique values
unique(master_data$GrLivArea)
#Check skewness, need to log transform 
hist(master_data$GrLivArea)
master_data$GrLivArea<-log(master_data$GrLivArea+1)



############
# PoolArea #
############
#Check unique values
unique(master_data$PoolArea)
#Check skewness,  nothing change turn to binary
hist(master_data$PoolArea)
#Pool Area occurrence 
master_data$HvPool<-ifelse(master_data$PoolArea>0,yes=1,no=0)


##########
# YrSold #
##########
#Check unique values
unique(master_data$YrSold)
#Check skewness, no need to log transform 
hist(master_data$YrSold)

#############
# SalePrice #
#############
#Check skewness,  need to log transform 
hist(master_data$SalePrice)
master_data$SalePrice<-log(master_data$SalePrice+1)


################################################################################################################################
#New Features#
##############

# Has been the house sold the year it was built
master_data$IsNewHouse <-ifelse(master_data$YearBuilt == master_data$YrSold, yes=1, no=0) 

# How old it is
master_data$Age <- as.numeric(master_data$YrSold - master_data$YearBuilt)

#Number of bathrooms 
master_data$Bathrooms<-as.numeric((master_data$FullBath)+(0.5*master_data$HalfBath))

#Other rooms to avoid confusion of tot rooms because bedrooms included we want to represent unique columns
master_data$OtherRooms<-master_data$TotRmsAbvGrd-master_data$BedroomAbvGr

################################################################################################################################
#Un needed variables #
######################

master_data$FullBath=NULL
master_data$HalfBath=NULL
master_data$TotRmsAbvGrd=NULL
master_data$PoolArea=NULL
master_data$BldgType=NULL
master_data$YrSold=NULL
master_data$Heating=NULL
master_data$period_built=NULL

str(master_data)

##############################################################################################################################
# Processed Files #
###################
#seperate to train and test 
CleanedTrain<-master_data[train==1]
CleanedTest<-master_data[train==0]

CleanedTrain$train=NULL
CleanedTest$train=NULL

write_csv(CleanedTest,"House prices 2/project/volume/data/processed/CleanedTest.csv")
write_csv(CleanedTrain,"House prices 2/project/volume/data/processed/CleanedTrain.csv")



