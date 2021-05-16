install.packages("data.table")
library(data.table)
library(readr)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)
install.packages("xgboost")
library(xgboost)
############################################################################################################################

##########################
# Prep Data for Modeling #
##########################

################################################################################################################

#advanced methods of hyperparameter tuning discussed here:
#https://rpubs.com/jeandsantos88/search_methods_for_hyperparameter_tuning_in_r


#read in data, notice the path will always look like this because the assumed working directory is the repo level folder
train<- data.table(read_csv("House prices 2/project/volume/data/processed/CleanedTrain.csv"))
test<-data.table(read_csv("House prices 2/project/volume/data/processed/CleanedTest.csv"))




##########################
# Prep Data for Modeling #
##########################
y.train<-train$SalePrice
y.test<-test$SalePrice

# work with dummies

dummies <- dummyVars(SalePrice~ ., data = train)
x.train<-predict(dummies, newdata = train)
x.test<-predict(dummies, newdata = test)



# notice that I've removed label=departure delay in the dtest line, I have departure delay available to me with the in my dataset but
# you dont have price for the house prices.
dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


hyper_perm_tune<-NULL
########################
# Use cross validation #
########################

param <- list(  objective           = "reg:linear",
                gamma               = 0.03,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.013,
                max_depth           = 5,
                min_child_weight    = 11,
                subsample           = 0.58,
                colsample_bytree    = 0.9,
                tree_method = 'hist'
)


XGBm<-xgb.cv( params=param,nfold=5,nrounds=10000,missing=NA,data=dtrain,print_every_n=1,early_stopping_rounds=30)

best_ntrees<-unclass(XGBm)$best_iteration

new_row<-data.table(t(param))

new_row$best_ntrees<-best_ntrees

test_error<-unclass(XGBm)$evaluation_log[best_ntrees,]$test_rmse_mean
new_row$test_error<-test_error
hyper_perm_tune<-rbind(new_row,hyper_perm_tune)

################################
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)
xgb.grid <- expand.grid(nrounds = 10000,
                        max_depth = seq(5,10),
                        eta = c(0.001,0.01,0.02,0.1),
                        gamma = c(0.001,0.01,0.03,0.1),
                        colsample_bytree = c(0.5,0.8, 0.9,1),
                        subsample=c(0.5,0.8,0.9,1),
                        min_child_weight=seq(1,10)
)

xgb_tune <-train(SalePrice ~.,
                 data=data.frame(train),
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid
)

print(xgb.grid)

####################################
# fit the model to all of the data #
####################################


# the watchlist will let you see the evaluation metric of the model for the current number of trees.
# in the case of the house price project you do not have the true houseprice on hand so you do not add it to the watchlist, just the dtrain
watchlist <- list( train = dtrain)

# now fit the full model
# I have removed the "early_stop_rounds" argument, you can use it to have the model stop training on its own, but
# you need an evaluation set for that, you do not have that available to you for the house data. You also should have 
# figured out the number of trees (nrounds) from the cross validation step above. 

XGBm<-xgb.train( params=param,nrounds=best_ntrees,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)
e<-data.table(XGBm$evaluation_log)
plot(e$iter,e$train_rmse_mean,col='red')
lines(e$iter,e$test_rmse_mean,col="blue")

# just like the other model fitting methods we have seen, we can use the predict function to get predictions from the 
# model object as long as the new data is identical in format to the training data. Note that this code saves the
# predictions as a vector, you will need to get this vector into the correct column to make a submission file. 
pred<-predict(XGBm, newdata = dtest)


# here I am not making a submission file because this data is not on kaggle, instead
# I am using the metrics package to check my test error

rmse(y.test,pred)
#########################
# make a submision file #
#########################


#our file needs to follow the example submission file format.
#we need the rows to be in the correct order

test_id1<-cbind(test_id1,data.table(pred))
test_id1$SalePrice=NULL
setnames(test_id1,"pred","SalePrice")

drops<-c("Id","SalePrice")
submission<-test_id1
submission<-submission[, drops, with = FALSE]
submission$SalePrice<-exp(submission$SalePrice)

write_csv(submission,"House prices 2/project/volume/data/processed/SubmissionPrices.csv")


