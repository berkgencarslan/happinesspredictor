
setwd("C:\\Users\\BERK\\Desktop")
training_data <- read.csv("projetrain.csv")
test_data <- read.csv("projetest.csv")
str(training_data_imp)
summary(training_data)
dim(training_data)
dim(test_data)
glimpse(training_data)
glimpse(test_data)

install.packages("dplyr")
install.packages("caret")
install.packages("caretEnsemble")
install.packages("mice")
install.packages("doParallel")
install.packages("car")
install.packages("CatEncoders")
library(CatEncoders)
library(dplyr)
library(caret)
library(caretEnsemble)
library(mice)
library(doParallel)
library(car)
library(plyr)

training_data[training_data==""]<-NA
test_data[test_data==""]<-NA
#replicate our sets
training_data_bind <- training_data
test_data_bind <- test_data
#create a new column "set" to label the observations
training_data_bind$set <- "TRAIN"
test_data_bind$set <- "TEST"
#merge them into 1 single set

a <- read.table("rowsnotneeded.txt")
class(a)
a.list <- as.list(as.data.frame(t(a)))

training_data_bind$Happy -> Happy
training_data_bind$Happy = NULL
full_dataset <- rbind(training_data_bind, test_data_bind)

na_rows = rowSums(is.na(full_dataset))
na_rows
read = full_dataset[which(na_rows > 50),]
which.max(na_rows)

dim(full_dataset)
set.seed(123)
imputed_full <- mice(full_dataset, 
                     m=1, 
                     maxit = 5,
                     seed=500)
full_imputed <- complete(imputed_full, 1)
dim(full_imputed)
(na_count_full_imputed <-data.frame(sapply(full_imputed, function(y) sum(length(which(is.na(y)))))))

issue_columns <- subset(imputed_full$loggedEvents, 
                        meth == "constant" | meth == "collinear")
print(issue_columns)


summary(full_imputed)
summary(full_imputed$EducationLevel)

min(full_imputed$YOB)
mean(full_imputed$YOB)
range(full_imputed$YOB)

full_imputed$YOB = full_imputed$YOB -1899

levels(full_imputed$EducationLevel) <- c(levels(full_imputed$EducationLevel), 1,2,3,4,5,6,7) 

full_imputed$EducationLevel[full_imputed$EducationLevel=="Current K-12"] <- "1"
full_imputed$EducationLevel[full_imputed$EducationLevel=="High School Diploma"] <- 2
full_imputed$EducationLevel[full_imputed$EducationLevel=="Current Undergraduate"] <-3
full_imputed$EducationLevel[full_imputed$EducationLevel=="Associate's Degree"] <-4
full_imputed$EducationLevel[full_imputed$EducationLevel=="Bachelor's Degree"] <- 5
full_imputed$EducationLevel[full_imputed$EducationLevel=="Master's Degree"] <- 6
full_imputed$EducationLevel[full_imputed$EducationLevel=="Doctoral Degree"] <- 7

summary(full_imputed$Income)

levels(full_imputed$Income) <- c(levels(full_imputed$Income), 1,2,3,4,5,6)

full_imputed$Income[full_imputed$Income=="under $25,000"] <- "1"
full_imputed$Income[full_imputed$Income=="$25,001 - $50,000"] <- 2
full_imputed$Income[full_imputed$Income=="$50,000 - $74,999"] <-3
full_imputed$Income[full_imputed$Income=="$75,000 - $100,000"] <-4
full_imputed$Income[full_imputed$Income=="$100,001 - $150,000"] <- 5
full_imputed$Income[full_imputed$Income=="over $150,000"] <- 6

full_imputed$Income  <- droplevels(full_imputed$Income)
full_imputed$Income <- as.numeric(as.character(full_imputed$Income))

full_imputed$EducationLevel<- droplevels(full_imputed$EducationLevel)
full_imputed$EducationLevel <- as.numeric(as.character(full_imputed$EducationLevel))

full_imputed = droplevels(full_imputed)

(na_count_full_imputed <-data.frame(sapply(training_data, function(y) sum(length(which(is.na(y)))))))
summary(training_data)
summary(training_data_imp)
summary(full_imputed)


#subset the full_imputed_filtered dataset
training_data_imp <- subset(full_imputed, set == "TRAIN")
test_data_imp <- subset(full_imputed, set == "TEST")

#drop the "set" column, we don't need it anymore
training_data_imp$set <- NULL
test_data_imp$set <- NULL


full_imputed_2 <- full_imputed


install.packages("fastDummies")
library(fastDummies)

training_data_imp <- dummy_cols(training_data_imp, remove_selected_columns =TRUE, remove_first_dummy = TRUE)


training_data_imp["Happy"] <- Happy
training_data_imp <- training_data_imp[, !(colnames(training_data_imp) %in% c("UserID","set_TEST"))]

test_data_imp <- dummy_cols(test_data_imp, remove_selected_columns =TRUE, remove_first_dummy = TRUE)
test_data_imp <- test_data_imp[, !(colnames(test_data_imp) %in% c("UserID","set_TEST"))]


summary(test_data_imp)
summary(training_data_imp)
str(training_data_imp3)
sapply(full_imputed_2,class)


#check dimensions
dim(training_data_imp)
dim(test_data_imp)
summary(full_imputed_2)

install.packages("randomForest")
install.packages("party")
library(randomForest)
library(party)


names(training_data_imp) <- make.names(names(training_data_imp))
names(test_data_imp) <- make.names(names(test_data_imp))

rf <- randomForest(Happy ~ .,data=training_data_imp)
cf <- cforest(Happy ~ .,data=training_data_imp)
lm1=lm(Happy ~ .,data=training_data_imp)
summary(lm1)

predicted = predict(cf, newdata=test_data_imp)
predicted = predict(rf, newdata=test_data_imp)
predicted

test_data$UserID -> ID
typeof(ID)

submit <- data.frame(ID)
submit$predicted <- predicted
submit

write.csv(submit,"C:\\Users\\BERK\\Desktop\\cforest.csv", row.names = FALSE)

summary(rf)
importance(rf)

training_data_imp2 <- training_data_imp[, !(colnames(training_data_imp) %in% c("Q123464_Yes","Q107491_Yes"))]
test_data_imp2 <- test_data_imp[, !(colnames(test_data_imp) %in% c("Q123464_Yes","Q107491_Yes"))]
rf2 <- randomForest(Happy ~ .,data=training_data_imp2)
cf2 <- cforest(Happy ~ .,data=training_data_imp2)

predicted2 = predict(rf2, newdata=test_data_imp2)
predicted2 = predict(cf2, newdata=test_data_imp2)

submit <- data.frame(ID)
submit$predicted <- predicted2

write.csv(submit,"C:\\Users\\BERK\\Desktop\\newest.csv", row.names = FALSE)

lm2=lm(Happy ~ .,data=training_data_imp2)
summary(lm2)


