
data <- read.csv('lending_club_loan_dataset.csv')
View(data)
#The dataset contains 20,000 observations. We can just view the first 10 obs to make it easier
head(data, 10)

# Or we can view a random 10 observations from the data
data[sample(nrow(data), 2), ]

# Check the summary of the entire dataset
summary(data)

# Or check the summary of a specific column
summary(data$term)

#---------------------------DATA PREPROCESSING----------------------# 

#Check for missing values in the dataset
sum(is.na(data)) #the dataset contains 19580 missing values


#Check for missing values in each columns:
sum(is.na(data$id)) #0
sum(is.na(data$grade)) #0
sum(is.na(data$annual_inc))#0
sum(is.na(data$short_emp))#0
sum(is.na(data$emp_length_num))#0
sum(is.na(data$home_ownership))#0
sum(is.na(data$dti)) #154 missing values 
sum(is.na(data$purpose))#0
sum(is.na(data$term))#0
sum(is.na(data$last_delinq_none))#0
sum(is.na(data$last_major_derog_none)) #19426 missing values
sum(is.na(data$revol_util))#0
sum(is.na(data$total_rec_late_fee))#0
sum(is.na(data$od_ratio))#0
sum(is.na(data$bad_loan))#0

#We have two columns that contains missing values, DTI and last_major_derog_none
# The DTI is missing only 154 values while last_major_derog_none is missing more than half.
# Therefore, last_major_derog_none will be dropped from the analysis

#Now for the DTI columns, the missing values will be replaced with the mean of the column

data$dti[is.na(data$dti)] <- mean(data$dti, na.rm = TRUE)

#Check again for missing values
sum(is.na(data$dti))


# Check for missing values in the entire dataset
sum(is.na(data))

#Check the columns types
str(data)


data= subset(data, select = -c(last_major_derog_none) )
# Dropping the ID columns as well since it doesn't provide any useful info 
data= subset(data, select = -c(id))

sapply(data, class)
View(data)


data$bad_loan <- as.factor(data$bad_loan)

#Step 1: Split the data into training and testing sets (75:25)
library(caTools)
split= sample.split(data$bad_loan, SplitRatio = 0.75)
training_set= subset(data, split==TRUE)
test_set = subset(data, split==FALSE)

#To separate the target variable from the rest of the dataset
x = training_set[,-13]
y = training_set$bad_loan

str(y)

library(caret)
library(e1071)
modelNB = train(x,y,'nb', ,trControl=trainControl(method='cv',number=10))
modelNB
Predict <- predict(modelNB,newdata = test_set ) 

cm= table(Predict, test_set$bad_loan)
cm


accuracy_score <- sum(diag(cm))/sum(cm)
accuracy_score

