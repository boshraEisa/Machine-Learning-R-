# First step: load the data
data <- read.csv('lending_club_loan_dataset.csv')

#View the data
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


data= subset(data, select = -c(last_major_derog_none) )
# Dropping the ID columns as well since it doesn't provide any useful info 
data= subset(data, select = -c(id))

sapply(data, class)
View(data)

#Now for the DTI columns, the missing values will be replaced with the mean of the column

data$dti[is.na(data$dti)] <- mean(data$dti, na.rm = TRUE)

#Check again for missing values
sum(is.na(data$dti))

# Check for missing values in the entire dataset
sum(is.na(data))

#Check the columns types
str(data)

# Some columns have the wrong type and must be corrected
#grade,          char --> factor
#short_emp,       int --> factor
#home_ownership,  char--> factor
#purpose,         char--> factor
#last_delinq_none,int --> factor
#bad_loan,        int --> factor


data$grade <- as.factor(data$grade)
data$short_emp <- as.factor(data$short_emp)
data$home_ownership <- as.factor(data$home_ownership)
data$purpose <- as.factor(data$purpose)
data$last_delinq_none <- as.factor(data$last_delinq_none)
data$bad_loan <- as.factor(data$bad_loan)


#Check the columns types
str(data)

# Checking imbalanced data
barplot(prop.table(table(data$bad_loan)),main="Imbalanced Data in bad_loan")
# The data is imbalanced
prop.table(table(data$bad_loan))
table(data$bad_loan)


#install and load the libraries
install.packages("ROSE")
install.packages('rpart')
library(rpart)
library(ROSE)


#using the oversampling method and printing out a table for it
over_data_balanced <- ovun.sample(bad_loan ~ ., data = data, method = "over")$data
table(over_data_balanced$bad_loan)

#using the undersampling method and printing out a table for it
under_data_balanced <- ovun.sample(bad_loan ~ ., data = data, method = "under")$data
table(under_data_balanced$bad_loan)

#using a combination between the both methods and printing out a table for it
both_data_balanced <- ovun.sample(bad_loan ~ ., data = data, method = "both")$data
table(both_data_balanced$bad_loan)

# comparing the balanced data techniques
barplot(prop.table(table(both_data_balanced$bad_loan)),main="Balanced Data (Both)")
barplot(prop.table(table(under_data_balanced$bad_loan)),main="Balanced Data (Under)")
barplot(prop.table(table(over_data_balanced$bad_loan)),main="Balanced Data (Over)")




#---------------------------DATA VISUALIZATION----------------------# 


library(DataExplorer)
plot_str(data, fontSize=20)

plot_bar(data$purpose, title= "Bar Chart for the Frequency")

plot_correlation(data, type=c("Continuous Variables"))

plot_histogram(data$emp_length_num, title="Empolynment in years")

# https://www.r-bloggers.com/2021/08/how-to-plot-categorical-data-in-r-quick-guide/
library(ggplot2)
#Grade:
ggplot(data, aes(x=reorder(grade, grade, function(x)-length(x)))) +
geom_bar(fill='red') +  labs(x='grade')

#delinquency
ggplot(data, aes(x=reorder(last_delinq_none, last_delinq_none, function(x)-length(x)))) +
  geom_bar(fill='pink') +  labs(x='delinquency')



#-----------------------MODEL IMPLEMENTATION-----------------------# 

#Random Forest: 
#step 1: Data partition
#step 2: Loading into the model
#step 3: Prediction & confusion matrix (train data)
#step 4: Prediction & confusion matrix (test data)
#step 5: Tuning 
#Step 6: Plotting


#Data partition
#divide the data into training and testing (70:30)
set.seed(123)
ind <- sample(2, nrow(over_data_balanced), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]


#Loading the data into the model
library(randomForest)
set.seed(222)
rf <- randomForest(bad_loan~., data=train,
                   ntree = 600,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
attributes(rf)
# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
cmTraining <- confusionMatrix(p1, train$bad_loan)
cmTraining

# Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
cmTesting <- confusionMatrix(p2, test$bad_loan)
cmTesting




# Tune mtry
t <- tuneRF(train[,-13], train[,13],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 600,
            trace = TRUE,
            improve = 0.05)
print(t)

# Number of nodes for trees
hist(treesize(rf),
     main = "No. of nodes for trees",
     col = "green")

varImpPlot(rf)
importance(rf)
varUsed(rf)
