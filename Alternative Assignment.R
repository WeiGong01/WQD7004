# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("ggplot2")
install.packages("psych")
install.packages('Amelia')
install.packages('mice')
install.packages("GGally")
install.packages("lares")
install.packages("janitor")
install.packages("dplyr")
install.packages("neuralnet")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(ggplot2)
library(psych) 
library(Amelia)
library(mice)
library(GGally)
library(lares)
library(janitor)
library(dplyr)
library(neuralnet)

#Data importing
#Reading data into R
df<- read.csv("D:/Class/semester 2/WQD7004 PROGRAMMING FOR DATA SCIENCE/Alternative assignment/E0.csv")

#Studying the structure of the data
str(df)
head(df)
describe(df)

#Data Cleaning
# Clean column names
clean<-clean_names(df)
colnames(clean)

# Remove empty column or rows
clean_x<-clean %>% remove_empty(whic=c("rows"))
clean_x<-clean %>% remove_empty(whic=c("cols"))

# Remove duplicate records
clean %>% get_dupes(home_team,away_team)

# Exploratory Data Analysis
#visualize the missing data
missmap(df)

#histogram graph of Full Time Home Team Goals
hist(df$FTHG, xlab="Home_goal", ylab="Frequency", col="yellow", fill="lightblue")

#histogram graph of Full Time Away Team Goals
hist(df$FTAG, xlab="Away_goal", ylab="Frequency", col="yellow", fill="lightblue")

#scatter plot, Full Time Home Team Goals VS Full Time Away Team Goals
plot(x=df$FTHG, y=df$FTAG, type="p",
     xlab="Home_goal",
     ylab="Away_goal",
     main="Home_goal Vs Away_goal"
     ,pch=19,
     col = "red",
     cex=1)

#side by side plot
boxplot(df$FTHG~df$HomeTeam, col="Orange", main="Home Impact", ylab="Home goals", las=2)

#bar-graph
counts <- table(df$FTHG, df$FTR)
barplot(counts, main="Distribution of Home/Away goals",
        xlab="Number of Goals", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

# Modelling
# Splitting the dataset into the training and test datasets
set.seed(42)
split = sample.split(df$FTR, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Fitting Neural Network to the Training set
nn=neuralnet(data$FTR~FTHG+FTAG+HS+AS+HST+AST+HF+AF+HC+AC+HY+AY+HR+AR,data=training_set, hidden=3,act.fct = "logistic", linear.output = FALSE)

# Plot the neural network
plot(nn)

# Predict results for the test dataset
predict=compute(nn,test_set)
results <- data.frame(actual = test_set$FTR, prediction = predict$net.result)
results

