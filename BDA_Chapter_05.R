# This chapter 5 is linked to lecture 5 on Big Data 
# iT consits of different types of data (including sensor data), different types of ML and platforms (R and Spark) 
# Setting up working directory using setwd()
setwd("C:/MCA2017") 

# Libraries to prepare and analyse data 
# dplyr libaray used as a number useful resource for data cleaning and data manipulation.
# dplyr has a number of verbs that can be used to prepare data within the cluster.
# datasets can be connected from R into the Spark cluster 
# For this install the nycflights13 and fda (functional data) packages 
library(dplyr)

# Data manipulation tasks under dplyr can be performed with the assistance of the forward-pipe operator (%>%). 
# Example of using forward-pipe operator (%>%) to create an ID variable to be used as the primary key
df <- df %>%
  mutate(ID = seq(1:nrow(df))) %>%
  select(ID, everything())

# Example of using forward-pipe operator (%>%) to merge tables - table 2 with to table 1 either left or right 
left_join(table_1, table_2, by = "ID")
right_join(table_1, table_2, by = "ID")


# Data loading and connecting - install.packages(c("nycflights13", "fda"))
library(nycflights13)
library(fda)
iris_data <- copy_to(sc, iris)
flights_data <- copy_to(sc, nycflights13::flights, "flights")
climate_data <- copy_to(sc, fda::CanadianWeather, "climate")

# Learning about data - Sructure and dimension of data sets

str(iris_data)
str(flights_data)
str(climate_data)

dim(iris_data)
dim(flights_data)
dim(climate_data)

# Data sets variables names and their number
names(iris_data)
names(flights_data)
names(climate_data)

length(iris_data)
length(flights_data)
length(climate_data)

# Display of few records 
head(iris_data, 3)
head(flights_data, 3)
head(climate_data, 3)

iris_data[1:3, 1:5] # display first 3 rows and first 5 columns
flights_data[1:3, 1:5]
climate_data[1:3, 1:5]
names(climate_data)
length(climate_data$coordinates)  # display the number of rows

str(climate_data$dailyAv) # display the structure (rows and variables) 
str(climate_data$coordinates)

dimnames(climate_data$dailyAv)[[1]]
dimnames(climate_data$dailyAv)[[2]]
dimnames(climate_data$dailyAv)[[3]]

dimnames(climate_data$coordinates)[[1]]
dimnames(climate_data$coordinates)[[2]]

## Common to both climtae data sets of dailyAv and coordinates data is the variable containing station names() 
## Dsiplaying climate data using coordinates of stations 

with(climate_data, plot(-coordinates[, 2], coordinates[, 1], type='p',
            col="blue",  xlab="Longitude", ylab="Latitude",  axes=FALSE) )
Wlon <- pretty(climate_data$coordinates[, 2])
axis(1, -Wlon, Wlon)
axis(2)

# plot() function has a list of all the different types of displaying data in a graph
# "p": Points
# "l": Lines
# "b": Both points and lines combined
# "c": The lines part alone of "b"
# "o": Both "overplotted"
# "h": Histogram like (or high-density) vertical lines
# "n": No plotting

with(climate_data, points(-coordinates[, 2], coordinates[, 1],
                             col="blue", pch='*' ))

# Adding a map countour to the plotted (geo/gps) coordinates (dataset) to locate the points geeographically 

library(maps)
map("world", boundary = FALSE, col="gray", add = TRUE)
map("world", add = TRUE)
map("world", boundary = FALSE, col="gray", add = TRUE)

# Display data for a station representing each of Canada 4 regions  
matplot(day.5, climate_data$dailyAv[, stations, "Temperature.C"],
        type="l", axes=FALSE, xlab="", ylab="Mean Temperature (deg C)")
axis(2, las=1)
# Axes labelling with the horizontal axis labelled with the month names
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)

# Adding the names of the weather stations
mtext(stations, side=4,
      at=climate_data$dailyAv[365, stations, "Temperature.C"],
      las=1)


####
# Climate data is a functional data with observations that are functions rather than single records
# For this type of data, functions are considered as the sum of functions, with basis functions
# To create the basis function for dailyAv, fourier could be used as follows:
daybasis65 <- create.fourier.basis(c(0, 365), 65)
daytempfd <- with(climate_data, smooth.basis(
  day.5, dailyAv[,,"Temperature.C"],
  daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd )
with(climate_data, plotfit.fd(
  dailyAv[,,"Temperature.C"], argvals=day.5,
  daytempfd, index=12, titles=place, col="red", axes=FALSE) )
# Label the horizontal axis with the month names
axisIntervals(1)
axis(2)
# Depending on the physical size of the plot,
# axis labels may not all print.
# In that case, there are 2 options:


##############
# dplyr data manipultaion - filtering procedure
# Filight data can be filtered by departure delay
flights_data %>% filter(dep_delay == 2)

delay <- flights_data %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

# Display of delays - ## `geom_smooth()` using method = 'gam'
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() + scale_size_area(max_size = 2)



## Using SQL
## SQL queries can be directly performed against tables within a Spark cluster. 
## The spark_connection object implements a DBI interface for Spark to query data that can be preparaed as R data frame:
  
library(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview

# function creation for data preparataion and data analysis
# Function to normalize using Range

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# Distributed R
# RStudio IDE and Spark
# Recent RStudio IDE releases includes integrated support for Spark and the sparklyr package, including tools for:
# Creating and managing Spark connections
# Browsing the tables and columns of Spark DataFrames
# Once sparklyr package installed the new Spark pane appaers within the IDE with connection dialog to make connections to local or remote Spark instances:
# Once a connection to Spark is estanlished the tables contained within the Spark cluster can be browsed 
# RStudio data viewer can be used to preview Spark DataFrames. The RStudio IDE features for sparklyr are available as part of the RStudio Preview Release.

library(sparklyr)
# You can execute arbitrary r code across your cluster using spark_apply. For example, we can apply rgamma over iris as follows:
  
  spark_apply(iris_data, function(data) {
    data[1:4] + rgamma(1,2)
  })


# Instead of clusters groups can be made by grouping columns to perform an operation over each group of rows 
#  and make use of any package within the closure:
  
  spark_apply(
    iris_data,
    function(e) broom::tidy(lm(Petal_Width ~ Petal_Length, e)),
    names = c("term", "estimate", "std.error", "statistic", "p.value"),
    group_by = "Species"
  )

library(sparklyr)
# Spark Extensions
# The facilities used internally by sparklyr for its dplyr and machine learning interfaces are available to extension packages. 
# Since Spark is a general purpose cluster computing system there are many potential applications for extensions.
# Defining an R interface to Spark line counting
count_lines <- function(sc, path) {
  spark_context(sc) %>% 
    invoke("textFile", path, 1L) %>% 
    invoke("count")
}

# Use of spark to count the lines of the CSV
count_lines(sc, tempfile)

# Spark allows to create extensions using sparklyr
# Table Utilities - caching a table into memory with:

tbl_cache(sc, "climate_data")
# and unload from memory using:

tbl_uncache(sc, "climate_data")

# Connection Utilities - the Spark web console using the spark_web function:

spark_web(sc)

# The log can be shwon using the spark_log function:

spark_log(sc, n = 10)


# Using H2O ML/AI framework 
# A package under the name rsparkling extends sparklyr to provide an interface into Sparkling Water. 
options(rsparkling.sparklingwater.version = "2.1.14")

library(rsparkling)
library(sparklyr)
library(dplyr)
library(h2o)

sc <- spark_connect(master = "local", version = "2.1.0")
mtcars_data <- copy_to(sc, mtcars, "mtcars")

mtcars_h2o <- as_h2o_frame(sc, mtcars_data, strict_version_check = FALSE)

mtcars_glm <- h2o.glm(x = c("wt", "cyl"), 
                      y = "mpg",
                      training_frame = mtcars_h2o,
                      lambda_search = TRUE)

# To prepare the data dplyr filtering process can help 
# filter fight data by departure delay and display the first few records

flights_data %>% filter(dep_delay == 2)

# Checking the data components in terms of variable names 
# Variable names of the data. New names can be assigned to the variables.
library(knitr) # Data Table package
library(DT) # Data Table package
kable(head(iris_data, 3))

# Structure and Dimension of the Data
# Understanding the structure of the data by using str() and dim () functions
str(iris_data)
dim(iris_data)

# Summary of the variables
# Displaying a summary of all the variables using summary() function.

# Displaying the data with only a few records using head() for first few rows or tail() for last few rows. 
# To better display the records use kable() function .
# Another option to interactively display dataset is to use library(DT) and library(knitr) 
library(knitr) # Data Table package
library(DT) # Data Table package
kable(head(iris_data, 3))
kable(tail(iris_data, 3))

# Data analysis and variable creation
# There may be a a need to modify or convert data types of certain variables, depending on based on their classes.
# Using the function as.numeric()
# Random Sampling of data sets to a Train and Test set for prediction
# Data will be split into random samples with either 70/30  of 50/50 ratios for fitst training to develop the model and a Test set to validate or carry oout the model's predction (unknon set).

# ML linear regression model can be used for predictionn such as predicting a car's fuel consumption (mpg) based on its weight (wt), and the number of cylinders the engine contains (cyl). 
# In this cas the relationship between mpg and each of our features is asuumed to be of linear nature.

# copy mtcars into spark
mtcars_data <- copy_to(sc, mtcars)

# Prior to analysis transform the data set, which partitioned into 'training', 'test'
partitions <- mtcars_data %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# After data preparation and partitioning of data models can be fit to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))
fit



# Gini index
# Gini is also commonly used measure to assess the performance of algorithms
# It is related to AUC as Gini = 2*AUC - 1.

# Model Performance Comparision - Measures of Accuracy

############################3
# Presence and absence data set
library(ROCR)        # Assess Model Performance with AUC curve

# binary_data <- read.table(file.choose(), header=TRUE, sep=",")
# Or call data from Github
binary_data <- read.csv("https://raw.githubusercontent.com/abari212/data/master/PRESENCE_ABSENCE_CASE.csv", header=T, dec=".",sep=",")

write.csv(binary_data, file="binary_data.csv")
names(binary_data)

binary_data <- read.csv("https://raw.githubusercontent.com/abari212/data/master/PRESENCE_ABSENCE_CASE.csv", header=T, dec=".",sep=",")

names(binary_data)
head(binary_data)
# Data preparation for ML in the form Y (response) <- f(X)
# Y is a binary variable and X is either the enivronment or attributes or both
binary_data$ID_R <- NULL 

binary_data <- na.omit(binary_data)
names(binary_data)
# Transformin data, if needed and then partition into a training and a test set.
data.index <- 1:nrow(binary_data)
testindex <- sample(data.index, trunc(length(index)/3))
data.test <- binary_data[testindex,]
data.train <- binary_data[-testindex,]

# Once ddata is prepraed ML can be carried out by fitting the model to
# either a linear model (GLM) or non-linear model (RF) to the training dataset (data.test)

# # Machine learning algorithms can be invoked in a Spark cluster via the machine learning functions within sparklyr. 
# Prior to prediction tuning of machine learning model can be carried out using the machine learning functions

# Logistic Regression
library(VGAM)
MLmodel.logit<-vglm(data.train$Y ~., family = "multinomial", data = data.train, maxit = 100) 
#  Accuracy ROC

summary(MLmodel.logit)
data.test$predictions <- with(data.test, predict(MLmodel.logit, data.test)) 
data.test$labels <- with(data.test$test, data.test$Y)

# ===
# Logistic Regression model
# Known also as logit model, which is a regression model where the dependent or response variable is categorical. 
MLmodel.logit.frame <- data.frame(cbind(data.test$predictions, data.test$labels))
# View(MLmodel.logit.frame)
names(MLmodel.logit.frame)
names(MLmodel.logit.frame)[c(1)] <- c("predictions")
names(MLmodel.logit.frame)[c(2)] <- c("labels")
#  View(MLmodel.logit.frame)
pred <- prediction(MLmodel.logit.frame$predictions, MLmodel.logit.frame$labels)
perf <- performance(pred,"tpr", "fpr")
plot(perf,colorize = TRUE)
grid()

library(sm)
names(MLmodel.logit.frame)
# Display the different grouos using density plots 
# =====
Presence.f <- factor(MLmodel.logit.frame$labels, levels= c(0,1),
                     labels = c("Absence", "Presence")) 

sm.density.compare((MLmodel.logit.frame$predictions-1), MLmodel.logit.frame$labels, xlab="")
title(main="Distribution of Absence and Presence")

# Adding legend to the graphs using the mouse click
colfill<-c(2:(2+length(levels(Presence.f)))) 
legend(locator(1), levels(Presence.f), fill=colfill) 



library(randomForest)# Developed by Leo Breiman and Cutler for Classification and Regression 
library(ggplot2)     # An Implementation of the Grammar of Graphics 
library(knitr)       # For Dynamic Report Generation in R 
# Tuning the model - defining the number of iteration and split variables
tuneRF(x=data.train, y=data.train$Y, trace=TRUE)

# Fiting the model
MLmodel.rf <- randomForest(as.factor(data.train$Y) ~., data = data.train, mtry=6,ntree=500, scale=TRUE, importance=TRUE, proximity=TRUE)

# Under Spark, we can use summary() the fitted model to check the quality of the fit and also carry out model tuning .
summary(MLmodel.rf)

# Calculate the confusion matrix and accuracy of the model
library(caret)
acc.res <- confusionMatrix(data.test$Y,predict(MLmodel.rf, data.test))
names(acc.res)


# Variable importance in the split
names(MLmodel.rf)
varImpPlot(MLmodel.rf,type=2)

# Modeling data with Y as a function of X
names(binary_data)
X <- binary_data[,2:9] 
Y <- binary_data$Y
### Unsupervised ML
# kmeans model
MLmodel.kc <- kmeans(X,2)
names(MLmodel.kc)
# Accuracy/Performance of model
table(Y, MLmodel.kc$cluster)
sub_data = binary_data[,c(2,5)]
plot(sub_data, col = MLmodel.kc$cluster)

sub_data = binary_data[,c(6,7)]
plot(sub_data, col =(MLmodel.kc$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

 
# predict the associated class
library(sparklyr)
names(binary_data)
binary_data <- data.frame(binary_data)
Skmeans_model <- binary_data %>%
  select(X6, X7) %>%
  ml_kmeans(centers = 2)
predicted <- sdf_predict(MLmodel.kc, binary_data) %>%
  collect
table(predicted$Y, predicted$prediction)

# Checking the accuracy of model (RF)
# Receiver Operating Characteristic(ROC) curve
# The ROC curve has its origin in engineering for diagnostic test evaluation. The curve as two amin axis one for true positive rate values (Sensitivity), which is plotted in function of the axis of false positive rate (100-Specificity), where each point on the  curve corresponds to a particular decision threshold. 
# ROC curve is a way to carry out cost/benefit analysis of diagnostic decision making process.

# AUC, which is the Area Under Curve of the ROC curve
# AUC metric is the most common measure of accuracy metrics for machine learning techniques for binary problems.  
# AUC values vary between an area under the curve of 1, where the entire graph would fall beneath the curve
# to 0 when the area under the curve is equal 0.0.

library(caret)
acc.res <- confusionMatrix(data.test$Y,predict(MLmodel.rf, data.test))
names(acc.res)

# ===
#  ROC
names(MLmodel.rf)
data.test$predictions <- with(data.test, predict(MLmodel.rf, data.test)) 
data.test$labels <- with(data.test$test, data.test$Y)

# ===
MLmodel.rf.frame <- data.frame(cbind(data.test$predictions, data.test$labels))
# View(MLmodel.rf.frame)
names(MLmodel.rf.frame)
names(MLmodel.rf.frame)[c(1)] <- c("predictions")
names(MLmodel.rf.frame)[c(2)] <- c("labels")
#  View(MLmodel.rf.frame)
pred <- prediction(MLmodel.rf.frame$predictions, MLmodel.rf.frame$labels)
perf <- performance(pred,"tpr", "fpr")
plot(perf,colorize = TRUE)
grid()

library(sm)
names(MLmodel.rf.frame)
# Display the different grouos using density plots 
# =====
Presence.f <- factor(MLmodel.rf.frame$labels, levels= c(0,1),
                       labels = c("Absence", "Presence")) 

sm.density.compare((MLmodel.rf.frame$predictions-1), MLmodel.rf.frame$labels, xlab="")
title(main="Distribution of Absence and Presence")

# Adding legend to the graphs using the mouse click
colfill<-c(2:(2+length(levels(Presence.f)))) 
legend(locator(1), levels(Presence.f), fill=colfill) 

###
# Exploring different ML using iris data - Machine Learning Models Accuracy - # Model Selection and Development
# The most important thing in developing model is to select right modeling machine learning algorithms 
# Machine learning algorithms can be performed in a Spark cluster via the machine learning functions using sparklyr. These functions connect to a set of high-level APIs built on top of DataFrames to create and tune machine learning workflows.

set.seed(123)

# Data Exploration
head(iris_data)
summary(iris_data)


# Multinomial linear ML
library(nnet)
library(caret)

nv <- length(iris_data[1,]) # number of variables as attributes
X <-iris_data[,1:(nv-1)] 
Y <-iris_data[,nv]
formula_YX <- as.formula(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)
nlev<-nlevels(Y) # number of factors describing class
model<-multinom(formula_YX , data = iris_data, maxit = 500, trace=FALSE)
prob<-predict(model,X,type="probs") 
pred<-apply(prob,1,which.max)
pred[which(pred=="1")]<-levels(Y)[1] 
pred[which(pred=="2")]<-levels(Y)[2] 
pred[which(pred=="3")]<-levels(Y)[3] 
pred[which(pred=="4")]<-levels(Y)[4] 
pred<-as.factor(pred)
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)

# Non-Linear ML Classification - Mixture Discriminant Analysis
library(mda) 
model<-mda(formula_YX,data=iris_data) 
pred<-predict(model,X) 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)


# Non-Linear ML - Regularized Discriminant Analysis
library(klaR) 
model<-rda(formula_YX,data=iris_data,gamma = 0.05,lambda = 0.01) 
pred<-predict(model,X)$class 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)

# Non-Linear ML - Neural Networ
# Neural Network Modeling
# Normally doesn't fit well with low number of observationsFor Neural network we need numeric data to model.

library(nnet)
library(devtools)
model<-nnet(formula_YX,data=iris_data,size = 3, decay = 0.0001, maxit = 500, trace = FALSE)
pred<-predict(model,X,type="class") 
pred<-as.factor(pred)
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)

# Support Vector Machine (SVM)
# A Support Vector Machine (SVM) performs classification by constructing an N-dimensional hyperplane that optimally separates the data into two categories. The SVM models are closely related to neural networks 
# ML Support Vector Machine
library(kernlab) 
model<-ksvm(formula_YX,data=iris_data) 
pred<-predict(model,X,type="response") 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)


# ML k-Nearest Neighbors
library(caret) 
model<-knn3(formula_YX,data=iris_data,k=nlev+1) 
pred<-predict(model,X,type="class") 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)


# Naive Bayes
library(e1071) 
model<-naiveBayes(formula_YX,data=iris_data,k=nlev+1) 
pred<-predict(model,X) 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)


# Recursive partitioning for classification
# Recursive partitioning is a multivariable analysis method, which creates a decision tree that split data it into sub-populations 
# Non-Linear Classification with Decision Trees
library(rpart) 
library(rpart.plot)
model<-rpart(formula_YX,data=iris_data) 
pred<-predict(model, X ,type="class") 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)


# Random Forest
# Random forest is an ensemble learning method as it is base on a multitude of decision trees above.
# ML Random Forest
library(randomForest) 
model<-randomForest(formula_YX,data=iris_data) 
pred<-predict(model,X) 
l<-union(pred,Y)
mtab<-table(factor(pred,l),factor(Y,l))
confusionMatrix(mtab)

# H2O Deep Learning
# Next updates




