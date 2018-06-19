# H2O ML Deep Learning 
# Parallel Distributed ML Algorithm with R and H2O, which is an open source math engine for Big Data that can be used to compute parallel distributed machine learning algorithms. 

# Download H2O directly at http://h2o.ai/download.
# Install H2O’s R package from CRAN at https://cran.r-project.org/web/packages/h2o/.
# install.packages("h2o")

# run h2o
library(h2o)

# Start H2O on your local machine using all available processor cores
# By default, CRAN policies limit use to only 2 cores)
# In my case there are 4 cores - Intel i7-2600K CPU.
h2o.init(nthreads = -1)

# To launch H2O locally with default initialization arguments, use the following:
# Example in R
h2o.init()

# Checking Cluster Status
# To check the status and health of the H2O cluster, use h2o.clusterInfo()
h2o.clusterInfo()

# Import dataset into H2O and display summary 
data(iris)
# Converts R object "iris" into H2O object "iris.hex"
iris.hex = as.h2o(iris, destination_frame= "iris.hex")
head(iris.hex)

# Search for factors in the data, if any
h2o.anyFactor(iris.hex)

##Displays the titles of the columns
colnames(iris.hex)
names(iris.hex)

##Displays data in a graph 
plot(iris$Petal.Length, 
     iris$Petal.Width, 
     pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], 
     xlab="Petal Length", 
     ylab="Petal Width")

## Getting Quantiles of the data
# Returns the percentiles at 0, 10, 20, ..., 100%

iris.qs <- quantile(iris.hex$Sepal.Length, probs =(1:10)/10)
iris.qs

# Take the outliers or the bottom and top 10% of iris data
Sepal.Length.outliers <- iris.hex[iris.hex$Sepal.Length <=
iris.qs["10%"] | iris.hex$Sepal.Length >= iris.qs["90%"],]
# Check that the number of rows return is about 20% of the original data
nrow(iris.hex)
nrow(Sepal.Length.outliers)
nrow(Sepal.Length.outliers)/nrow(iris.hex)

# View quantiles and histograms
quantile(x = iris.hex$Sepal.Length, na.rm = TRUE)
h2o.hist(iris.hex$Sepal.Length)

###
# Data split with target assined categorical values
library(keras)
# keras as a package uses the pipe operator (%>%) to connect functions or operations together 
# KerasR uses the dollar $ sign as operator. The pipe operator generally improves the readability of codes
# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)

# Generating Random Numbers
# Creates object for uniform distribution on iris data set
rnd_i <- h2o.runif(iris.hex)
summary (rnd_i) ## Summarize the results of h2o.runif

## Construct test and train sets 
## Create training set with threshold of 0.67
iris.train <- iris.hex[rnd_i <= 0.67,]
##Assign name to training set
iris.train <- h2o.assign(iris.train, "iris.train")
## Create test set with threshold to filter values greater than 0.67
iris.test <- iris.hex[rnd_i > 0.67,]
## Assign name to test set
iris.test <- h2o.assign(iris.test, "iris.test")
## Combine results of test & training sets, then display result
nrow(iris.train) + nrow(iris.test)
nrow(iris.hex) ## Matches the full set

# Or  by spiliting frames on two separate subsets based on a specified ratio
# Splits data in prostate data frame with a ratio of 0.75
iris.split <- h2o.splitFrame(data = iris.hex , ratios = 0.75)
# Creates training set from 1st data set in split
iris.train <- iris.split[[1]]
# Creates testing set from 2st data set in split
iris.test <- iris.split[[2]]

# Getting frames by creating a reference object to the data frame in H2O
# index.hex <- h2o.getFrame(id = "index.hex")


# Getting Models in H2O by creating a reference object for the model in H2O, using h2o.getModel().
gbm.model <- h2o.getModel(model_id = "GBM_8e4591a9b413407b983d73fbd9eb44cf")

# Listing H2O Objects by generating a list of all H2O objects generated during a session and each
object‘s size in bytes.
h2o.ls()


# Running Models in H2O 
# Some of the model types:
# 1 Gradient Boosting Machine (GBM)
# 2 Generalized Linear Models (GLM)
# 3 K-means
# 4 Principal Components Analysis (PCA)

library(h2o)
h2o.init(nthreads = -1)
data(iris)
iris.hex <- as.h2o(iris,destination_frame = "iris.hex")
iris.gbm01 <- h2o.gbm(y = 1, x = 2:5, training_frame = iris.hex, ntrees = 10,
max_depth = 3,min_rows = 2, learn_rate = 0.2, distribution= "gaussian")
# To check further the Mean-squared Error by tree from the model object type:

# To find the most important variables type:
names(iris.gbm01@model)
iris.gbm01@model$variable_importances

# In the case of classification model that uses labels we will use distribution="multinomial":
iris.gbm02 <- h2o.gbm(y = 5, x = 1:4, training_frame = iris.hex, ntrees = 15, max_depth = 5, min_rows =
2, learn_rate = 0.01, distribution= "multinomial")
names(iris.gbm2@model)
iris.gbm02@model$variable_importances

######
# Generalized linear models (GLM) ARE commonly-used sa they are extremely fast,and
# scales extremely well for models with a limited number of predictors.
binary_data <- read.csv("https://raw.githubusercontent.com/abari212/data/master/PRESENCE_ABSENCE_CASE.csv", header=T, dec=".",sep=",")
# Converts R object "binary_data" into H2O object "binary_dat.hex"
binary_data.hex = as.h2o(binary_data, destination_frame= "binary_data.hex")
head(binary_data.hex)

names(binary_data.hex)
binary_data.glm01 <- h2o.glm(y = "Y", x = c("X1","X2","X3",
"X4","X5","X6", "X7","X8"), 
training_frame = binary_data.hex, family = "binomial", nfolds = 10, alpha = 0.5)
binary_data.glm01@model$cross_validation_metrics
names(binary_data.glm01@model)

# AUC values
binary_data.glm01@model$cross_validation_metrics_summary


# Set predictor (Y) and response variables (X=(X1, X2...)
names(binary_data.hex)
Y = "Y"
X = x = c("X1","X2","X3","X4","X5","X6", "X7","X8")

# Define the data for the model and display the results
## Construct test and train sets 

# Generating Random Numbers
# Creates object for uniform distribution for binary data set
rnd_i <- h2o.runif(binary_data.hex)
summary (rnd_i) ## Summarize the results of h2o.runif

## Create training set with threshold of 0.67
binary_data.train <- binary_data.hex[rnd_i <= 0.67,]
##Assign name to training set
binary_data.train <- h2o.assign(binary_data.train, "binary_data.train")
## Create test set with threshold to filter values greater than 0.67
binary_data.test <- binary_data.hex[rnd_i > 0.67,]
## Assign a name to the test set
binary_data.test <- h2o.assign(binary_data.test, "binary_data.test")
## Combine results of test & training sets, then display result
nrow(binary_data.train) + nrow(binary_data.test)
nrow(binary_data.hex) ## Matches the full set

binary_data.glm02 <- h2o.glm(training_frame=binary_data.train, x=X, y=Y, family = "gaussian", alpha = 0.5)
# View model information: training statistics, performance, important variables
summary(binary_data.glm02)

# Predict using GLM model
binary_data.pred = h2o.predict(object = binary_data.glm02, newdata =binary_data.test)
# Look at summary of predictions: probability of TRUE
names(binary_data.pred)
binary_data.pred


