# Chapter 4
# 
# Starting by identyfying a problem and formulation of a Hypothesis (Project) 
# step 1 – Problem statement,
# step 2 – Hypothesis formulation, 
# step 3 – Data gathering and preparation (pre-processing),
# step 4 – Model development (validation and tuning),
# step 5 – Interpretation and draw of the conclusions.

# Reference: Cherkassky and Mulier (2007) 

# Accuracy metric - Area under the curve (AUC)

# Sensitivity =  True positive/ (True positive  + False negative)

# Specificity = True Negative / (True Negative + False Positive)

# Start by placing data into spark

iris_data <- copy_to(sc, iris_json)

# Transform data, if needed and then partition into a training and a test set.
data.index <- 1:nrow(iris_data)
    testindex <- sample(data.index, trunc(length(index)/3))
    data.test <- iris_data[testindex,]
    data.train <- iris_data[-testindex,]

# Once ddata is prepraed ML can be carried out by fitting the model to
# either a linear model (GLM) or non-linear model (RF) to the training dataset (data.test)

# # Machine learning algorithms can be invoked in a Spark cluster via the machine learning functions within sparklyr. 
# Prior to prediction tuning of machine learning model can be carried out using the machine learning functions

tuneRF(x=data.train[,1:4], y=data.train$Species, trace=TRUE)

MLmodel.rf <- randomForest(Species ~., data = data.train, mtry=2,ntree=500, scale=TRUE, importance=TRUE, proximity=TRUE)

# Under Spark, we can use summary() the fitted model to check the quality of the fit and also carry out model tuning …
summary(MLmodel.rf)

#calculate the confusion matrix and accuracy
library(caret)
acc.res <- confusionMatrix(mca_test$Species,predict(mca_rf, mca_test[,1:4]))
names(acc.res)


# Variable importance in the split
names(MLmodel.rf)
varImpPlot(MLmodel.rf,type=2)

# Variable importance in the split
names(mca_rf)
varImpPlot(MLmodel.rf,type=2)

### Unsupervised ML
# use of iris_data without its labels (Species' names = y)
# command "names" displays column names (x = variables and y= labels)
names(iris_data)
X = iris_data[,-5] # variables 
Y = iris_data[,5] # labels

# kmeans model
MLmodel.kc <- kmeans(X,3)
names(Mlmodel.kc)
# Accuracy/Performance of model
table(Y, MLmodel.kc$cluster)
plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(MLmodel.kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)



