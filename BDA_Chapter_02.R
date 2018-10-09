# cma Chapter 02

# Parallel processing CAN can be done,localy and also in a remote cluster
# A cluster is a collection of nodes (servers) for storing and analyzing huge amounts of unstructured data in a more distributed computing environment.
# Under such archtecture parallel processing helps to carry our distributed computing. 
library(doParallel)
library(dplyr) # 
# Calculate the number of cores available locally
no_cores_1 <- detectCores() - 1 # Leave a spare core 
# Initiate cluster
cl <- makeCluster(no_cores_1)
registerDoParallel(cl)

# Data for parallel processing 
library(jsonlite)
# install.packages(jsonlite)
iris_json <- fromJSON("https://raw.githubusercontent.com/abari212/mca/master/iris_json.json")
names(iris_json)
# Data frame in R
iris_data <- data.frame(iris_json)
summary(iris_data)
names(iris_data)
# Explore and process data
iris_data[1,]
plot(iris_data[,2])
# Process data in parallel 
foreach(i = 1:3, .combine=c, .packages="dplyr") %dopar% {
  iris_data[i, ] %>% select(-Species) %>% sum
}
stopCluster(cl)

# There is also another parallel processing code using the foreach package:

# to inslall foreach
install.packages("foreach", repos="http://R-Forge.R-project.org")

# to run foreach
library(foreach)
# To execute repeatedly a function (function) under foreach: 
x <- foreach(i=1:length(x)) %do% function(i)
####  
library(doParallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
foreach(i = 1:3, .combine=c) %dopar% {
  i**2
}

stopCluster(cl)
