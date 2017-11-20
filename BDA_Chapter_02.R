# cma Chapter 02

# Parallel processing (locally)
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
