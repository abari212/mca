# chapter 03
library(rjson)
iris_json <- fromJSON("https://raw.githubusercontent.com/abari212/mca/master/iris_json.json")
names(iris_json)
# Data frame in R


# Convertion of JSON file to a data frame.
iris_data <- data.frame(iris_json)
# Dispaly dataframe content
head(iris_data)
tail(iris_data)
iris_data[1:3,1:5]

# Display of histograms and boxplots for preliminary insights 
# Check whether there are outliers, if any, in the data.
# Print a summary of iris JSON variables 
summary(iris_data)
# Display the name of colums/variables and check their attributes and classes 
names (iris_data)
hist(as.numeric(iris_data$SepalLength), breaks = 20, col = rgb(0,0.5,0.5,0.5))

# Searching for outliers using bokplot
boxplot(as.numeric(iris_data$SepalLength), col = rgb(0,0,1,0.5), main = "Sepal Length")

# Checking whether data transformations (normalisation) are required 
qqnorm(as.numeric(iris_data$SepalLength), main = "Normal QQ Plot - iris_data")
qqline(as.numeric(iris_data$SepalLength), col = "red") 

# Detecting outliers in functional type of data - example Canadian Weather datasets
# Reference : Febrero-Bande et al (2008) - Environmetrics 19, 4, 331{-}345.

# functional data extraction and preparation
library(fda)
library(fda.usc)
fdat<-fdata(t(CanadianWeather$dailyAv[,,1]))
m<-ncol(fdat)
n<-nrow(fdat)

# arguments
nb<-100;smo=0.05;trim=0.05
# The process is long depending of the number of resamples: nb )
# The Method based on trimming function: outliers.depth.trim
out.mode<-outliers.depth.trim(fdat,dfunc=depth.mode,nb=nb,smo=smo,trim=trim)
out.FM<-outliers.depth.trim(fdat,dfunc=depth.FM,nb=nb,smo=smo,trim=trim)
out.RP<-outliers.depth.trim(fdat,dfunc=depth.RP,nb=nb,smo=smo,trim=trim)
out.RPD<-outliers.depth.trim(fdat,dfunc=depth.RPD,nb=nb,smo=smo,trim=trim)

#Method based on ponderation: outliers.depth.pond
out2.mode<-outliers.depth.pond(fdat,dfunc=depth.mode,nb=nb,smo=smo)
out2.FM<-outliers.depth.pond(fdat,dfunc=depth.FM,nb=nb,smo=smo)
out2.RP<-outliers.depth.pond(fdat,dfunc=depth.RP,nb=nb,smo=smo)
out2.RPD<-outliers.depth.pond(fdat,dfunc=depth.RPD,nb=nb,smo=smo)

# plot
out<-out.FM
plot(fdat,col="blue",lty=1)
lines(fdat[out[[1]]],col=2,lty=2,lwd=2)

# Another example with outliers added to illustrate further the effects of outliers
# Data with one colomn of size 100,000 from a standard normal (i.e, a normal with mean 0 and standard deviation 1),
x = rnorm(100000,0,1); plot(density(x))
y = c(x, 10,11) ; plot(density(y))
hist(x, breaks = 50, col = rgb(0,0.5,0.5,0.5))
boxplot(x, col = rgb(0,0,1,0.5), main = "")
qqnorm(x, main = "Normal QQ Plot - y")
qqline(x, col = "red") 
hist(y, breaks = 50, col = rgb(0,0.5,0.5,0.5))
boxplot(y, col = rgb(0,0,1,0.5), main = "")
qqnorm(y, main = "Normal QQ Plot - y")
qqline(y, col = "red")

