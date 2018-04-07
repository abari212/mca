# This is part of a series of scripts to use R and Spark platforms combined to scale data processing and analytics 
# Chapter 01 helps to get acquainted with R and Spark platforms (magic platform) 
# Through the use the case of extracting and transforming HTML data into a table or a data frame
# Using as an example the stock market data - Apple stock market trends case 
# Starting by Setting up the working platforms (R and Spark)
# Start by installing R followed by R Studio

# R is available to download and install and run on Windows, OS X and a wide variety of Unix platforms. 
# The primary part of R language is available from its Comprehensive R Archive Network (CRAN). Many add-on packages used to extend the functionality of R language are also hosted in the CRAN.

# Download R from the site http://cran.us.r-project.org/.
# Click on Base to download R 

# Once R installed download RStudio Desktop for windows from http://rstudio.org/download/desktop 

# Once RStudio installed run or open RStudio
# Under RStudio you can also setup and change your working directory

setwd()
getwd()

# Setting up data working directory 
# It is always handy to set up a working directory (wd) prior to data integration, preparation and analysis 

setwd("~/mydirectory") 

# Use the command-line scripting and editing at the top left window of RStudio, which is 
# the console where command-line scripting and editing space to create a file with multiple lines of R code
# Installing packages under RStudio by typing : install.packages(package-name) # Rstudio tab

# Spark can be installed via Rstudio using sparklyr.
install.packages(sparklyr)
# To run the package - library(package-namE) 
library(sparklyr)
# devtools::install_github("rstudio/sparklyr")
spark_install()
# options(sparklyr.java8 = TRUE)
sessionInfo()
sc <- spark_connect(master = "local", version = "2.1.0", config = list(sparklyr.log.console = TRUE))

# Configurations can be made using the config argument in its spark_connect() function.

# Data preprataion - install dplyr package (within R Studio)
install.packages(dplyr)
# run the package dplyr
library(dplyr)

# Reading and converting files (HTML) to R Data Frame format
# Content of files such as HTML are by tags  <tag>content</tag>, such tags can be identified in the case of a table content by html_nodes(): 

install.packages(rvest)
# run the package rvest
library(rvest)
stock.trend.html <- read_html("https://finance.yahoo.com/quote/AAPL/history?p=AAPL")
head(stock.trend.html)
stock.trend.table <- html_nodes(stock.trend.html, "table")
head(stock.trend.table)
str(stock.trend.table)

stock.trend.table <- stock.trend.html %>%
  html_nodes("table") %>%
  .[1:1] %>%
  html_table(fill = TRUE)
str(stock.trend.table)
stock.trend.df <- data.frame(stock.trend.table)
str(stock.trend.df)
head(stock.trend.df)
names(stock.trend.df)

# Convert data to numeric values

# Date conversion in R using different codes below
# Code	Value
# %d	Day of the month (decimal number)
# %m	Month (decimal number)
# %b	Month (abbreviated)
# %B	Month (full name)
# %y	Year (2 digit)
# %Y	Year (4 digit)
stock.trend.df$Date <- as.Date(stock.trend.df$Date, format = "%b %d, %Y")
stock.trend.df$Open <- as.numeric(stock.trend.df$Open)
stock.trend.df$High <- as.numeric(stock.trend.df$High)
stock.trend.df$Low <- as.numeric(stock.trend.df$Low)
stock.trend.df$Close.  <- as.numeric(stock.trend.df$Close.)    
stock.trend.df$Adj.Close.. <- as.numeric(stock.trend.df$Adj.Close..)
stock.trend.df$Volume  <-  as.numeric(stock.trend.df$Volume)      

# Display plots
plot(stock.trend.df$Date, stock.trend.df$Open, col='white', type='o')
# Add lines to connect the dots
lines(stock.trend.df$Date, stock.trend.df$High , col='blue')
lines(stock.trend.df$Date, stock.trend.df$Low, col='green')
lines(stock.trend.df$Date, stock.trend.df$Adj.Close.., col='red')

# Save extracted html data as data frame CSV and TXT 
write.csv(stock.trend.df,'stock.trend.df.csv')
write.table(stock.trend.df, "stock.trend.df.txt", sep="\t")




