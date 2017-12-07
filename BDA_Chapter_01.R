# This is part of a series of scripts to use R and Spark combined  
# Chapter 01 - To get acquainted with R and Spark pltaform 
# Use the case of extracting and transforming HTML data into a table or data frame
# Stock market data - Apple stock market trends 

# Strating by setting a working directory 
setwd()
getwd()
# Reading and conveting files (HTML) to R Data Frame format
# Content of files such as HTML are by tags  <tag>content</tag>, such tags can be identifed in the case of a table coentent by html_nodes(): 

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
str(stock.trend.df)
stock.trend.df <- data.frame(stock.trend.table)
str(stock.trend.df)
head(stock.trend.df)
names(stock.trend.df)

stock.trend.df$Date <- as.Date(stock.trend.df$Date, format = "%b %d, %Y")
stock.trend.df$Open <- as.numeric(stock.trend.df$Open)
stock.trend.df$High <- as.numeric(stock.trend.df$High)
stock.trend.df$Low <- as.numeric(stock.trend.df$Low)
stock.trend.df$Close.  <- as.numeric(stock.trend.df$Close.)    
stock.trend.df$Adj.Close.. <- as.numeric(stock.trend.df$Adj.Close..)
stock.trend.df$Volume  <-  as.numeric(stock.trend.df$Volume)      


# Date conversion in R 
# Code	Value
# %d	Day of the month (decimal number)
# %m	Month (decimal number)
# %b	Month (abbreviated)
# %B	Month (full name)
# %y	Year (2 digit)
# %Y	Year (4 digit)

stock.trend.df <- na.omit(stock.trend.df)
str(stock.trend.df)
head(stock.trend.df)
names(stock.trend.df)

write.csv(stock.trend.df,'stock.trend.df.csv')
write.table(stock.trend.df, "stock.trend.df.txt", sep="\t")
