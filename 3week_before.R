setwd("/Users/suyue/Desktop/")
data<-read.csv("trycode.csv", header = TRUE)
data$Date <- as.Date(data$Date, "%m/%d/%y")

#name roll name
rolldate <- c("7/22/19", "8/19/19")
rolldate <- as.Date(rolldate, "%m/%d/%y")
as.numeric(rolldate[2]-rolldate[1])
counter = 1

assignrollnr <- function(data, rolldate){
  rollnr <- double(dim(data)[1])
  counter = 0
  for (i in rolldate){
    counter <- counter + 1
    for(x in 1:dim(data)[1]){
      datadate <- data[x,1]
      rollnr[x]<-ifelse(datadate<i & datadate>i-21, counter, rollnr[x])
    }  }
  data$rollnr <- rollnr
  return(data)}

undebug(assignrollnr)
data <- assignrollnr(data, rolldate)

assignweeknr <- function(data, rolldate){
  weeknr <- double(dim(data)[1])
  for (i in rolldate){
    for(x in 1:dim(data)[1]){
      datadate <- data[x,1]
      weeknr[x]<-ifelse(datadate<i & datadate>i-8, -1, weeknr[x])
      weeknr[x]<-ifelse(datadate<i-7 & datadate>i-15, -2, weeknr[x])
      weeknr[x]<-ifelse(datadate<i-14 & datadate>i-22, -3, weeknr[x])
    }  }
  data$weeknr <- weeknr
  return(data)}

undebug(assignweeknr)

data <- assignweeknr(data, rolldate)
data$rollweek <- paste(data$rollnr, data$weeknr, sep = ":")
