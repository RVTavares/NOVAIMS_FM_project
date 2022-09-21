# NOVAIMS_FM_project


#FORECASTING METHODS PROJECT
#TOPIC: FORECASTING CABO VERDE´S EMIGRANTS REMITTANCES USING MONTHLY DATA FROM JAN2002 TO JUN2022

#Steps

#1.First, istall important packages

install.packages("RCurl")
install.packages("XML")
install.packages("readxl")
install.packages("dplyr")
install.packages("forecast")
install.packages("zoo")

#2.Load theses packages

library(RCurl)
library(XML)
library(readxl)
library(dplyr)
library(forecast)
library(zoo)
library(fpp)
library(tseries)
#library(help="tseries")

#3.Import data from GitHub and transform it.

url <-"https://raw.githubusercontent.com/RVTavares/NOVAIMS_FM_project/main/remittance_data.csv"
data.raw<-read.csv(url, header = TRUE, sep = ";")
colnames(data.raw) <- c("Date","REM") ##Remane the columns#
data.raw$Date <- as.Date(data.raw$Date, format = "%d/%m/%Y") ##Convert date##
data.raw$REM <-gsub(" ","", data.raw$REM) ##remove space##
data.raw$REM <- as.numeric(gsub(",",".",data.raw$REM)) ##to replace comma "," by poin "."##
data.raw

#OBS: if we want see our time series, any time we can tipe "data.raw" and ctr+enter

#4. To Create a time series date

full.ts <- data.frame("Date" = seq(from = as.Date('2002-01-01'),to = as.Date('2022-06-01'),by = "month"))
full.ts
data.ts <- full.ts %>% 
  left_join(data.raw, by = c("Date")) %>%
  na.locf(fromLast = TRUE)

ts <- ts(data.ts$REM)
ts

ts <- ts(data.ts$REM, 
         start = c(2002,01),
         end = c(2022,06),
         frequency = 12)
ts
plot(ts)

#5 time series is ready. Let's analyse it
#6
acf(ts)
pacf(ts)
decompose(ts)
plot(decompose(ts))

#7
auto.arima(ts)

ts2 <- diff(ts)
plot(ts2)
tail(ts2
