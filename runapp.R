#################################################################################
#### Author: Fedor Kolyadin
#### Date: April, 2015
#### Description: An R implementation of the (multiple) Support Vector Machine 
#### Recursive Feature Elimination (mSVM-RFE) feature ranking algorithm
#### NOTE: update project location in setwd
#################################################################################

# Uncomment install packages if running first time and need to install them
# install.packages("shiny")
# install.packages("xts")
# install.packages("forecast")
# install.packages("lubridate")
# install.packages("reshape2")
# install.packages("xlsx")
# install.packages("RPostgreSQL")
# install.packages("DBI")
# devtools::install_github("ramnathv/rCharts")

library(RPostgreSQL)
library(DBI)
library(xts)
library(forecast)
library(lubridate)
library(reshape2)
library(xlsx)
library(rCharts)

x<-setwd("/xxx/")
shinyAppDir(x)
runApp("ARIMA_gdp")
