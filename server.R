#################################################################################
#### Author: Fedor Kolyadin
#### Date: April, 2015
#### Description: An R implementation of the (multiple) Support Vector Machine 
#### Recursive Feature Elimination (mSVM-RFE) feature ranking algorithm
#################################################################################


library(RPostgreSQL)
library(DBI)
library(xts)
library(forecast)
library(lubridate)
library(reshape2)
library(xlsx)
library(rCharts)

#############################
## Temp loading data
load('gdp.RData')
load('hpi.RData')
load('unrate.RData')
load('gdp_adverse.RData')
load('hpi_adverse.RData')
load('unrate_adverse.RData')
load('gdp_base.RData')
load('hpi_base.RData')
load('unrate_base.RData')
#############################

drv = dbDriver("PostgreSQL") # Driver to connect to PostgreSQL
# hashed1 <- scrypt::hashPassword("xxx")
# scrypt::verifyPassword(hashed1, "xxx")

con = dbConnect(drv, user = "xxx", password = "xxx", dbname = "QTRACK", host = "xxx", port = 5432)

#################################################################################
## Data Prep
## get data from macro

gdp <- dbGetQuery (con, "select date, gdp_nat_log from macro ")
hpi <- dbGetQuery (con, "select date, hpi_nat_log from macro ")
unrate <- dbGetQuery (con, "select date, unrate_nat_log from macro ")

save(gdp, file='gdp.RData')
save(hpi, file='hpi.RData')
save(unrate, file='unrate.RData')

## get data from macro_base

gdp_base <- dbGetQuery (con, "select date, gdp_nat_log from macro_base ")
hpi_base <- dbGetQuery (con, "select date, hpi_nat_log from macro_base ")
unrate_base <- dbGetQuery (con, "select date, unrate_nat_log from macro_base ")

save(gdp_base, file='gdp_base.RData')
save(hpi_base, file='hpi_base.RData')
save(unrate_base, file='unrate_base.RData')

## get data from macro_adverse

gdp_adverse <- dbGetQuery (con, "select date, gdp_nat_log from macro_adverse ")
hpi_adverse <- dbGetQuery (con, "select date, hpi_nat_log from macro_adverse ")
unrate_adverse <- dbGetQuery (con, "select date, unrate_nat_log from macro_adverse ")

save(gdp_adverse, file='gdp_adverse.RData')
save(hpi_adverse, file='hpi_adverse.RData')
save(unrate_adverse, file='unrate_adverse.RData')
# mydata$Date <- as.Date(as.character(mydata$Date, format = "%Y-%m-%d"))
# gdp <- unique(mydata[c(1,2)])
# hpi <- unique(mydata[c(1,3)])
# unrate <- unique(mydata[c(1,4)])

## macro time series
gdpXts <- xts(gdp$gdp_nat_log, gdp$date)
gdpXts <- to.yearly(gdpXts)
gdpSeries <- as.ts(gdpXts[,4], start = c(1976))

hpiXts <- xts(hpi$hpi_nat_log, hpi$date)
hpiXts <- to.yearly(hpiXts)
hpiSeries <- as.ts(hpiXts[,4], start = c(1976))

unrateXts <- xts(unrate$unrate_nat_log, unrate$date)
unrateXts <- to.yearly(unrateXts)
unrateSeries <- as.ts(unrateXts[,4], start = c(1976))

## macro_base time series
gdpXts_base <- xts(gdp_base$gdp_nat_log, gdp_base$date)
gdpXts_base <- to.yearly(gdpXts_base)
gdpSeries_base <- as.ts(gdpXts_base[,4], start = c(1976))

hpiXts_base <- xts(hpi_base$hpi_nat_log, hpi_base$date)
hpiXts_base <- to.yearly(hpiXts_base)
hpiSeries_base <- as.ts(hpiXts_base[,4], start = c(1976))

unrateXts_base <- xts(unrate_base$unrate_nat_log, unrate_base$date)
unrateXts_base <- to.yearly(unrateXts_base)
unrateSeries_base <- as.ts(unrateXts_base[,4], start = c(1976))

## macro_adverse time series
gdpXts_adverse <- xts(gdp_adverse$gdp_nat_log, gdp_adverse$date)
gdpXts_adverse <- to.yearly(gdpXts_adverse)
gdpSeries_adverse <- as.ts(gdpXts_adverse[,4], start = c(1976))

hpiXts_adverse <- xts(hpi_adverse$hpi_nat_log, hpi_adverse$date)
hpiXts_adverse <- to.yearly(hpiXts_adverse)
hpiSeries_adverse <- as.ts(hpiXts_adverse[,4], start = c(1976))

unrateXts_adverse <- xts(unrate_adverse$unrate_nat_log, unrate_adverse$date)
unrateXts_adverse <- to.yearly(unrateXts_adverse)
unrateSeries_adverse <- as.ts(unrateXts_adverse[,4], start = c(1976))

data_sets <- c("GDP", "HPI", "UNRATE", "GDP_base", "HPI_base", "UNRATE_base", "GDP_adverse", "HPI_adverse", "UNRATE_adverse" )

#################################################################################
## Fit data to ARIMA model
## macro fit

fit_gdp <- auto.arima(gdpSeries)
fcast_gdp <- forecast(fit_gdp)

fit_hpi <- auto.arima(hpiSeries)
fcast_hpi <- forecast(fit_hpi)

fit_unrate <- auto.arima(unrateSeries)
fcast_unrate <- forecast(fit_unrate)

## macro_base fit
fit_gdp_base <- auto.arima(gdpSeries_base)
fcast_gdp_base <- forecast(fit_gdp_base)

fit_hpi_base <- auto.arima(hpiSeries_base)
fcast_hpi_base <- forecast(fit_hpi_base)

fit_unrate_base <- auto.arima(unrateSeries_base)
fcast_unrate_base <- forecast(fit_unrate_base)

## macro_adverse fit
fit_gdp_adverse <- auto.arima(gdpSeries_adverse)
fcast_gdp_adverse <- forecast(fit_gdp_adverse)

fit_hpi_adverse <- auto.arima(hpiSeries_adverse)
fcast_hpi_adverse <- forecast(fit_hpi_adverse)

fit_unrate_adverse <- auto.arima(unrateSeries_adverse)
fcast_unrate_adverse <- forecast(fit_unrate_adverse)

## The following code takes the input from the UI.r to display the prediction
shinyServer(function(input, output) {
    
    datasetInput <- reactive({
      switch(input$dataset,
             "GDP" = gdpSeries,
             "HPI" = hpiSeries,
             "UNRATE" = unrateSeries,
             "GDP_base" = gdpSeries_base, 
             "HPI_base" = hpiSeries_base, 
             "UNRATE_base" = unrateSeries_base, 
             "GDP_adverse"= gdpSeries_adverse, 
             "HPI_adverse" = hpiSeries_adverse, 
             "UNRATE_adverse" = unrateSeries_adverse)
    })
    
    output$plot_gdp <- renderPlot({ plot(fcast_gdp, ylab='GDP (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'orangered')})    
    output$plot_hpi <- renderPlot({ plot(fcast_hpi, ylab='HPI (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'gold')})
    output$plot_unrate <- renderPlot({ plot(fcast_unrate, ylab='UNRATE (log)',xlab='Year',
                                        pch=20, lty=1,type='o', col = 'springgreen')})
    
    output$plot_gdp_base <- renderPlot({ plot(fcast_gdp_base, ylab='GDP Base (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'springgreen1')})    
    output$plot_hpi_base <- renderPlot({ plot(fcast_hpi_base, ylab='HPI Base (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'deepskyblue2')})
    output$plot_unrate_base <- renderPlot({ plot(fcast_unrate_base, ylab='UNRATE Base (log)',xlab='Year',
                                            pch=20, lty=1,type='o', col = 'violetred1')})
    
    output$plot_gdp_adverse <- renderPlot({ plot(fcast_gdp_adverse, ylab='GDP Adverse (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'purple3')})    
    output$plot_hpi_adverse <- renderPlot({ plot(fcast_hpi_adverse, ylab='HPI Adverse (log)',xlab='Year',
                                         pch=20, lty=1,type='o', col = 'maroon')})
    output$plot_unrate_adverse <- renderPlot({ plot(fcast_unrate_adverse, ylab='UNRATE Adverse (log)',xlab='Year',
                                            pch=20, lty=1,type='o', col = 'yellow1')})
    
    output$Fcast_gdp <- renderDataTable({Fcast_gdp <- as.data.frame(fcast_gdp)
                                      Fcast_gdp})
    
    output$Fcast_hpi <- renderDataTable({Fcast_hpi <- as.data.frame(fcast_hpi)
                                     Fcast_hpi})
    
    output$Fcast_unrate <- renderDataTable({Fcast_unrate <- as.data.frame(fcast_unrate)
                                     Fcast_unrate})
})


dbDisconnect(con)
