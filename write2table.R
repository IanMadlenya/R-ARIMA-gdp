# Write result to database
ScriptName = c("rpart_model_tree")
Date = c(Sys.Date())
TreeResult = c(rpk.text)
table = data.frame(ScriptName, Date, TreeResult)
dbWriteTable(con, name="tree_results", table, append = T)



id <- dbGetQuery(
  con, "select * from arima_result")+1

sql <- paste("INSERT INTO arima_result(id, scriptname, date, jsonresult)
             VALUES ('",id,"', 'rpart_model_tree','now()','",dat,"')")

dbGetQuery(con, sql)


x_gdp <- (rep("GDP", 10))
gdp_rt <- cbind(x_gdp, as.data.frame(fcast_gdp))

x_hpi <- (rep("HPI", 10))
hpi_rt <- cbind(x_hpi, as.data.frame(fcast_hpi))

x_unrate <- (rep("UNRATE", 10))
unrate_rt <- cbind(x_unrate, as.data.frame(fcast_unrate))

colnames(gdp_rt) <- colnames(hpi_rt) <- colnames(unrate_rt) <- 
  c("measure", "point_forecast", "lo_80", "hi_80", "lo_95", "hi_95")
rbind.data.frame(gdp_rt, hpi_rt, unrate_rt, make.row.names=TRUE)

rbind(gdp_rt, hpi_rt, by="row.names",all.x=TRUE)


##########################################################################
z <- matrix(c(0,0,1,1,0,0,1,1,0,0,0,0,1,0,1,1,0,1,1,1,1,0,0,0,"RND1","WDR", "PLAC8","TYBSA","GRA","TAF"), nrow=6,
            dimnames=list(c("ILMN_1651838","ILMN_1652371","ILMN_1652464","ILMN_1652952","ILMN_1653026","ILMN_1653103"),c("A","B","C","D","symbol")))

t<-matrix(c("GO:0002009", 8, 342, 1, 0.07, 0.679, 0, 0, 1, 0, 
            "GO:0030334", 6, 343, 1, 0.07, 0.065, 0, 0, 1, 0,
            "GO:0015674", 7, 350, 1, 0.07, 0.065, 1, 0, 0, 0), 
          nrow=10, dimnames= list(c("GO.ID","LEVEL","Annotated","Significant","Expected","resultFisher","ILMN_1652464","ILMN_1651838","ILMN_1711311","ILMN_1653026")))

cbind(t, z[, "symbol"][match(rownames(t), rownames(z))])

