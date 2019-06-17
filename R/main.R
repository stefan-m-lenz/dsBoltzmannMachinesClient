ds.monitored_fitrbm <- function(datasources, x, ...) {
   # see https://stackoverflow.com/questions/33288387/r-using-the-ellipsis-in-a-call
   cally <- call("monitored_fitrbmDS", as.symbol(x), substitute(...()))

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}
