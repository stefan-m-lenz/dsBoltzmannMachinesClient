ds.monitored_fitrbm <- function(datasources, x, ...) {
   # see https://stackoverflow.com/questions/33288387/r-using-the-ellipsis-in-a-call
   expr <- c(as.symbol("monitored_fitrbmDS"), list(x), substitute(...()))
   names(expr)[2] <- "x"
   cally <- as.call(expr)

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}
