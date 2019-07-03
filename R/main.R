ds.monitored_fitrbm <- function(datasources, data, newobj = 'rbm', ...) {
   # see https://stackoverflow.com/questions/33288387/r-using-the-ellipsis-in-a-call
   cally <- call('monitored_fitrbmDS', newobj = newobj, data = data)
   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}
