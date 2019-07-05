ds.monitored_fitrbm <- function(datasources, data, newobj = 'rbm',
                                monitoringdata = NULL, # TODO add labels, choose monitoring function
                                # keyword arguments for fitrbm
                                nhidden = NULL,
                                epochs = NULL,
                                upfactor = NULL,
                                downfactor = NULL,
                                learningrate = NULL,
                                learningrates = NULL,
                                pcd = NULL,
                                cdsteps = NULL,
                                batchsize = NULL,
                                rbmtype = NULL,
                                startrbm = NULL)
{
   if (!is.null(learningrates)) {
      learningrates <- paste0(as.character(learningrates), collapse = ",")
   }

   cally <- call('monitored_fitrbmDS', newobj = newobj, data = data,
                 monitoringdata = monitoringdata,
                 # keyword arguments for fitrbm
                 # (The  order is important (must be same as in server package)
                 # -  Bug in Opal?)
                 nhidden = nhidden,
                 epochs = epochs,
                 upfactor = upfactor,
                 downfactor = downfactor,
                 learningrate = learningrate,
                 learningrates = learningrates,
                 pcd = pcd,
                 cdsteps = cdsteps,
                 batchsize = batchsize,
                 rbmtype = rbmtype,
                 startrbm = startrbm)

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}

ds.splitdata <- function(data, ratio, newobj1, newobj2) {
   cally <- call("splitdataDS", data, ratio, newobj1, newobj2)
   datashield.aggregate(datasources, cally)
}

ds.setBoltzmannSeed <- function(seed) {
   cally <- call("setBoltzmannSeedDS", seed)
   datashield.aggregate(datasources, cally)
}
