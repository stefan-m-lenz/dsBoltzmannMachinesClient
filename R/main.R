ds.monitored_fitrbm <- function(datasources, data, newobj = 'rbm',
                                monitoring = "reconstructionerror",
                                monitoringdata = NULL,
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
                                startrbm = NULL) {

   learningrates <- as.dsVectorArg(learningrates)
   monitoringdata <- as.dsVectorArg(monitoringdata)

   cally <- call('monitored_fitrbmDS', newobj = newobj, data = data,
                 monitoring = monitoring,
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


ds.splitdata <- function(datasources, data, ratio, newobj1, newobj2) {
   cally <- call("splitdataDS", data, ratio, newobj1, newobj2)
   datashield.aggregate(datasources, cally)
}


ds.setJuliaSeed <- function(datasources, seed) {
   cally <- call("setBoltzmannSeedDS", seed)
   datashield.aggregate(datasources, cally)
}


ds.samples <- function(datasources, bm, nsamples,
                       burnin = NULL,
                       conditionIndex = NULL,
                       conditionValue = NULL,
                       samplelast = NULL) {

   cally <- call("samplesDS", bm, nsamples,
                 burnin = burnin,
                 conditionIndex = conditionIndex,
                 conditionValue = conditionValue,
                 samplelast = samplelast)
   datashield.aggregate(datasources, cally)
}


ds.defineLayer <- function(datasources, newobj,
                           epochs = NULL,
                           learningrate = NULL,
                           learningrates = NULL,
                           sdlearningrate = NULL,
                           sdlearningrates = NULL,
                           categories = NULL,
                           monitoring = NULL,
                           rbmtype = NULL,
                           nhidden = NULL,
                           nvisible = NULL,
                           batchsize = NULL,
                           pcd = NULL,
                           cdsteps = NULL,
                           startrbm = NULL) {

   learningrates <- as.dsVectorArg(learningrates)
   sdlearningrates <- as.dsVectorArg(sdlearningrates)
   nhidden <- as.dsVectorArg(nhidden)
   nvisible <- as.dsVectorArg(nvisible)
   categories <- as.dsVectorArg(categories)

   cally <- call("defineLayerDS",
                 epochs = epochs,
                 learningrate = learningrate,
                 learningrates = learningrates,
                 sdlearningrate = sdlearningrate,
                 sdlearningrates = sdlearningrates,
                 categories = categories,
                 monitoring = monitoring,
                 rbmtype = rbmtype,
                 nhidden = nhidden,
                 nvisible = nvisible,
                 batchsize = batchsize,
                 pcd = pcd,
                 cdsteps = cdsteps,
                 startrbm = startrbm)

   datashield.assign(datasources, newobj, cally)
}
