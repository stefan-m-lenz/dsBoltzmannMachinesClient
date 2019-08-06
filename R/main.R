asDSVectorArg <- function (x) {
   if (!is.null(x)) {
      x <- paste0(as.character(x), collapse = ",")
   }
   return(x)
}


ds.monitored_fitrbm <- function(datasources, data = "D", newobj = 'rbm',
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

   cally <- call('monitored_fitrbmDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 # keyword arguments for fitrbm
                 # (The  order is important (must be same as in server package)
                 # -  Bug in Opal?)
                 nhidden = nhidden,
                 epochs = epochs,
                 upfactor = upfactor,
                 downfactor = downfactor,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 pcd = pcd,
                 cdsteps = cdsteps,
                 batchsize = batchsize,
                 rbmtype = rbmtype,
                 startrbm = startrbm)

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}


ds.monitored_stackrbms <- function(datasources, data = "D", newobj = 'rbmstack',
                                   monitoring = "reconstructionerror",
                                   monitoringdata = NULL,
                                   # keyword arguments for stackrbms
                                   nhiddens = NULL,
                                   epochs = NULL,
                                   predbm = NULL,
                                   samplehidden = NULL,
                                   learningrate = NULL,
                                   batchsize = NULL,
                                   trainlayers = NULL) {

   cally <- call('monitored_stackrbmsDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 # keyword arguments for stackrbms
                 nhiddens = asDSVectorArg(nhiddens),
                 epochs = epochs,
                 predbm = predbm,
                 samplehidden = samplehidden,
                 learningrate = learningrate,
                 batchsize = batchsize,
                 trainlayers = asDSVectorArg(trainlayers))

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}


ds.monitored_fitdbm <- function(datasources, newobj = "dbm", data = "D",
                                monitoring = "logproblowerbound",
                                monitoringdata = data,
                                monitoringpretraining = "reconstructionerror",
                                monitoringdatapretraining = monitoringdata,
                                nhiddens = NULL,
                                epochs = NULL,
                                nparticles = NULL,
                                learningrate = NULL,
                                learningrates = NULL,
                                learningratepretraining = NULL,
                                epochspretraining = NULL,
                                batchsizepretraining = NULL,
                                pretraining = NULL) {

   cally <- call('monitored_fitdbmDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 monitoringpretraining = asDSVectorArg(monitoringpretraining),
                 monitoringdatapretraining = asDSVectorArg(monitoringdatapretraining),
                 # keyword arguments for fitdbm
                 nhiddens = asDSVectorArg(nhiddens),
                 epochs = epochs,
                 nparticles = nparticles,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 learningratepretraining = learningratepretraining,
                 epochspretraining = epochspretraining,
                 batchsizepretraining = batchsizepretraining,
                 pretraining = asDSVectorArg(pretraining))

   monitoringoutput <- datashield.aggregate(datasources, cally)
   return(monitoringoutput)
}


ds.splitdata <- function(datasources, data, ratio, newobj1, newobj2) {
   cally <- call("splitdataDS", data, ratio, newobj1, newobj2)
   datashield.aggregate(datasources, cally)
   invisible()
}


ds.setJuliaSeed <- function(datasources, seed) {
   cally <- call("setJuliaSeedDS", seed)
   datashield.aggregate(datasources, cally)
   invisible()
}


ds.samples <- function(datasources, bm, nsamples,
                       burnin = NULL,
                       conditionIndex = NULL,
                       conditionValue = NULL,
                       samplelast = NULL) {

   cally <- call("samplesDS", bm, nsamples,
                 burnin = burnin,
                 conditionIndex = asDSVectorArg(conditionIndex),
                 conditionValue = asDSVectorArg(conditionValue),
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

   cally <- call("defineLayerDS", newobj,
                 epochs = epochs,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 sdlearningrate = sdlearningrate,
                 sdlearningrates = asDSVectorArg(sdlearningrates),
                 categories = asDSVectorArg(categories),
                 rbmtype = rbmtype,
                 nhidden = asDSVectorArg(nhidden),
                 nvisible = asDSVectorArg(nvisible),
                 batchsize = batchsize,
                 pcd = pcd,
                 cdsteps = cdsteps,
                 startrbm = startrbm)

   datashield.aggregate(datasources, cally)
   invisible()
}


ds.definePartitionedLayer <- function (datasources, newobj, parts) {
   parts <- asDSVectorArg(parts)
   cally <- call("definePartitionedLayerDS", newobj, parts)
   datashield.aggregate(datasources, cally)
   invisible()
}


ds.dbm2TopLatentDims <- function(datasources, dbm = "dbm", data = "D") {
   cally <- call("dbm2TopLatentDimsDS", dbm, data)
   datashield.aggregate(datasources, cally)
}
