ds.monitored_fitrbm <- function(datasources, data, newobj = 'rbm',
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
                                rbmtype = NULL,# TODO
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
