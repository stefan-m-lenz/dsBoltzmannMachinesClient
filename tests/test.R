library(dsBoltzmannMachinesClient)

result <- ds.monitored_fitdbm(o, data ="D", epochs = 2,nhiddens = c(2,2))
ds.setJuliaSeed(o, 1) # for reproducibility
result <- ds.monitored_fitdbm(datasources = o, data ="D", nhiddens = c(50, 25, 15),
                              epochs = 10,
                              epochspretraining = 20)

plotMonitoring(result)
comps <- ds.dbm.top2LatentDims(o)
plot(comps[[1]][,1], comps[[1]][,2])

ds.dbm.samples(o, nsamples = 5, conditionIndex = c(1,2), conditionValue=c(1,1))
ds.bm.defineLayer(o, newobj = "layer1", nhidden = 5, epochs = 20)
ds.bm.defineLayer(o, newobj = "layer2", nhidden = 4, epochs = 10)
result <- ds.monitored_fitdbm(o, pretraining = c("layer1", "layer2"), epochs = 21)

ds.bm.defineLayer(o, "layer1_1", nhidden = 10, nvisible = 20)
ds.bm.defineLayer(o, "layer1_2", nhidden = 10, nvisible = 30)
ds.bm.definePartitionedLayer(o, "layer1", c("layer1_1", "layer1_2"))
ds.bm.defineLayer(o, "layer2", nhidden = 10, nvisible = 20)
result <- ds.monitored_fitdbm(o, pretraining = c("layer1", "layer2"), epochs = 21)
plotMonitoring(result)


ds.splitdata(o, "D", 0.1, "D.Train", "D.Test")

result <- ds.monitored_fitrbm(o, data = "D.Train", monitoringdata = "D.Test", learningrate = 0.001, epochs = 2) # fast test
ds.splitdata(o, "D.Train", 0.11, "D.Train2", "D.Test2")
result <- ds.monitored_fitrbm(o, data = "D.Train2", monitoring = "reconstructionerror",
                              monitoringdata = c("D.Test", "D.Test2"), learningrate = 0.001, epochs = 2)
plotMonitoring(result)

result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"), learningrate = 0.001)

result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"),
                              monitoring = "exactloglikelihood",
                              nhidden = 2, learningrate = 0.005, epochs = 25)
ds.rbm.samples(o, nsamples = 5)
ds.setJuliaSeed(o, 5)
result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"),
                              monitoring = "loglikelihood",
                              nhidden = 2,
                              learningrate = 0.005, epochs = 25) # TODO change plots for likelihood

ds.bm.samples(datasources = o, bm = "rbm", nsamples = 5, burnin = 100)

result <- ds.monitored_fitrbm(o, data = "D.Train", monitoring = NULL)


result <- ds.monitored_stackrbms(o)
plotMonitoring(result)
result <- ds.monitored_stackrbms(o, nhiddens = c(6,5,4), epochs = 15, predbm = TRUE, learningrate = 0.01, batchsize = 5)
plotMonitoring(result)
result <- ds.monitored_stackrbms(o, monitoring = NULL)
result <- ds.monitored_stackrbms(o, monitoring = c("reconstructionerror", "loglikelihood"), nhiddens = c(5,2))
plotMonitoring(result)


ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10)
ds.monitored_fitrbm(o, data = "D", epochs = 10, newobj = "rbm1")
ds.monitored_fitrbm(o, data = "D", startrbm = "rbm1")





result <- ds.monitored_fitrbm(o, data = "D", epochs = 100, batchsize = 10, pcd = FALSE)
plotMonitoring(result)

# Test likelihood
library(dsBaseClient)
# o <- datashield.login(logins = data.frame(server = "server",
#                                           url = "http://10.5.10.57:8080",
#                                           user = "user",
#                                           password = "password",
#                                           table ="50bin.x"),
#                       assign = TRUE)
ds.subset("D", subset = "first5", cols = 1:5)
ds.splitdata(o, "first5", 0.1, "D.Train", "D.Test")
ds.monitored_fitdbm(o, data = "first5", nhiddens = c(2,2))

result <- ds.monitored_traindbm(o, data = "first5", learningrate = 0.01, epochs = 15)
plotMonitoring(result)

ds.dbm.exactloglikelihood(o, data = "D.Test")
ds.dbm.logproblowerbound(o, data = "D.Test")
ds.dbm.logproblowerbound(o, data = "D.Test", nparticles = 50, burnin = 10, ntemperatures = 50, parallelized = TRUE)
ds.dbm.loglikelihood(o, data = "D.Test")
ds.dbm.loglikelihood(o, data = "D.Test", nparticles = 50, burnin = 10, ntemperatures = 50, parallelized = TRUE)

ds.monitored_fitrbm(o, data = "first5", nhidden = 2)
ds.rbm.exactloglikelihood(o, data = "D.Test")
ds.rbm.loglikelihood(o, data = "D.Test")
ds.rbm.loglikelihood(o, data = "D.Test", nparticles = 50, burnin = 10, ntemperatures = 50, parallelized = TRUE)


# TODO data preprocessing for Softmax0BernoulliRBM
ds.bm.defineLayer(o, "layer1", nhidden = 4, rbmtype = "Softmax0BernoulliRBM", categories = 2)
ds.bm.defineLayer(o, "layer2", nhidden = 4)
ds.monitored_fitdbm(o, pretraining = c("layer1", "layer2"))

ds.monitored_fitrbm(o, nhidden = 4, rbmtype = "Softmax0BernoulliRBM", categories = 2)

datashield.logout(o)
