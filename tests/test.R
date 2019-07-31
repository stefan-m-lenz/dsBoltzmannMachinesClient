library(BoltzmannMachinesRPlots)

library(dsBoltzmannMachinesClient)
logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")

o <- datashield.login(logins = logindata, assign = TRUE)
ds.monitored_fitdbm(o, data ="D", epochs = 2) # TODO test with more arguments
result <- ds.monitored_fitdbm(o, data ="D", epochs = 10, epochspretraining = 20)
ds.samples(o, bm = "dbm", nsamples = 5, conditionIndex = c(1,2), conditionValue=c(1,1))
ds.defineLayer(o, newobj = "layer1", nhidden = 5, epochs = 20)
ds.defineLayer(o, newobj = "layer2", nhidden = 4, epochs = 10)
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
ds.setJuliaSeed(o, 5)
result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"),
                              monitoring = "loglikelihood",
                              nhidden = 2,
                              learningrate = 0.005, epochs = 25) # TODO change plots for likelihood

ds.samples(datasources = o, bm = "rbm", nsamples = 5, burnin = 100)

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
datashield.logout(o)
