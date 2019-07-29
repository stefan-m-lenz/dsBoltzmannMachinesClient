library(opal)

library(BoltzmannMachinesRPlots)

library(dsBoltzmannMachinesClient)
logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")

o <- datashield.login(logins = logindata, assign = TRUE)
ds.monitored_fitdbm(o, data ="D", epochs = 10)
datashield.logout(o)

ds.splitdata(o, "D", 0.1, "D.Train", "D.Test")
ds.defineLayer(o, newobj = "t1", nhidden = 1, epochs = 5)
result <- ds.monitored_fitrbm(o, data = "D.Train", monitoringdata = "D.Test", learningrate = 0.001, epochs = 2) # fast test
ds.splitdata(o, "D.Train", 0.11, "D.Train2", "D.Test2")
result <- ds.monitored_fitrbm(o, data = "D.Train2", monitoring = "reconstructionerror",
                              monitoringdata = c("D.Test", "D.Test2"), learningrate = 0.001, epochs = 2)
plotEvaluation(result[[1]])

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
plotEvaluation(result[[1]][[2]])
result <- ds.monitored_stackrbms(o, nhiddens = c(6,5,4), epochs = 15, predbm = TRUE, learningrate = 0.01, batchsize = 5)
plotEvaluation(result[[1]][[3]])
result <- ds.monitored_stackrbms(o, monitoring = NULL)
result <- ds.monitored_stackrbms(o, monitoring = c("reconstructionerror", "loglikelihood"), nhiddens = c(5,2))
plotEvaluation(result[[1]][[1]])


ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10)
ds.monitored_fitrbm(o, data = "D", epochs = 10, newobj = "rbm1")
ds.monitored_fitrbm(o, data = "D", startrbm = "rbm1")





result <- ds.monitored_fitrbm(o, data = "D", epochs = 100, batchsize = 10, pcd = FALSE)
plotEvaluation(result[[1]])

