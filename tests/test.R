library(opal)

library(BoltzmannMachinesRPlots)

library(dsBoltzmannMachinesClient)
logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")
o <- datashield.login(logins = logindata, assign = TRUE)

ds.splitdata(o, "D", 0.1, "D.Train", "D.Test")
ds.defineLayer(o, newobj = "t1", nhidden = 1, epochs = 5)
result <- ds.monitored_fitrbm(o, data = "D.Train", monitoringdata = "D.Test", learningrate = 0.001, epochs = 2) # fast test

result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"), learningrate = 0.001)

result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"),
                              monitoring = "exactloglikelihood",
                              nhidden = 2, learningrate = 0.005, epochs = 25)

result <- ds.monitored_fitrbm(o, data = "D.Train",
                              monitoringdata = c("D.Train", "D.Test"),
                              monitoring = "loglikelihood",
                              nhidden = 2,
                              learningrate = 0.005, epochs = 25)

ds.samples(datasources = o, bm = "rbm", nsamples = 5, burnin = 100)

result <- ds.monitored_fitrbm(o, data = "D.Train", monitoring = NULL)

ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10)
ds.monitored_fitrbm(o, data = "D", epochs = 10, newobj = "rbm1")
ds.monitored_fitrbm(o, data = "D", startrbm = "rbm1")



result <- ds.monitored_fitrbm(o, data = "D", epochs = 100, batchsize = 10, pcd = FALSE)
plotEvaluation(result[[1]])
datashield.logout(o)
