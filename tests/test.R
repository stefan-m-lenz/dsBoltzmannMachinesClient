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
ds.monitored_fitrbm(o, data = "D", learningrate = 0.001)
ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10)
ds.monitored_fitrbm(o, data = "D", epochs = 10, newobj = "rbm1")
ds.monitored_fitrbm(o, data = "D", startrbm = "rbm1")



result <- ds.monitored_fitrbm(o, data = "D", epochs = 100, batchsize = 10, pcd = FALSE)
plotEvaluation(result[[1]][[1]])
datashield.logout(o)
