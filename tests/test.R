library(opal)
library(dsBoltzmannMachinesClient)
library(BoltzmannMachinesRPlots)
logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")
o <- datashield.login(logins = logindata, assign = TRUE)
ds.monitored_fitrbm(o, data = "D", learningrate = 0.001)
ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10)
ds.monitored_fitrbm(o, data = "D", learningrates = rep(0.001, 10), epochs = 10, batchsize = 10, pcd = FALSE)
datashield.logout(o)
