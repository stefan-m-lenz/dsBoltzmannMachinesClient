library(opal)
library(dsBoltzmannMachinesClient)
library(BoltzmannMachinesRPlots)
logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")
o <- datashield.login(logins = logindata, assign = TRUE)
plotEvaluation(ds.monitored_fitrbm(o, data = "D")[[1]])
datashield.logout(o)
