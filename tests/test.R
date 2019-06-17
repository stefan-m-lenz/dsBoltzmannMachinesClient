logindata <- data.frame(server = "server",
                        url = "http://10.5.10.57:8080",
                        user = "user",
                        password = "password",
                        table ="50bin.x")
o <- datashield.login(logins = logindata, assign = TRUE)
ds.monitored_fitrbm(o, "D")
datashield.logout(o)
