source("MixPanel.R")

m <- mixpanel(key = "your_key_here", secret = "your_secret_here")
res <- m$exportEvents(from = as.Date("2015-08-09"), to = as.Date("2015-08-11"))