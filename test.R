source("logToTraces.R")

fake <- data.frame(id = sample(1:10, 1000, replace = T),
                   event = sample(1:5, 1000, replace = T),
                   timestamp = runif(1000) * 100)

fake <- sortLog(fake)

View(getAllTraces(fake))
