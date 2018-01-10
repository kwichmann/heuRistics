source("logToTraces.R")

fake <- data.frame(id = sample(1:10, 1000, replace = T),
                   event = sample(LETTERS, 1000, replace = T),
                   timestamp = runif(1000) * 100)

a <- getAllTraces(fake)

View(a)
