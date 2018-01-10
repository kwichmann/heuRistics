source("logToTraces.R")
source("heuristics.R")

fake <- data.frame(id = sample(1:10, 1000, replace = T),
                   event = sample(LETTERS[1:8], 1000, replace = T),
                   timestamp = runif(1000) * 100)

a <- getAllTraces(fake)

b <- directSequenceMatrix(fake)

c <- directSequenceHeuristicsValue(b)

print(c)
