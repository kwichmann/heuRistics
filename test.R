source("logToTraces.R")
source("heuristics.R")

fake <- data.frame(id = sample(1:10, 100, replace = T),
                   event = sample(LETTERS[1:8], 100, replace = T),
                   timestamp = runif(100) * 100)

a <- getAllTraces(fake)

b <- heuristicsMatrices(fake)

print(b)
