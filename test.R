source("logToTraces.R")
source("heuristics_matrices.R")

fake <- data.frame(id = sample(1:10, 100, replace = T),
                   event = sample(LETTERS[1:8], 100, replace = T),
                   timestamp = runif(100) * 100)

a <- getAllTraces(fake)

b <- heuristicsMatrices(fake)

dsm <- b[[1]]
tlm <- b[[2]]
ltrm <- b[[3]]

print("Direct sequence heuristics")
print(directSequenceHeuristicsValue(dsm))

print("One loop heuristics")
print(oneLoopHeursticsValue(dsm))

print("Two loop heuristics")
print(twoLoopHeursticsValue(tlm))

print("Long term dependence heuristics")
print(longTermHeuristicsValue(ltrm, dsm))
