require(dplyr)

# Get dictionary of event names
getDictionary <- function(w, events = "event") {
  unique(w[,events])
}

# Sort event log by time stamp
sortLog <- function(w, timestamps = "timestamp") {
  w[order(w[[timestamps]]),]
}

# Get the trace corresponding to id from a *time sorted* event log
getTrace <- function(w, id, ids = "id", events = "event") {
  tr <- w[w[[ids]] == id,]
  list(trace = tr[[events]])
}

# Get list of traces
getAllTraces <- function(w, ids = "id", events = "event", timestamps = "timestamp") {
  dict <- getDictionary(w, events)
  w <- sortLog(w, timestamps)
  traces <- mapply(getTrace, list(w), dict, ids, events)
  allTraces <- data.frame(id = as.factor(dict), trace = 0)
  allTraces$trace <- traces
  allTraces
}
