# Functions for turning event log into a data frame of traces

# Get event names
getEvents <- function(w, events = "event") {
  unique(w[,events])
}

# Get ids
getIds <- function(w, ids = "id") {
  unique(w[,ids])
}

# Sort event log by time stamp
sortLog <- function(w, timestamps = "timestamp") {
  w[order(w[[timestamps]]),]
}

# Get the trace corresponding to id from a *time sorted* event log
getTrace <- function(w, id, ids = "id", events = "event") {
  tr <- w[w[[ids]] == id,]
  as.character(tr[[events]])
}

# Get data frame of traces
getAllTraces <- function(w, ids = "id", events = "event", timestamps = "timestamp") {
  id_list <- getIds(w, ids)
  w <- sortLog(w, timestamps)
  traces <- mapply(getTrace, list(w), id_list, ids, events)
  # Hack to insert list of vectors into data frame
  allTraces <- data.frame(id = as.factor(id_list), trace = NA)
  allTraces$trace <- traces
  allTraces
}

# Make hash function for list of event names
eventHash <- function(event_list) {
  num_events <- length(event_list)
  hash_df <- as.data.frame(matrix(nrow = 1, ncol = num_events, 1:num_events))
  colnames(hash_df) <- event_list
  # Return the hashing function (a closure)
  function(event) {
    hash_df[[event]]
  }
}
