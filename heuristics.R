# Functions for calculating matrices relevant to heuristics mining

# Calculate direct sequence matrix
directSequenceMatrix <- function(w, ids = "id", events = "event", timestamps = "timestamp") {
  # Make data frame of traces
  traces <- getAllTraces(w, ids, events, timestamps)
  # Make hash function for event names
  event_list <- getEvents(w, events)
  event_hash <- eventHash(event_list)
  # Initialize matrix with zeroes
  matrixDimension <- length(event_list)
  dsm <- matrix(0, nrow = matrixDimension, ncol = matrixDimension)
  for (trace in traces$trace) {
    for (i in 1:(length(trace) - 1)) {
      # Use event hash function to translate to numbers
      from <- event_hash(trace[i])
      to <- event_hash(trace[i + 1])
      dsm[from, to] <- dsm[from, to] + 1
    }
  }
  dsm
}
