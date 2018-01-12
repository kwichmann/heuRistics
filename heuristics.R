# Functions for calculating matrices relevant to heuristics mining

# Find the position of the first and last occurence of an element in a vector
firstLast <- function(vec, elt) {
  if (!(elt %in% vec)) {
    return(c(-1, -1))
  }
  elt_locations <- which(vec == elt)
  c(min(elt_locations), max(elt_locations))
}

# Calculate direct sequence matrix, two-loop matrix, and long term sequence
heuristicsMatrices <- function(w, ids = "id", events = "event", timestamps = "timestamp") {
  # Make data frame of traces
  traces <- getAllTraces(w, ids, events, timestamps)
  # Make hash function for event names
  event_list <- getEvents(w, events)
  event_hash <- eventHash(event_list)
  # Initialize direct sequence matrix with zeroes
  matrixDimension <- length(event_list)
  dsm <- matrix(0, nrow = matrixDimension, ncol = matrixDimension)
  rownames(dsm) <- event_list
  colnames(dsm) <- event_list
  # Initialize two loop matrix with with zeroes
  tlm <- matrix(0, nrow = matrixDimension, ncol = matrixDimension)
  rownames(tlm) <- sapply(event_list, function(e) {paste0(e, "x", e)})
  colnames(tlm) <- event_list
  # Loop for summing up dsm and tlm
  for (trace in traces$trace) {
    for (i in 1:max(1, (length(trace) - 1))) {
      # Use event hash function to translate to numbers
      from <- event_hash(trace[i])
      to <- event_hash(trace[i + 1])
      # Update direct sequence matrix
      dsm[from, to] <- dsm[from, to] + 1
      # Check for two-loops
      if (i < length(trace) - 1) {
        two <- event_hash(trace[i + 2])
        if (from == two) {
          tlm[from, to] <- tlm[from, to] + 1
        }
      }
    }
  }
  # Initialize long term dependence matrix
  ltdm <- matrix(0, nrow = matrixDimension, ncol = matrixDimension)
  rownames(ltdm) <- event_list
  colnames(ltdm) <- event_list
  # Loop for summing up ltdm
  for (trace in traces$trace) {
    locations <- mapply(firstLast, list(trace), event_list)
    for (i in 1:matrixDimension) {
      for (j in i:matrixDimension) {
        if ((locations[1, i] > 0) && (locations[1, i] < locations[2, j])) {
          ltdm[i,j] <- ltdm[i,j] + 1
        }
        if ((locations[1, j] > 0) && (locations[1, j] < locations[2, i])) {
          ltdm[j,i] <- ltdm[j,i] + 1
        }
      }
    }
  }
  # Diagonal elements are counted twice
  diag(ltdm) <- round(diag(ltdm) * 0.5)
  # Return list of matrices
  list(dsm, tlm, ltdm)
}

# Calculate direct sequence heuristics value
directSequenceHeuristicsValue <- function(dsm) {
  (dsm - t(dsm)) / (dsm + t(dsm) + 1)
}

# Calculate one-loop heuristics values
oneLoopHeursticsValue <- function(dsm) {
  diagonal <- diag(dsm)
  diagonal / (diagonal + 1)
}

# Calculate two-loop heuristics values
twoLoopHeursticsValue <- function(tlm) {
  (tlm + t(tlm)) / (tlm + t(tlm) + 1)
}

# Disregard values under a certain threshold
trimMatrix <- function(mat, threshold = 0.9) {
  (mat > threshold) * mat
}
