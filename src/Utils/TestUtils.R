WithinDelta <- function(delta) {
  function(expected,actual) {
    abs(actual - expected) <= delta
  }
}

Equal <- function(expected, actual) {
  expected == actual
}

AssertMatch <- function(expected,actual,fun,description,verbose = T) {
  if (!fun(expected,actual)) {
    warning(paste(description,". No match! Expected: ", expected, ", got: ", actual, sep = ""), call. = F)
  } else if (verbose) {
    print(paste(description,". Success!",sep = ""))
  }
}