complete <- function(directory, id = 1:332) {
  
  outputs = lapply(id, getmonitor, directory = directory)
  compCases = lapply(outputs, complete.cases)
  
  # count is length(which(logical.vector))
  onlyTrues = lapply(compCases, which)
  nobs = sapply(onlyTrues, length)
  
  data.frame(id, nobs)
}
