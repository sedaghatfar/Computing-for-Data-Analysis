count <- function(cause = NULL){
  
  ## Read "homicides.txt" data file
  homidices <- readLines("homicides.txt")
  
  ## Check if "cause" is non-NULL; else throw error
  if (is.null(cause)) stop("cause null")
  
  # Make cause lower case
  cause <- tolower(cause)
  
  ## Check that specific "cause" is allowed; else throw error
  causes <- c('asphyxiation', 'blunt force', 'other', 
              'shooting', 'stabbing', 'unknown')
  if (! cause %in% causes) stop('cause is not allowed')
  
  
  
  ## Extract causes of death
  r <- regexec("<dd>Cause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  res <- sapply(m, function(x) if (!is.na(x[2]))x[2])
  res <- tolower(res)
  ## Return integer containing count of homicides for that cause
  length(res[res == cause])
}
