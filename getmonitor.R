getmonitor <- function(id, directory, summarize = FALSE) {
 
  filename <- sprintf("%s/%03d.csv",directory,as.numeric(id))
  
  result <- read.csv(filename)
  
  if (summarize){
    print(summary(result))
  }
  result
}
