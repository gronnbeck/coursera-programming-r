corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  a <- complete(directory)
  over <- a[a[, "nobs"] > threshold,]
  id <- over[,"id"]
  
  files <- list.files(path = directory)[id]
  cors <- c()
  for (f in files) {
    path <- paste(directory, f, sep= '/')
    data <- read.csv(path)
    
    cols <- data[2:3]
    cors <- c(cors, cor(cols, use= "complete.obs")[1,2])
  }
  cors
}