complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  names <- c("id", "nobs")
  files <- list.files(path = directory)[id]
  cases <- c()
  for (f in files) {
    path <- paste(directory, f, sep= '/')
    data <- read.csv(path)
    
    id_s <- strsplit(f, '[.]')[[1]][1]
    id <- as.integer(id_s)
    
    cols <- !is.na(data[2:3])
    r <- rowSums(cols)
    s <- r[r == 2]/2
    vec <- c(id, sum(s))
    cases <- c(cases, vec)
  }
  is.odd <- rep(c(TRUE, FALSE), length = length(cases))
  data.frame(id = cases[is.odd], nobs = cases[!is.odd])
}
