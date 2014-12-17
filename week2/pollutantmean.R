pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  files <- list.files(path = directory)
  m <- c()
  for (c in files[id]) {
    
    path <- paste(directory, c, sep= '/')
    data <- read.csv(path)
    filter <- data[pollutant]
    m <- c(m, filter[!is.na(filter)])  
  }
  mean(m)
}

pollutantmean("~/dev/04-kompetanse/coursera-r-programming/week2/specdata", "sulfate")

