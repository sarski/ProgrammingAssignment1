corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  files <- list.files(directory, full.names = TRUE)
  initdata <- complete(directory)
  result <- c()
  for (numbr in initdata$id) {
    if (initdata$nobs[numbr] > threshold) {
      datafile <- read.csv(files[numbr])
      datacorr <- datafile[complete.cases(datafile), ]
      corvalue <- cor(datacorr$sulfate, datacorr$nitrate)
      result <- c(result, corvalue)
    }
  }
  result
}