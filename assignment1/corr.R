source('complete.R')

corr <- function(directory, threshold = 0) {
  data <- complete(directory)
  data <- subset(data, nobs >= threshold)
  ids  <- data$id
  
  correspond <- function(id) {
    data <- na.omit(read_data(id, directory))
    cor(data$sulfate, data$nitrate)
  }

  unlist(lapply(ids, correspond))
}
