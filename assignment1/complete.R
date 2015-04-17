read_data <- function(id, directory) {
  file <- sprintf("%s/%03d.csv", directory, as.numeric(id))
  read.csv(file)
}

complete <- function(directory, id = 1:332) {
  count <- function(id){
    monitor <- read_data(id, directory)
    nrow(na.omit(monitor))
  }
  nobs <- unlist(Map(count, id))
  data.frame(id = id, nobs = nobs)
}
