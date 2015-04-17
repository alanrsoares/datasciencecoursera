pollutantmean <- function(directory, pollutant, id = 1:332) {
  fileList = list.files(directory)
  files = as.numeric(sub("\\.csv$", "", fileList))
  selected_files = fileList[match(id, files)]
  data = lapply(file.path(directory, selected_files), read.csv)
  data = do.call(rbind.data.frame, data)
  result <- mean(data[,pollutant], na.rm = TRUE)
  format(round(result, 3), nsmall = 3)
}
