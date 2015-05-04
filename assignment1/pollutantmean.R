pollutantmean <- function(directory, pollutant, id = 1:332) {
  file_list = list.files(directory)
  files = as.numeric(sub("\\.csv$", "", file_list))
  selected_files = file_list[match(id, files)]
  data = lapply(file.path(directory, selected_files), read.csv)
  data = do.call(rbind.data.frame, data)
  mean(data[, pollutant], na.rm = TRUE)
} 
