best <- function(state, outcome) {
  data <- read.csv('data/outcome-of-care-measures.csv', colClasses = 'character')
  states <- unique(data[, 7])

  if(!is.element(state, states))
    stop('invalid state')
  
  # data filtered by state
  data <- data[data$State == state, ]
  
  column <- 0
  column <- switch(outcome
                , 'heart attack'  = 13
                , 'heart failure' = 19
                , 'pneumonia'     = 25)
  
  if(column == 0)
    stop('invalid outcome')
  
  data <- data[, c(2, column)]
  data[, 2] <- as.numeric(data[, 2])
  data <- data[order(data[2]), ]
  data[1, 1]
}
