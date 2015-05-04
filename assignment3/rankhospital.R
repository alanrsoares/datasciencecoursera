rankhospital <- function(state, outcome, num = 'best') {
  data <- read.csv('data/outcome-of-care-measures.csv', colClasses = 'character')
  states <- unique(data[, 7])
  
  # validate state
  if(!is.element(state, states))
    stop('invalid state')

  column <- switch(outcome
                   , 'heart attack'  = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
                   , 'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
                   , 'pneumonia'     = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
                   , stop('invalid outcome'))
  
  # data filtered by state
  data <- data[data$State == state, c("Hospital.Name", column)]
  
  # outcome column as numeric
  data[, 2] <- as.numeric(data[, 2])
  
  # order by outcome
  ordered_data <- order(data[column], data$Hospital.Name, na.last=NA)
  
  # hospital getter by order key
  hospital <- function(order_key) data$Hospital.Name[ordered_data[order_key]]
  
  if (num == 'best')
    hospital(1)
  else if (num == 'worst')
    hospital(length(ordered_data))
  else if (is.numeric(num))
    hospital(num)
  else
    stop('invalid num')
}
