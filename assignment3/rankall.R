source('rankhospital.R')

rankall <- function(outcome, num = 'best') {
  data <- read.csv('data/outcome-of-care-measures.csv', colClasses = 'character')
  states <- sort(unique(data[, 7]))
  column <- switch( outcome
                  , 'heart attack'  = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
                  , 'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
                  , 'pneumonia'     = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
                  , stop('invalid outcome')
                  )
  
  rank_hospital_by_state <-  function(state) rankhospital(state, outcome, num) 
  
  data.frame( hospital = unlist(lapply(states, rank_hospital_by_state))
            , state    = states 
            )
}
