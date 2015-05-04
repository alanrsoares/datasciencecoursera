source('rankhospital.R')

rankall <- function(outcome, num = 'best') {
  column <- get_outcome_column(outcome)
  
  rank_hospital_by_state <- function(state) rankhospital(state, outcome, num)
  
  data.frame( hospital = sapply(STATES, rank_hospital_by_state)
            , state    = STATES
            )
}
