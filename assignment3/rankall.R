source('rankhospital.R')

rankall <- function(outcome, num = 'best') {

  states <- get_states()
  column <- get_outcome_column(outcome)
  
  rank_hospital_by_state <-  function(state) rankhospital(state, outcome, num) 
  
  data.frame( hospital = unlist(lapply(states, rank_hospital_by_state))
            , state    = states 
            )
}
