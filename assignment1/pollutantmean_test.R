source("pollutantmean.R")

assert <- function(result, expected){
  if(result == expected)
    sprintf("Success! The result is %g, as expedted.", expected)
  else
    sprintf("Fail! The result is %g. Expected value was %g", result, expected)
}

cases <- c(
  assert(pollutantmean("specdata", "sulfate", 1:10), 4.064),
  assert(pollutantmean("specdata", "nitrate", 23), 1.281),
  assert(pollutantmean("specdata", "nitrate", 70:72), 1.706)
)

lapply(cases, print)
