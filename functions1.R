add2 <- function(x, y){
  x + y
}

above <- function(x, y){
  use <- x > y
  x[use]
}

columnmean <- function(y, removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[, i], na.rm = removeNA)
  } 
  means
}

hd <- function(xs) xs[1]

tl <- function(xs) xs[-1]

map2 <- function(fn, xs){
  if(length(xs) == 0) return(xs)
  if(length(xs) == 1) return(fn(hd(xs)))
  c(fn(hd(xs)), map2(fn, tl(xs)))
}

reduce <- function(fn, xs, accum){
  if(length(xs) == 0) return(accum)
  if(length(xs) == 1) return(fn(accum, hd(xs)))
  reduce(fn, xs[-1], fn(accum, xs[1]))
}

filter <- function(fn, xs){
  if(length(xs) == 0) return(xs)
  if(length(xs) == 1 && fn(hd(xs))) return(hd(xs))
  if(!fn(hd(xs))) return(filter(fn, tl(xs)))
  c(hd(xs), filter(fn, tl(xs)))
}

