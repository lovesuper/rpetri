pickRandom <- function(range) {
  colsCount <- ncol(range)
  random1 <- rnd(colsCount)
  index <- floor(random1)
  return(range[0, index])
}

pickNext <- function(range, rumber) {
  index <- mod(number, ncol(range))
  return(range[0, index])
}

pickByOrder <- function(orderedRange, number) {
  return(
    pickNext(orderedRange, number)
  )
}

weightRoundRobin <- function(nodes, iteration) {
  return(
    pickByOrder(nodes, iteration)
  )
}

roundRobin <- function(nodes, iteration) {
  return(
    pickNext(nodes, iteration)
  )
}

randomBalancing <- function(conflicts) {
  return(pickRandom(conficts))
}
