source("data.R")
source("balancing.R")
source("petri.R")
source("utils.R")


main <- function(inputFunc, outputFunc, M, number) {
  
  commonFunc <- outputFunc - inputFunc
  randomTime <- 0
  randomTime2 <- 0
  newTime <- 0
  log <- matrix(c(0,0,0),nrow=3, ncol=1, byrow=TRUE)
  processTime <- 0.0
  for (i in 1:number) {
    transposedVector <- t(M)
    allowedTransitions <- getAllowedTransitions(transposedVector, inputFunc)
    transitionNumber <- getTransitionNumberWithResolving(allowedTransitions, i, "roundRobin")
    cat("[main] Transition number : ", wd)
    if (ncol(log) == 1 && allowedTransitions == conflictedTransitions) {
      nodes <- getNodesIds(allowedTransitions)
      log <- matrix(0, 3, ncol(nodes))
      for (k in 1:ncols(nodes)) {
        log[1,k] <- (t(nodes))[k]
      }
    }
    for (m in 1:ncols(log)) {
      if (log[1, m] == transitionNumber) {
        log[2, m] <- log[2, m] + 1
        log[3, m] <- log[3, m] + 2 # ???
      }
    }
    newProccesTime <- performTransition(transitionNumber, processTime)
    processTime <- newProccesTime
    startingVector <- createStartingVector(allowedTransitions)
    M <- t(startingVector) * commonFunc + M
  }
  cat("[main] Result time: ", processTime)
  print(log)
}
















