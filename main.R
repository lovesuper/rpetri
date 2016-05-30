source("data.R")
source("balancing.R")
source("petri.R")
source("utils.R")

numberOfTransitionsToPerform <- 50

#' Title
#'
#' @param inputFunc
#' @param outputFunc
#' @param M
#' @param number
#'
#' @return
#' @export
#'
#' @examples
main <- function(inputFunc, outputFunc, M, number) {
  commonFunc <- outputFunc - inputFunc
  randomTime <- 0
  randomTime2 <- 0
  newTime <- 0
  logg <- matrix(c(0,0,0),nrow=3, ncol=1, byrow=TRUE)
  processTime <- 0.0
  for (i in 1:number) {
    transposedVector <- t(M)
    allowedTransitions <- getAllowedTransitions(transposedVector, inputFunc)
    transitionNumber <- getTransitionNumberWithResolving(allowedTransitions, i, "roundRobin")
    cat("[main] Transition number : ", transitionNumber)
    if (ncol(logg) == 1 && allowedTransitions == conflictedTransitions) {
      nodes <- getNodesIds(allowedTransitions)
      logg <- matrix(0, 3, ncol(nodes))
      for (k in 1:ncol(nodes)) {
        logg[1,k] <- t(nodes)[k]
      }
    }
    for (m in 1:ncol(logg)) {
      if (logg[1, m] == transitionNumber) {
        logg[2, m] <- logg[2, m] + 1
        logg[3, m] <- logg[3, m] + 2 # ???
      }
    }
    newProccesTime <- performTransition(transitionNumber, processTime)
    processTime <- newProccesTime
    startingVector <- createStartingVector(allowedTransitions)
    M <- t(startingVector) * commonFunc + M
  }
  cat("[main] Result time: ", processTime)
  print(logg)
}

main(APlus, AMinus, startingMarks, numberOfTransitionsToPerform)














