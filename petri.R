#' Title
#'
#' @param currentState
#' @param inputFunc
#'
#' @return
#' @export
#'
#' @examples
getAllowedTransitions <- function(currentState, inputFunc) {
  transposedInputFunc <- t(inputFunc)
  print(transposedInputFunc)
  rowsCount <- nrow(inputFunc)
  colsCount <- ncol(inputFunc)
  transposedMatrix <- matrix(NA, nrow=9, ncol=10)
  transitionIndicatorVector <- vector('integer')
  for (ig in 1:rowsCount) {
    transposedMatrix[ig,] <- t(transposedInputFunc[,ig])
  }
  allowedTransitions <- vector('integer')
  for (i in 1:rowsCount) {
    transposedRow <- t(transposedMatrix[i,])

    transitionIndicatorVector[i] <- 0
    for (j in 1:colsCount) {
      if ((currentState[j] - transposedRow[j]) >= 0) {
        transitionIndicatorVector[i] <- transitionIndicatorVector[i] + 1
      }
      if (transitionIndicatorVector[i] >= colsCount) {
        allowedTransitions[i] <- 1
      } else {
        allowedTransitions[i] <- 0
      }
    }
  }
  return(allowedTransitions)
}

createStartingVector <- function(vector) {
  vectorLength <- length(vector)
  startingVector <- 0
  q <- 1
  for (i in 1:vectorLength) {
    startingVector[i] <- q * vector[i]
    if (vector[i] > 0) {
      q <- 0
    }
  }
   return(startingVector)
}

getTransactionNumber <- function(vector) {
  vectorLength <- length(vector)
  q <- 1
  for (i in 1:vectorLength) {
    if ((q * vector[i]) == 1) {
      transitionNumber <- i + 1
    }
    if (vector[i] > 0) {
      q <- 0
    }
  }
  return(transitionNumber)
}

getEventTimeByLambda <- function(lambda, time) {
  randomTime <- (-1 / lambda) * log(1 - runif(1, 0, 1))
  return(time + randomTime)
}

analysEvents <- function(lambda, time1, time2) {
  randomTime <- (-1 / lambda) * log(1 - runif(1, 0, 1))
  maxTime <- max(time1, time2)
  return(maxTime + randomTime)
}

getSystemCharacteristics <- function(timeOnOutput, idleTime, time1, time2, deltaTime) {
  timeOnOutput <- timeOnOutput + deltaTime
  if (time1 >= time2) {
    time <- time1 - time2
  } else {
    time <- 0
  }
  idleTime <- idleTime + time
  return(c(timeOnOutput, idleTime))
}

resolveConflicts <- function(method, nodes, iteration) {
  if (method == "random") {
    return(randomBalancing(nodes))
  }

  if (method == "roundRobin") {
    return(roundRobin(nodes, iteration))
  }

  if (method == "weightRoundRobin") {
    return(weightRoundRobin(nodes, iteration))
  }

  stop()
}

getNodesIds <- function(nodes) {
  conflictedNodes <- c(0)
  for (i in 1:length(nodes)) {
    if (nodes[i] == 1) {
      if (ncol(conflictedNodes) == 1 && conflictedNodes[0] == 0) {
        conflictedNodes[0] <- i + 1
        next()
      }
    conflictedNodes <- c(conflictedNodes, c(i + 1))
    }
  }
  return(conflictedNodes)
}

getTransitionNumberWithResolving <- function(nodes, iteration, method) {
  vectorLength <- length(nodes)
  q <- 1
  if (nodes == conflictedTransitions) {
    conflictedNodes <- getNodesIds(nodes)
    transitionNumber <- resolveConflict(method, conflictedNodes, iteration)
    return(transitionNumber)
  }

  for (i in 1:vectorLength) {
    if((q * nodes[i]) == 1) {
      transitionNumber <- i + 1
    }
    if(nodes[i] > 0) {
      q <- 0
    }
  }
  return(transitionNumber)
}

getRandomTimeForLambda <- function(lambda) {
  randomTime <- (-1/lambda) * log(1 - runif(1, 0, 1))
  return(randomTime)
}

getTransitionTime <- function(currentTime, lambda) {
  time <- currentTime + getRandomTimeForLambda(lambda)
  return(time)
}

performTransition <- function(transition, processTime) {
  lambda <- 0.1
  transitionTime <- 0.0
  if (transition == 1) {
    lambda <- 0.1
  }
  if (transition == 2) {
    lambda <- 0.2
  }
  if (transition == 3) {
    lambda <- 0.3
  }
  if (transition == 4) {
    lambda <- 0.4
  }
  if (transition == 5) {
    lambda <- 0.5
  }
  if (transition == 6) {
    lambda <- 0.6
  }
  if (transition == 7) {
    lambda <- 0.7
  }
  if (transition == 8) {
    lambda <- 0.8
  }
  if (transition == 9) {
    lambda <- 0.9
  }
  transitionTime <- getRandomTimeForLambda(lambda)
  return(processTime + transitionTime)
}
