source("data.R")

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
  vectorLength <- len(vector)
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
  vectorLength <- len(vector)
  q <- 1
  for (i in 1:vectorLength) {
    if ((q * vector[i]) == 1) {
      transitionNumber = i + 1
    }
    if (vector[i] > 0) {
      q <- 0
    }
  }
  return(transitionNumber)
}

getEventTimeByLambda <- function(lambda, time) {
  randomTime <- (-1 / lambda) * ln(1 - rnd(1))
  return(time + randomTime)
}

analysEvents <- function(lambda, time1, time2) {
  randomTime <- (-1 / lambda) * ln(1 - rnd(1))
  maxTime <- max(time1, time2)
  return(maxTime + randomTime)
}

getSystemCharackteristics <- function(timeOnOutput, idleTime, time1, time2, deltaTime) {
  timeOnOutput <- timeOnOutput + deltaTime
  if (time1 >= time2) {
    time <- time1 - time2
  } else {
    time <- 0
  }
  idleTime <- idleTime + time
  return(c(timeOnOutput, idleTime))
}

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

resolveConflicts <- function(method, nodes, iteration) {
  if (method == "random") {
    return(randomBalancing(nodes))
  }

  if (method == "roundRobin") {
    return(roundRobin(nodes))
  }

  if (method == "weightRoundRobin") {
    return(weightRoundRobin(nodes, iteration))
  }
  
  stop()
}

getNodesIds <- function(nodes) {
  conflictedNodes <- c(0)
  for (i in 1:len(nodes)) {
    if (nodes[i] == 1) {
      if (ncol(conflictedNodes) == 1 && conflictedNodes[0] == 0) {
        conflictedNodes[0] <- i + 1
        next()
      }
    conflictedNodes <- conflictedNodes + v(i + 1)
    }
  }
  return(conflictedNodes)
}

getTransitionNumberWithResolving <- function(nodes, iteration, method) {
  vectorLength <- len(nodes)
  q <- 1
  if (nodes == conflictedTransitions) {
    conflicedNodes <- getNodesIds(nodes)
    transitionNumber <- resolveConflict(method, conflicedNodes, iteration)
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
  randomTime <- (-1/lambda) * ln(1 - rnd(1))
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

main <- function(inputFunc, outputFunc, M, number) {
  commonFunc <- outputFunc - inputFunc
  
}
















