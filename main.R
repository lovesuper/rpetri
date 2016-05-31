source("data.R")

numberOfTransitionsToPerform <- 100
# A+ function
APlus = matrix(
    c(
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), nrow=9, ncol=10, byrow=TRUE)

# A- function
AMinus = matrix(
    c(
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 1, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ), nrow=9, ncol=10, byrow=TRUE)

# Transitions that conflicted with each other
conflictedTransitions = matrix(
    c(0, 1, 1, 1, 0, 0, 0, 0, 0) ,
    nrow = 9,
    ncol = 1,
    byrow = TRUE
)

# Started position of marks in the net
startingMarks = matrix(
    c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0),
    nrow = 1,
    ncol = 10
)

testRequestsWeights = matrix(
    c(1, 3, 2, 5, 9, 3, 6, 4, 9, 7, 4, 8, 4, 9, 6, 3, 4, 2, 1, 1),
    nrow = 1,
    ncol = 20,
    byrow = TRUE
)

# Implementations of algorithms for load balancing

pickRandom <- function(range) {
    colsCount <- length(range)
    random1 <- runif(1, 0, colsCount)
    index <- floor(random1)
    return(range[index + 1])
}

pickNext <- function(range, number) {
    index <- number %% length(range)
    return(range[index + 1])
}

pickByOrder <- function(orderedRange, number) {
    return(pickNext(orderedRange, number))
}

weightRoundRobin <- function(nodes, iteration) {
    return(pickByOrder(nodes, iteration))
}

roundRobin <- function(nodes, iteration) {
    return(pickNext(nodes, iteration))
}

randomBalancing <- function(conflicts) {
    return(pickRandom(conflicts))
}

getNodesIds <- function(nodes) {
    conflictedNodes <- matrix(c(0),
                              nrow = 1,
                              ncol = 1,
                              byrow = TRUE)
    for (i in 1:length(nodes)) {
        if (nodes[i] == 1) {
            if ((ncol(conflictedNodes) == 1) && (conflictedNodes[1] == 0)) {
                conflictedNodes[1] <- i
                next()
            }
            conflictedNodes <- c(conflictedNodes, c(i))
        }
    }
    return(conflictedNodes)
}

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
    rowsCount <- nrow(inputFunc)
    colsCount <- ncol(inputFunc)
    transposedMatrix <- matrix(NA, nrow = 9, ncol = 10)
    transitionIndicatorVector <- vector('integer')
    for (ig in 1:rowsCount) {
        transposedMatrix[ig, ] <- t(transposedInputFunc[, ig])
    }
    allowedTransitions <- vector('integer')
    for (i in 1:rowsCount) {
        transposedRow <- t(transposedMatrix[i, ])

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
    startingVector <- matrix(0, nrow = 1, ncol = 1)
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

resolveConflict <- function(method, nodes, iteration) {
    if (method == "random") {
        return(randomBalancing(nodes))
    } else if (method == "roundRobin") {
        return(roundRobin(nodes, iteration))
    } else if (method == "weightRoundRobin") {
        return(weightRoundRobin(nodes, iteration))
    }

    stop()
}

getTransitionNumberWithResolving <- function(nodes, iteration, method) {
    vectorLength <- length(nodes)
    q <- 1
    if (all(nodes == conflictedTransitions)) {
        conflictedNodes <- getNodesIds(nodes)
        transitionNumber <-
            resolveConflict(method, conflictedNodes, iteration)
        return(transitionNumber)
    }

    for (i in 1:vectorLength) {
        if ((q * nodes[i]) == 1) {
            transitionNumber <- i
        }
        if (nodes[i] > 0) {
            q <- 0
        }
    }
    return(transitionNumber)
}

getRandomTimeForLambda <- function(lambda) {
    randomTime <- (-1 / lambda) * log(1 - runif(1, 0, 1))
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
    } else if (transition == 2) {
        lambda <- 0.2
    } else if (transition == 3) {
        lambda <- 0.3
    } else if (transition == 4) {
        lambda <- 0.4
    } else if (transition == 5) {
        lambda <- 0.5
    } else if (transition == 6) {
        lambda <- 0.6
    } else if (transition == 7) {
        lambda <- 0.7
    } else if (transition == 8) {
        lambda <- 0.8
    } else if (transition == 9) {
        lambda <- 0.9
    }
    transitionTime <- getRandomTimeForLambda(lambda)
    return(processTime + transitionTime)
}

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
    tasksCount <- 0
    logg <- matrix(c(0, 0, 0),
                   nrow = 3,
                   ncol = 1,
                   byrow = TRUE)
    processTime <- 0.0
    for (i in 1:number) {
        transposedVector <- t(M)
        allowedTransitions <-
            getAllowedTransitions(transposedVector, inputFunc)
        transitionNumber <-
            getTransitionNumberWithResolving(allowedTransitions, i, "random")
        cat("[Main] Transition number is: ", transitionNumber, "\n")
        if (ncol(logg) == 1 &&
            allowedTransitions == conflictedTransitions) {
            nodes <- getNodesIds(allowedTransitions)
            logg <- matrix(0, 3, length(nodes))
            for (k in 1:length(nodes)) {
                logg[1, k] <- t(nodes)[k]
            }
        }

        for (m in 1:ncol(logg)) {
            if (logg[1, m] == transitionNumber) {
                tasksCount <- tasksCount + 1
                cat("task number: ", tasksCount)
                logg[2, m] <- logg[2, m] + 1
                logg[3, m] <- logg[3, m] + testRequestsWeights[tasksCount %% length(testRequestsWeights) + 1]
            }
        }

        newProccesTime <-
            performTransition(transitionNumber, processTime)
        processTime <- newProccesTime
        startingVector <- createStartingVector(allowedTransitions)
        M <- t(startingVector) %*% commonFunc + M
    }
    cat("[main] Result time: ", processTime, "\n")
    print(logg)
}

main(APlus, AMinus, startingMarks, numberOfTransitionsToPerform)
































