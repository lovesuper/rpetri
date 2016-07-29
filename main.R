#https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R
library(crayon)
require("clue")

error <- red$bold

list <- structure(NA, class = "result")
#' Title
#'
#' @param x
#' @param ...
#' @param value
#'
#' @return
#' @export
#'
#' @examples
"[<-.result" <- function(x, ..., value) {
    args <- as.list(match.call())
    args <- args[-c(1:2, length(args))]
    length(value) <- length(args)
    for (i in seq(along = args)) {
        a <- args[[i]]
        if (!missing(a))
            eval.parent(substitute(a <- v, list(a = a, v = value[[i]])))
    }
    x
}
# imported funtions end

# A+ function
APlus = matrix(
    c(
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), nrow = 13, ncol = 14, byrow = TRUE)

# A- function
AMinus = matrix(
    c(
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ), nrow = 13, ncol = 14, byrow = TRUE)

# Transitions that conflicted with each other
conflictedTransitions = matrix(
    c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    nrow = 13,
    ncol = 1,
    byrow = TRUE
)

# Started position of marks in the net
startingMarks = matrix(
    c(1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
    nrow = 1,
    ncol = 14
)

# Random algorithm
#' Title
#'
#' @param range
#'
#' @return
#' @export
#'
#' @examples
pickRandom <- function(range) {
    colsCount <- length(range)
    random1 <- runif(1, 0, colsCount)
    index <- floor(random1)
    range[index + 1]
}

#' Title
#'
#' @param conflicts
#'
#' @return
#' @export
#'
#' @examples
randomBalancing <- function(conflicts) {
    pickRandom(conflicts)
}

# Round Robin algorithm
#' Title
#'
#' @param range
#' @param number
#'
#' @return
#' @export
#'
#' @examples
pickNext <- function(range, number) {
    index <- number %% length(range)
    range[index + 1]
}

#' Title
#'
#' @param nodes
#' @param iteration
#'
#' @return
#' @export
#'
#' @examples
roundRobin <- function(nodes, iteration) {
    pickNext(nodes, iteration)
}

# Weight Round Robin algorithm
#' Title
#'
#' @param orderedRange
#' @param number
#'
#' @return
#' @export
#'
#' @examples
pickByOrder <- function(orderedRange, number) {
    pickNext(orderedRange, number)
}

#' Title
#'
#' @param nodes
#' @param iteration
#'
#' @return
#' @export
#'
#' @examples
weightRoundRobin <- function(nodes, iteration) {
    pickByOrder(nodes, iteration)
}

# Implementations of algorithms for load balancing

# Last succesful connectoin algorithm
#' Title
#'
#' @return
#' @export
#'
#' @examples
pickNodeWithLeastConnections <- function() {

}

# Dynamic Weight Round Robin algorithm
#' Title
#'
#' @return
#' @export
#'
#' @examples

exoustedPickNext <- function(schedule) {
    if (length(schedule) == 0) {
        return(list(NULL, NULL))
    }

    list(
        head(schedule, length(schedule) - 1),
        tail(schedule, 1)
    )
}

algorithmSwitchStratege <- function(systemHistory) {
    for (i in 1:length(systemHistory)) {
        if (length(systemHistory[[i]]) < 5) {
            return(FALSE)
        }
    }

    TRUE
}

dynamicWeightAlgorithm <- function(systemHistory, scheduleList, number, defaultNodes) {
    # if we don't need to change strateg to Hungarian
    if (!algorithmSwitchStratege(systemHistory)) {
        # we'll use standart Round Robin balancing stratege
        return(pickNext(defaultNodes, number))
    }
    # if schedule list is empty then we need to recalculate it
    if (length(scheduleList) == 0) {
        f <- systemHistory[[1]]
        s <- systemHistory[[2]]
        t <- systemHistory[[3]]
        f <- systemHistory[[4]]
        fi <- systemHistory[[5]]

        performanceHistory <- matrix(
            c(
                tail(f, 3)[1], tail(f, 3)[2], tail(f, 3)[3],
                tail(s, 3)[1], tail(s, 3)[2], tail(s, 3)[3],
                tail(t, 3)[1], tail(t, 3)[2], tail(t, 3)[3],
                tail(f, 4)[1], tail(f, 4)[2], tail(f, 4)[3],
                tail(fi, 5)[1], tail(fi, 5)[2], tail(fi, 5)[3]
            ),
            ncol = 5,
            nrow = 5,
            byrow = TRUE
        )
        performanceHistory = t(performanceHistory)

        # optimalAssignment <- solve_LSAP(performanceHistory, maximum = TRUE)
        optimalAssignment <- solve_LSAP(performanceHistory, maximum = FALSE)

        extractList <- function(it) { it[1] }
        scheduleList <- lapply(optimalAssignment, extractList)

        # scheduleList2 <- lapply(optimalAssignment2, extractList)

        # scheduleList <- rev(scheduleList)

        # c(unlist(scheduleList), unlist(scheduleList2))

        # scheduleList <- c(scheduleList[[1]],
        #                   scheduleList[[2]],
        #                   scheduleList[[1]],
        #                   scheduleList[[3]]
        #                   )

        # scheduleList <- rep(scheduleList, 2)
    }

    targetNode <- NA
    newScheduleList <- NA
    # pop next node number from scheduleList
    list[newScheduleList, targetNode] <- exoustedPickNext(scheduleList)

    return(list(targetNode = targetNode[[1]] + 1, scheduleList = newScheduleList))
}

# Utils
#' Title
#'
#' @param nodes
#'
#' @return
#' @export
#'
#' @examples
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

    conflictedNodes
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
    transposedMatrix <- matrix(NA, nrow = 13, ncol = 14)
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

    allowedTransitions
}

#' Title
#'
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
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

    startingVector
}

#' Title
#'
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
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

    transitionNumber
}

#' Title
#'
#' @param lambda
#' @param time
#'
#' @return
#' @export
#'
#' @examples
getEventTimeByLambda <- function(lambda, time) {
    randomTime <- (-1 / lambda) * log(1 - runif(1, 0, 1))

    time + randomTime
}

#' Title
#'
#' @param lambda
#' @param time1
#' @param time2
#'
#' @return
#' @export
#'
#' @examples
analysEvents <- function(lambda, time1, time2) {
    randomTime <- (-1 / lambda) * log(1 - runif(1, 0, 1))
    maxTime <- max(time1, time2)

    maxTime + randomTime
}

#' Title
#'
#' @param timeOnOutput
#' @param idleTime
#' @param time1
#' @param time2
#' @param deltaTime
#'
#' @return
#' @export
#'
#' @examples
getSystemCharacteristics <- function(timeOnOutput, idleTime, time1, time2, deltaTime) {
    timeOnOutput <- timeOnOutput + deltaTime
    if (time1 >= time2) {
        time <- time1 - time2
    } else {
        time <- 0
    }
    idleTime <- idleTime + time

    c(timeOnOutput, idleTime)
}

#' Title
#'
#' @param method
#' @param nodes
#' @param iteration
#'
#' @return
#' @export
#'
#' @examples
resolveConflict <- function(method, nodes, iteration, systemHistory, scheduleList, performedTasksCount) {
    if (method == "random") {
        randomBalancing(nodes)
    } else if (method == "roundRobin") {
        roundRobin(nodes, performedTasksCount)
    } else if (method == "weightRoundRobin") {
        weightRoundRobin(c(4, 2, 3, 5, 6), performedTasksCount)
    } else if (method == "leastConnectoins") {
        pickNodeWithLeastConnections()
    } else if (method == "dynamicWeightAlgorithm") {
        dynamicWeightAlgorithm(systemHistory, scheduleList, performedTasksCount, nodes)
    } else {
        cat("There is " %+% error("no") %+% " such balancing method: ", method)
        stop("Error")
    }
}

#' Title
#'
#' @param nodes
#' @param iteration
#' @param method
#'
#' @return
#' @export
#'
#' @examples
getTransitionNumberWithResolving <- function(nodes, iteration, method, systemHistory, scheduleList, performedTasksCount) {
    vectorLength <- length(nodes)
    q <- 1
    if (all(nodes == conflictedTransitions)) {
        conflictedNodes <- getNodesIds(nodes)
        transitionNumber <- resolveConflict(
            method,
            conflictedNodes,
            iteration,
            systemHistory,
            scheduleList,
            performedTasksCount
        )

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

    transitionNumber
}

#' Title
#'
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
getRandomTimeForLambda <- function(lambda) {
    # (-1 / lambda) * log(1 - runif(1, 0, 1))
    runif(1, 0, 1) + lambda
}

#' Title
#'
#' @param currentTime
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
getTransitionTime <- function(currentTime, lambda) {
    currentTime + getRandomTimeForLambda(lambda)
}

#' Perform Transition
#'
#' @param transition
#' @param processTime
#' @param taskWeight
#' @param detailedPerformanceLog
#'
#' @return
#' @export
#'
#' @examples

performTransition <-
    function(transition,
             processTime,
             taskWeight,
             detailedPerformanceLog,
             nodesState,
             rejectedTasks,
             nodesPerfs,
             nodesGaps) {
        firstNodePerf <- nodesPerfs[[1]]
        secondNodePerf <- nodesPerfs[[2]]
        thirdNodePerf <- nodesPerfs[[3]]
        fourthNodePerf <- nodesPerfs[[4]]
        fifthNodePerf <- nodesPerfs[[5]]

        firstNodeGap <- nodesGaps[[1]]
        secondNodeGap <- nodesGaps[[2]]
        thirdNodeGap <- nodesGaps[[3]]
        fourthNodeGap <- nodesGaps[[4]]
        fifthNodeGap <- nodesGaps[[5]]

        taskExcTime <- NULL
        lambda <- 0.1
        transitionTime <- 0.0
        currentPerformer <- NA

        nodesState[[2]] <- nodesState[[2]] * 0.7
        nodesState[[3]] <- nodesState[[3]] * 0.7
        nodesState[[4]] <- nodesState[[4]] * 0.7
        nodesState[[5]] <- nodesState[[5]] * 0.7
        nodesState[[6]] <- nodesState[[6]] * 0.7

        if (transition == 1) {
            # Starting transition
            # Needs to make a custom generator here in another words --
            # custom distribution
            lambda <- 0.1
        } else if (transition == 2) {
            # First node
            lambda <- firstNodePerf * taskWeight
            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= firstNodeGap) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
                # cat("FALLEN!\n")
            } else {
                # nodesState[[transition]] <-  nodesState[[transition]] - nodesState[[transition]] / 10
            }
            taskExcTime <- lambda
            currentPerformer <- transition
            # cat((per, "%\n\n")
            # detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
            per <- round(nodesState[[transition]] * 100 / firstNodeGap, digits = 0)
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(100 - per))
        } else if (transition == 3) {
            # Second node
            lambda <- secondNodePerf * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= secondNodeGap) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
                # nodesState[[transition]] <-  nodesState[[transition]] - nodesState[[transition]] / 10
            }
            taskExcTime <- lambda
            currentPerformer <- transition
            # detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
            per <- round(nodesState[[transition]] * 100 / secondNodeGap, digits = 0)
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(100 - per))
        } else if (transition == 4) {
            # Third node
            lambda <- thirdNodePerf * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= thirdNodeGap) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
                # nodesState[[transition]] <- nodesState[[transition]] - nodesState[[transition]] / 10
            }
            taskExcTime <- lambda
            currentPerformer <- transition
            # detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
            per <- round(nodesState[[transition]] * 100 / thirdNodeGap, digits = 0)
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(100 - per))
        } else if (transition == 5) {
            # Fourth node
            lambda <- fourthNodePerf * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= fourthNodeGap) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
                # nodesState[[transition]] <- nodesState[[transition]] - nodesState[[transition]] / 10
            }
            taskExcTime <- lambda
            currentPerformer <- transition
            # detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
            per <- round(nodesState[[transition]] * 100 / fourthNodeGap, digits = 0)
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(100 - per))
        } else if (transition == 6) {
            # Fifth node
            lambda <- fifthNodePerf * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= fifthNodeGap) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
                # nodesState[[transition]] <- nodesState[[transition]] - nodesState[[transition]] / 10
            }
            taskExcTime <- lambda
            currentPerformer <- transition
            # detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
            per <- round(nodesState[[transition]] * 100 / fifthNodeGap, digits = 0)
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(100 - per))
        } else if (transition == 7) {
            lambda <- 0.1
        } else if (transition == 8) {
            lambda <- 0.1
        } else if (transition == 9) {
            lambda <- 0.1
        } else if (transition == 10) {
            lambda <- 0.1
        } else if (transition == 11) {
            lambda <- 0.1
        } else if (transition == 12) {
            lambda <- 0.1
        } else if (transition == 13) {
            lambda <- 0.1
        }

        # transitionTime <- getRandomTimeForLambda(lambda)
        transitionTime <- lambda
        processTime <- processTime + transitionTime

        list(processTime, detailedPerformanceLog, nodesState, rejectedTasks, currentPerformer, taskExcTime)
    }

#' Title
#'
#' @param headerVector
#'
#' @return
#' @export
#'
#' @examples
createStatsMatrix <- function(headerVector) {
    nodes <- getNodesIds(headerVector)
    statsMatrix <- matrix(0, 5, length(nodes))
    for (k in 1:length(nodes)) {
        statsMatrix[1, k] <- t(nodes)[k]
    }

    statsMatrix
}

#' Title
#'
#' @param firstNode
#' @param secondNode
#'
#' @return
#' @export
#'
#' @examples
makeAPlot <- function(firstNode, secondNode) {
    # Calculate range from 0 to max value of cars and trucks
    g_range <- range(0, firstNode, secondNode)
    # Graph autos using y axis that ranges from 0 to max
    # value in cars or trucks vector.  Turn off axes and
    # annotations (axis labels) so we can specify them ourself
    plot(
        firstNode,
        type = "o",
        col = "blue",
        ylim = g_range,
        axes = FALSE,
        ann = FALSE
    )

    # Make x axis using Mon-Fri labels
    axis(1,
         at = 1:5,
         lab = c("Mon", "Tue", "Wed", "Thu", "Fri"))

    # Make y axis with horizontal labels that display ticks at
    # every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
    axis(2, las = 1, at = 4 * 0:g_range[2])

    # Create box around plot
    box()

    # Graph trucks with red dashed line and square points
    lines(
        secondNode,
        type = "o",
        pch = 22,
        lty = 2,
        col = "red"
    )

    # Create a title with a red, bold/italic font
    title(main = "Test 1",
          col.main = "red",
          font.main = 4)

    # Label the x and y axes with dark green text
    title(xlab = "Days", col.lab = rgb(0, 0.5, 0))
    title(ylab = "Total", col.lab = rgb(0, 0.5, 0))

    # Create a legend at (1, g_range[2]) that is slightly smaller
    # (cex) and uses the same line colors and points used by
    # the actual plots
    legend(
        1,
        g_range[2],
        c("firstNode", "secondNode"),
        cex = 0.8,
        col = c("blue", "red"),
        pch = 21:22,
        lty = 1:2
    )

}

header <- function(text) {
    cat("\n[", toupper(text), "]\n")
}

subheader <- function(text) {
    cat("\n[", text, "]\n")
}

calculate_efficiency <- function(workingTime, idleTime) {
    signif(1 - 1 / (workingTime / idleTime), 3)
}

calculate_loading <- function(workingTime, idleTime) {
    signif(workingTime / idleTime, 3)
}

#' Main function for experiment
#'
#' @param inputFunc is param for A+
#' @param outputFunc is param for A-
#' @param M is starting marking
#' @param transitionsCount is count of transitions to perform
#' @param tasksCount is count of tasks to be done
#'
#' @return
#' @export
#'
#' @examples
main <- function(inputFunc,
                 outputFunc,
                 M,
                 transitionsCount,
                 tasksCount,
                 distribution,
                 distributionName,
                 balancingMethod,
                 nodesPerfs,
                 nodesGaps) {
    # print(distribution)
    commonFunc <- outputFunc - inputFunc
    performedTasksCount <- 0
    performanceLog <- matrix(c(0, 0, 0, 0, 0), nrow = 5, ncol = 1, byrow = TRUE)
    detailedPerformanceLog <- list(c(), c(), c(), c(), c())
    timeForCyclesVector <- c()
    processTime <- 0.0
    timeForCycle <- 0.0
    # may be used for nodesStatesVector !
    nodesState <- list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    rejectedTasks <- list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    idleTimeForWorkingNodes <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
    loadingForWorkingNodes <- list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
    currentPerformer <- NA
    # schedule list for DWRR
    scheduleList <- NULL
    transitionNumber <- NA
    # list of times of excecutions for nodes
    tasksExcecutionTimeVector <- c()
    for (i in 1:transitionsCount) {
        transposedVector <- t(M)
        allowedTransitions <- getAllowedTransitions(transposedVector, inputFunc)
        result <- getTransitionNumberWithResolving(
            allowedTransitions,
            i,
            balancingMethod,
            detailedPerformanceLog,
            scheduleList,
            performedTasksCount
        )

        if (is.list(result)) {
            transitionNumber <- result$targetNode
            scheduleList <- result$scheduleList
        } else {
            transitionNumber <- result
        }
        # Creating matrix for results
        if (ncol(performanceLog) == 1 && allowedTransitions == conflictedTransitions) {
            performanceLog = createStatsMatrix(allowedTransitions)
        }

        for (m in 1:ncol(performanceLog)) {
            if (performanceLog[1, m] == transitionNumber) {
                currentTaskWeight <- distribution[performedTasksCount %% length(distribution) + 1]
                performedTasksCount <- performedTasksCount + 1
                # Counting tasks amount
                performanceLog[2, m] <- performanceLog[2, m] + 1
                # Counting weight of tasks
                performanceLog[3, m] <- performanceLog[3, m] + currentTaskWeight
            }
        }

        newProccesTime <- 0.0
        newNodesState <- NA
        newRejectedTasks <- NA
        newCurrentPerformer <- NA
        newLambda <- NA
        list[newProccesTime, detailedPerformanceLog, newNodesState,
             newRejectedTasks, newCurrentPerformer, newLambda] <- performTransition(
            transitionNumber,
            timeForCycle,
            currentTaskWeight,
            detailedPerformanceLog,
            nodesState,
            rejectedTasks,
            nodesPerfs,
            nodesGaps
        )
        if (!is.na(newCurrentPerformer)) {
            currentPerformer <- newCurrentPerformer
        }

        tasksExcecutionTimeVector <- c(tasksExcecutionTimeVector, newLambda)
        nodesState <- newNodesState
        rejectedTasks <- newRejectedTasks
        timeForCycle <- newProccesTime

        startingVector <- createStartingVector(allowedTransitions)
        if (transitionNumber == nrow(commonFunc)) {
            # LOADING
            loadingForWorkingNodes[[currentPerformer - 1]] <- 100 - tail(detailedPerformanceLog[[currentPerformer - 1]], 1)
            idleTimeForLoop <- timeForCycle - newLambda
            idleTimeForWorkingNodes[[currentPerformer - 1]] <- c(
                idleTimeForWorkingNodes[[currentPerformer - 1]], idleTimeForLoop
            )
            # if wwrr is enabled
            if (i >= 3) {
                # firstNodeLoading <- calculate_loading(performanceLog[3, 1], idleTimeForWorkingNodes[[1]])
                # secondNodeLoading <- calculate_loading(performanceLog[3, 2], idleTimeForWorkingNodes[[2]])
                # thirdNodeLoading <- calculate_loading(performanceLog[3, 3], idleTimeForWorkingNodes[[3]])
                # systemLoading <- signif(
                #         median(
                #             c(median(firstNodeLoading), median(secondNodeLoading), median(thirdNodeLoading)
                #         )
                #     ), 3
                # )
                # cat(timeForCycle, ", ")
            }

            timeForCyclesVector <- c(timeForCyclesVector, c(timeForCycle))
            processTime <- processTime + timeForCycle
            timeForCycle <- 0.0
        }

        M <- t(startingVector) %*% commonFunc + M
        if (tasksCount > 0 && performedTasksCount == tasksCount) {
            break()
        }

    }

    if (TRUE) {
        cat("Balancing method:\t", balancingMethod, "\n")
        # cat("Working nodes count:\t", 3, "\n")
        cat("Input distribution:\t", distributionName, "\n")
        # cat("[xor with next] Transitions count:\t", transitionsCount, "\n")
        # cat("[xor with prev] Tasks count to perform:\t", tasksCount, "\n")
        # header("System characteristics")
        # cat("Idle time for 1st working node", sum(idleTimeForWorkingNodes[[1]]), "\n")
        # cat("Idle time for 2nd working node", sum(idleTimeForWorkingNodes[[2]]), "\n")
        # cat("Idle time for 3rd working node", sum(idleTimeForWorkingNodes[[3]]), "\n")
        # cat("Tasks performed in 1st working node", performanceLog[2, 1], "(transition number:", performanceLog[1, 1], ")\n")
        # cat("Tasks performed in 2nd working node", performanceLog[2, 2], "(transition number:", performanceLog[1, 2], ")\n")
        # cat("Tasks performed in 3rd working node", performanceLog[2, 3], "(transition number:", performanceLog[1, 3], ")\n")
        # rownames(performanceLog) <- c("Transition", "Tasks count", "Loading")
        # print(performanceLog)
        # subheader("Rejected tasks per node")
        cat("Node 1 rejected", rejectedTasks[[2]], "tasks\n")
        cat("Node 2 rejected", rejectedTasks[[3]], "tasks\n")
        cat("Node 3 rejected", rejectedTasks[[4]], "tasks\n")
        cat("Node 4 rejected", rejectedTasks[[5]], "tasks\n")
        cat("Node 5 rejected", rejectedTasks[[6]], "tasks\n")
        # subheader("Tasks performing per node")
        firstNode <- detailedPerformanceLog[[1]]
        secondNode <- detailedPerformanceLog[[2]]
        thirdNode <- detailedPerformanceLog[[3]]
        fourthNode <- detailedPerformanceLog[[4]]
        fifthNode <- detailedPerformanceLog[[5]]
        if (!is.null(firstNode)) {
            # cat("Node 1 task performing mean time: ", signif(mean(firstNode), 3), "\n")
            # cat("Node 1 tasks performing median time: ", signif(median(firstNode), 3), "\n")
        } else {
            firstNode <- NULL
        }

        if (!is.null(secondNode)) {
            # cat("Node 2 tasks performing mean time: ", signif(mean(secondNode), 3), "\n")
            # cat("Node 2 tasks performing median time: ", signif(median(secondNode), 3), "\n")
        } else {
            secondNode <- NULL
        }

        if (!is.null(thirdNode)) {
            # cat("Node 3 tasks performing mean time: ", signif(mean(thirdNode), 3), "\n")
            # cat("Node 3 tasks performing median time: ", signif(median(thirdNode), 3), "\n")
        } else {
            thirdNode <- NULL
        }

        if (!is.null(fourthNode)) {
            # cat("Node 4 tasks performing mean time: ", signif(mean(fourthNode), 4), "\n")
            # cat("Node 4 tasks performing median time: ", signif(median(fourthNode), 4), "\n")
        } else {
            fourthNode <- NULL
        }

        if (!is.null(fifthNode)) {
            # cat("Node 5 tasks performing mean time: ", signif(mean(fifthNode), 5), "\n")
            # cat("Node 5 tasks performing median time: ", signif(median(fifthNode), 5), "\n")
        } else {
            fifthNode <- NULL
        }

        firstNodeLoading <- mean(loadingForWorkingNodes[[1]])
        # cat("Loading of first working node is", firstNodeLoading, "\n")
        secondNodeLoading <- mean(loadingForWorkingNodes[[2]])
        # cat("Loading of second working node is", secondNodeLoading, "\n")
        thirdNodeLoading <- mean(loadingForWorkingNodes[[3]])
        # cat("Loading of third working node is", thirdNodeLoading, "\n")
        fourthNodeLoading <- mean(loadingForWorkingNodes[[4]])
        # cat("Loading of fourth working node is", fourthNodeLoading, "\n")
        fifthNodeLoading <- mean(loadingForWorkingNodes[[5]])
        # cat("Loading of fifth working node is", fifthNodeLoading, "\n")

        if (!is.null(firstNode)) {
            firstNodeEfficiency <- calculate_efficiency(performanceLog[3, 1], sum(idleTimeForWorkingNodes[[1]]))
            # cat("Efficiency of first working node is", firstNodeEfficiency, "\n")
        } else {
            firstNodeEfficiency <- 0
        }

        if (!is.null(secondNode)) {
            secondNodeEfficiency <- calculate_efficiency(performanceLog[3, 2], sum(idleTimeForWorkingNodes[[2]]))
            # cat("Efficiency of second working node is", secondNodeEfficiency, "\n")
        } else {
            secondNodeEfficiency <- 0
        }

        if (!is.null(thirdNode)) {
            thirdNodeEfficiency <- calculate_efficiency(performanceLog[3, 3], sum(idleTimeForWorkingNodes[[3]]))
            # cat("Efficiency of third working node is", thirdNodeEfficiency, "\n")
        } else {
            thirdNodeEfficiency <- 0
        }

        if (!is.null(fourthNode)) {
            fourthNodeEfficiency <- calculate_efficiency(performanceLog[3, 4], sum(idleTimeForWorkingNodes[[4]]))
            # cat("Efficiency of fourth working node is", fourthNodeEfficiency, "\n")
        } else {
            fourthNodeEfficiency <- 0
        }

        if (!is.null(fifthNode)) {
            fifthNodeEfficiency <- calculate_efficiency(performanceLog[3, 5], sum(idleTimeForWorkingNodes[[5]]))
            # cat("Efficiency of fifth working node is", fifthNodeEfficiency, "\n")
        } else {
            fifthNodeEfficiency <- 0
        }

        meanLoading = signif(median(c(firstNodeLoading, secondNodeLoading, thirdNodeLoading, fourthNodeLoading, fifthNodeLoading)), 3)
        # cat("Result system whole time: ", processTime, "\n")
        wholeSystemEfficiency =  mean(c(
            firstNodeEfficiency,
            secondNodeEfficiency,
            thirdNodeEfficiency,
            fourthNodeEfficiency,
            fifthNodeEfficiency
        ))
        # cat("Efficiency of whole system: ", signif(wholeSystemEfficiency, 3), "\n")
        cat("Mean loading of whole system: ", meanLoading, "\n")
        # cat("Mean time of processing task in whole system:", mean(timeForCyclesVector), "\n")
        cat("Rejected tasks in whole system:",
            rejectedTasks[[2]] + rejectedTasks[[3]] + rejectedTasks[[4]] + rejectedTasks[[5]] + rejectedTasks[[6]],
            "\n")
        cat(replicate(20, "="),"\n")

        # --- OUTPUT DATA ---

        # Total vector per task excecution time
        tasksExcecutionTimeVector
        # Detailed Performance Log for every processing unit
        detailedPerformanceLog
        # Total performed tasks count
        performedTasksCount
        # Summary performance log
        performanceLog
        # Total process time
        processTime
        # Time for cycles vector
        timeForCyclesVector
        # Rejected tasks
        rejectedTasks
        # Idle time for working nodes
        idleTimeForWorkingNodes
    }
    return(list(tasksExcecutionTimeVector,
                detailedPerformanceLog,
                performedTasksCount,
                meanLoading,
                processTime,
                timeForCyclesVector,
                rejectedTasks,
                idleTimeForWorkingNodes,
                performanceLog,
                wholeSystemEfficiency
    ))
}

# cat("\014") # Clear consosle output

transitionsCount <- 1000000

# INPUT DATA
binomD <- rbinom(1:30, size = 40, prob = 1 / 6)
poisD <- rpois(1:20, 24)
cuD <- runif(1:20, min = 1, max = 3)
pexpD <- pexp(1:20, rate = 1 / 3)
normD <- pnorm(1:20, mean = 2, sd = 25.2, lower.tail = FALSE)
chiD <- rchisq(1:20, df = 7)
FD <- rf(1:20, df1 = 5, df2 = 2)
studD <- rt(1:20, df = Inf) # !

nodesPerfs <- list(0.6, 0.6, 0.6, 0.6, 0.6)

# cuD
nodesGaps <- list(15, 17, 16, 16, 14)

# normD
# nodesGaps <- list(1, 1, 2, 1, 2)

# ExpD
# nodesGaps <- list(10, 9, 8, 6, 10)

# Pois
# nodesGaps <- list(25, 18, 22, 22, 20)

testDistribution <- c(
    14.617036, 23.499859, 15.674704, 10.719347, 7.892669, 8.942420, 40.888192,
    16.332921, 29.963656, 4.489176, 34.746372, 3.615892, 36.894031, 5.643181,
    3.785384, 43.331681, 29.505039, 7.454642, 27.730628, 41.550876, 10.137719,
    39.745254, 8.330701, 3.091769, 20.920556, 3.083225, 18.615411, 6.135353,
    35.041964, 18.802846, 39.236330, 3.182860, 19.411467, 44.857298, 23.516752,
    25.296108, 26.312003, 17.086546, 33.567694, 19.348448, 25.935871, 24.427297,
    45.007425, 3.539543, 4.235011, 2.501467, 19.954152, 31.347208, 6.746856, 9.348601
)

distributionName <- "непрерывное равномерное распределение"
# distributionName <- "нормальное распределение"
# distributionName <- "экспоненциальное распределение"
# distributionName <- "распределение Пуассона"

checkedDistribution <- unlist(lapply(cuD, function(it) {it * 10}))
# checkedDistribution <- unlist(lapply(normD, function(it) {it * 10}))
# checkedDistribution <- unlist(lapply(pexpD, function(it) {it * 10}))
# checkedDistribution <- poisD

print(checkedDistribution)

tasksCount <- 100

myPartialMain <- pryr::partial(
    main,
    inputFunc = APlus,
    outputFunc = AMinus,
    M = startingMarks,
    transitionsCount = transitionsCount,
    tasksCount = tasksCount,
    distribution = checkedDistribution,
    distributionName = distributionName,
    nodesPerfs = nodesPerfs,
    nodesGaps = nodesGaps
)

resultsRR <- myPartialMain(balancingMethod = "roundRobin")
resultsWRR <- myPartialMain(balancingMethod = "weightRoundRobin")
resultsRand <- myPartialMain(balancingMethod = "random")
resultsDW <- myPartialMain(balancingMethod = "dynamicWeightAlgorithm")

cat("\n[Excecution finished]")

resultsPath <- "~/Downloads"
algorithmsNamesVectors <- c("Циклический", "Весовой", "Случайный", "Динамич.\nвесов")
createDirIfNotExists <- function(dirName) {
    if (!file.exists(dirName)) {
        dir.create(
            path = dirName,
            showWarnings = TRUE,
            recursive = FALSE,
            mode = "0777"
        )
    }
    dirName
}
algorithmsInUseTitle <- "Используемые алгоритмы"
distributionInUseTitle <- paste("Используется ", distributionName, sep = "")
cat("\n[Working dir]:", resultsPath)

# Rejected tasks
if (TRUE) {
    rejTasksForRR <- sum(unlist(resultsRR[[7]][1:5]))
    rejTasksForWRR <- sum(unlist(resultsWRR[[7]][1:5]))
    rejTasksForRand <- sum(unlist(resultsRand[[7]][1:5]))
    rejTasksForDW <- sum(unlist(resultsDW[[7]][1:5]))
    resultsDirPath <- createDirIfNotExists(
        paste(resultsPath, "[results] rejectedTasks/", sep = "/")
    )
    # png(file = paste(resultsDirPath, "rejected-tasks", ".png", sep = ""),
    #     width = 800,
    #     height = 600,
    #     res = 140)

    rejectedData <- c(rejTasksForRR, rejTasksForWRR, rejTasksForRand, rejTasksForDW)
    barplot(
        height = rejectedData,
        width = 3,
        space = 0.1,
        names.arg = algorithmsNamesVectors,
        cex.names = 0.9,
        main = "Отброшенные заявки",
        density = c(1),
        sub = distributionInUseTitle,
        xlab = algorithmsInUseTitle,
        ylab = "Количество отброшенных заявок"
    )
    box(bty = "l")
    # dev.off()
}

# Mean system loading
if (FALSE) {
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] meanSysLoading/", sep = "/"))
    systemLoadingData <- c(resultsRR[[4]], resultsWRR[[4]], resultsRand[[4]], resultsDW[[4]])
    # png(
    #     file = paste(resultsDirPath, "summary-mean-loading", ".png", sep = ""),
    #     width = 800,
    #     height = 600,
    #     res = 140
    # )

    barplot(
        height = systemLoadingData,
        width = 3,
        space = 0.2,
        names.arg = algorithmsNamesVectors,
        cex.names = 0.9,
        main = "Средняя загруженность системы",
        density = c(1),
        sub = distributionInUseTitle,
        xlab = algorithmsInUseTitle,
        ylab = "Средняя загруженность системы"
    )
    box(bty = "l")
    # dev.off()
}

# Task excecution times plot (combiled)
if (FALSE) {
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] taskExcecutionTime/", sep = "/"))
    tasksExcecutionTimeVectorRR <- list(resultsRR[[1]], "Циклический алгоритм")
    tasksExcecutionTimeVectorWRR <- list(resultsWRR[[1]], "Весовой циклический алгоритм")
    tasksExcecutionTimeVectorRandom <- list(resultsRand[[1]], "Случайный алгоритм")
    tasksExcecutionTimeVectorDRR <- list(resultsDW[[1]], "Алгоритм динамических весов")

    for (algoritmData in list(tasksExcecutionTimeVectorRR,
                       tasksExcecutionTimeVectorWRR,
                       tasksExcecutionTimeVectorRandom,
                       tasksExcecutionTimeVectorDRR
                       )) {
        algorithm = algoritmData[[1]]
        algorithmName = algoritmData[[2]]

        # png(
        #     file = paste(resultsDirPath, algorithmName, ".png", sep = ""),
        #     width = 800,
        #     height = 600,
        #     res = 140
        # )

        plot(
            1:length(algorithm),
            algorithm,
            type = "n",
            main = paste("Тенденция времени выполнения заявок\n в узле-исполнителе (", algorithmName, ")", sep = ""),
            sub = distributionInUseTitle,
            xlab = "Время системы",
            ylab = "Время выполнения"
        )
        par(mar = c(2, 2, 2, 2), pin = c(5, 2))
        points(
            approx(1:length(algorithm), algorithm),
            type = "l",
            col = 1,
            lwd = 2
        )
        abline(h = median(algorithm), col = "blue")
        abline(h = max(algorithm), col = "green")
        abline(h = mean(algorithm), col = "red")

        # dev.off()
    }
}

# Tasks and node loading
if (FALSE) {
    detailedNodesLogRR <- list(resultsRR[[9]], "Циклический алгоритм")
    detailedNodesLogWRR <- list(resultsWRR[[9]], "Весовой алгоритм")
    detailedNodesLogRand <- list(resultsRand[[9]], "Случайный алгоритм")
    detailedNodesLogDW <- list(resultsDW[[9]], "Динамический весовой алгоритм")
    logsToIterate <-
        list(detailedNodesLogRR,
             detailedNodesLogWRR,
             detailedNodesLogRand,
             detailedNodesLogDW)
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] tasks&Loadings/", sep = "/"))
    for (oneAlgorithmData in logsToIterate) {
        png(
            file = paste( resultsDirPath, oneAlgorithmData[[2]], ".png", sep = "" ),
            width = 800,
            height = 600,
            res = 140
        )

        methodData <- oneAlgorithmData[[1]]
        methodName <- oneAlgorithmData[[2]]

        tasksCount <- methodData[2,]
        nodeLoading <- methodData[3,]

        nodesInfo <- cbind(nodeLoading, tasksCount)

        colnames(nodesInfo) <- c("Processed tasks", "Mean loading")
        rownames(nodesInfo) <- c("Узел №1", "Узел №2", "Узел №3", "Узел №4", "Узел №5")
        barplot(
            height = t(nodesInfo),
            main = "Заявки и нагрузка на узлы кластера",
            xlab = paste("Узлы РВС (", methodName, ")", sep = ""),
            col = c("lightgray", "darkgray"),
            sub = distributionInUseTitle,
            beside = TRUE
        )
        legend(
            x = "bottomleft",
            legend = c("Нагрузка", "Количество заявок"),
            bty = "o",
            text.width = 5,
            col = c("lightgray", "darkgray"),
            lwd = 10
        )
        box(bty = "l")
        # dev.off()
    }
}

# Whole time system working
if (FALSE) {
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] wholeSystemTime/", sep = "/"))
    wholeTimeWorkingData <- c(resultsRR[[5]], resultsWRR[[5]], resultsRand[[5]], resultsDW[[5]])
    # png(
    #     file = paste(resultsDirPath, "whole-time-working", ".png", sep = ""),
    #     width = 800,
    #     height = 600,
    #     res = 140
    # )

    barplot(
        height = wholeTimeWorkingData,
        width = 3,
        space = 0.2,
        names.arg = algorithmsNamesVectors,
        cex.names = 0.9,
        main = "Общее время работы системы",
        density = c(1),
        sub = distributionInUseTitle,
        xlab = algorithmsInUseTitle,
        ylab = "Время работы системы"
    )
    box(bty = "l")

    # dev.off()
}

# Times of tasks being in system
if (FALSE) {
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] taskBeingInThaSystem/", sep = "/"))
    taskInSystemTimeVectorRR <- list(resultsRR[[6]], "Циклический алгоритм")
    taskInSystemTimeVectorWRR <- list(resultsWRR[[6]], "Весовой циклический алгоритм")
    taskInSystemTimeVectorRandom <- list(resultsRand[[6]], "Случайный алгоритм")
    taskInSystemTimeVectorDRR <- list(resultsDW[[6]], "Алгоритм динамических весов")
    taskInSystemList <- list(
        taskInSystemTimeVectorRR,
        taskInSystemTimeVectorWRR,
        taskInSystemTimeVectorRandom,
        taskInSystemTimeVectorDRR
    )
    for (algoritmData in taskInSystemList) {
        algorithm = algoritmData[[1]]
        algorithmName = algoritmData[[2]]
        # png(
        #     file = paste(resultsDirPath, algorithmName, ".png", sep = ""),
        #     width = 800,
        #     height = 600,
        #     res = 140
        # )
        plot(
            1:length(algorithm),
            algorithm,
            type = "n",
            main = paste("Тенденция времени нахождения заявки в системе\n (", algorithmName, ")", sep = ""),
            sub = distributionInUseTitle,
            xlab = "Поступление заявок (время системы)",
            ylab = "Время нахождения заявки в системе"
        )
        par(mar = c(2, 2, 2, 2), pin = c(5, 2))
        points(
            approx(1:length(algorithm), algorithm),
            type = "l",
            col = 1,
            lwd = 2
        )
        abline(h = median(algorithm), col = "blue")
        abline(h = max(algorithm), col = "green")
        abline(h = mean(algorithm), col = "red")

        # dev.off()
    }
}

# Efficency of system
if (FALSE) {
    efficencyForRR <- resultsRR[[10]]
    efficencyForWRR <- resultsWRR[[10]]
    efficencyForRand <- resultsRand[[10]]
    efficencyForDW <- resultsDW[[10]]
    resultsDirPath <- createDirIfNotExists(paste(resultsPath, "[results] efficency/", sep = "/"))
    wholeEfficency <- c(efficencyForDW, efficencyForWRR, efficencyForRand, efficencyForDW)

    # signif(wholeSystemEfficiency, 3)

    # png(
    #     file = paste(resultsDirPath, "summary-efficency", ".png", sep = ""),
    #     width = 800,
    #     height = 600,
    #     res = 140
    # )
    barplot(
        height = wholeEfficency,
        width = 3,
        space = 0.2,
        names.arg = algorithmsNamesVectors,
        cex.names = 0.9,
        main = "Эффективность системы",
        density = c(1),
        sub = distributionInUseTitle,
        xlab = algorithmsInUseTitle,
        ylab = "Эффективность системы"
    )
    box(bty = "l")
    # dev.off()
}

# Mean time for performing task in system
if (FALSE) {
    taskInSystemTimeVectorRR <- resultsRR[[6]]
    taskInSystemTimeVectorWRR <- resultsWRR[[6]]
    taskInSystemTimeVectorRandom <- resultsRand[[6]]
    taskInSystemTimeVectorDRR <- resultsDW[[6]]

    resultsDirPath <- createDirIfNotExists(
        paste(resultsPath, "[results] meanTaskTimeInSystem/", sep = "/")
    )
    meanTaskTimes <- c(
        median(taskInSystemTimeVectorRR),
        median(taskInSystemTimeVectorWRR),
        median(taskInSystemTimeVectorRandom),
        median(taskInSystemTimeVectorDRR)
    )

    # png(
    #     file = paste(resultsDirPath, "mean-task-performing-in-system", ".png", sep = ""),
    #     width = 800,
    #     height = 600,
    #     res = 140
    # )

    barplot(
        height = meanTaskTimes,
        width = 3,
        space = 0.2,
        names.arg = algorithmsNamesVectors,
        cex.names = 0.9,
        main = "Среднее время нахождения заявки в системе",
        density = c(1),
        sub = distributionInUseTitle,
        xlab = algorithmsInUseTitle,
        ylab = "Среднее время нахождения заявки в системе"
    )
    box(bty = "l")

    # dev.off()
}

# Сделать эксперимент с меняющимися настройками сервера во времени
