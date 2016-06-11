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
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1
    ), nrow = 9, ncol = 10, byrow = TRUE)

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
    ), nrow = 9, ncol = 10, byrow = TRUE)

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

testRequestsWeights = matrix(
    c(10, 3, 20, 5, 9, 30, 6, 4, 90, 7, 4, 80, 4, 90, 6, 3, 40, 2, 10, 10),
    nrow = 1,
    ncol = 20,
    byrow = TRUE
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
    # 3
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
        if (length(systemHistory[[i]]) < 3) {
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

        performanceHistory <- matrix(
            c(tail(f, 3)[1], tail(f, 3)[2], tail(f, 3)[3],
              tail(s, 3)[1], tail(s, 3)[2], tail(s, 3)[3],
              tail(t, 3)[1], tail(t, 3)[2], tail(t, 3)[3]),
            ncol = 3,
            nrow = 3,
            byrow = TRUE
        )
        # print(performanceHistory)
        optimalAssignment <- solve_LSAP(performanceHistory)
        # cat("\n")

        extractList <- function(it) { it[1] }

        scheduleList <- lapply(optimalAssignment, extractList)

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
resolveConflict <- function(method, nodes, iteration, systemHistory, scheduleList) {
    if (method == "random") {
        randomBalancing(nodes)
    } else if (method == "roundRobin") {
        roundRobin(nodes, iteration)
    } else if (method == "weightRoundRobin") {
        weightRoundRobin(nodes, iteration)
    } else if (method == "leastConnectoins") {
        pickNodeWithLeastConnections()
    } else if (method == "dynamicWeightAlgorithm") {
        dynamicWeightAlgorithm(systemHistory, scheduleList, iteration, nodes)
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
getTransitionNumberWithResolving <- function(nodes, iteration, method, systemHistory, scheduleList) {
    vectorLength <- length(nodes)
    q <- 1
    if (all(nodes == conflictedTransitions)) {
        conflictedNodes <- getNodesIds(nodes)
        transitionNumber <-
            resolveConflict(method, conflictedNodes, iteration, systemHistory, scheduleList)
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
             rejectedTasks) {
        lambda <- 0.1
        transitionTime <- 0.0
        currentPerformer <- NA
        if (transition == 1) {
            # Starting transition
            # Need to make a custom generator here in another words --
            # custom distribution
            lambda <- 0.1
        } else if (transition == 2) {
            # First node
            lambda <- 0.5 * taskWeight
            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= 30) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
            }
            currentPerformer <- transition
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
        } else if (transition == 3) {
            # Second node
            lambda <- 0.5 * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= 20) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
            }

            currentPerformer <- transition
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
        } else if (transition == 4) {
            # Third node
            lambda <- 0.5 * taskWeight

            nodesState[[transition]] <- nodesState[[transition]] + lambda
            if (nodesState[[transition]] >= 25) {
                # skip this task
                rejectedTasks[[transition]] = rejectedTasks[[transition]] + 1
                nodesState[[transition]] <- 0
            } else {
            }

            currentPerformer <- transition
            detailedPerformanceLog[[transition - 1]] <- c(detailedPerformanceLog[[transition - 1]], c(lambda))
        } else if (transition == 5) {
            #
            lambda <- 0.1
        } else if (transition == 6) {
            lambda <- 0.1
        } else if (transition == 7) {
            lambda <- 0.1
        } else if (transition == 8) {
            lambda <- 0.1
        } else if (transition == 9) {
            lambda <- 0.1
        }

        # transitionTime <- getRandomTimeForLambda(lambda)
        transitionTime <- lambda
        processTime <- processTime + transitionTime

        list(processTime, detailedPerformanceLog, nodesState, rejectedTasks, currentPerformer)
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
    statsMatrix <- matrix(0, 3, length(nodes))
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
main <- function(inputFunc, outputFunc, M, transitionsCount, tasksCount) {
    commonFunc <- outputFunc - inputFunc
    performedTasksCount <- 0

    # Summary performance log for every processing unit
    performanceLog <- matrix(c(0, 0, 0), nrow = 3, ncol = 1, byrow = TRUE)

    # Detailed Performance Log for every processing unit
    detailedPerformanceLog <- list(c(), c(), c())
    processLogForEveryTask <- c()
    processTime <- 0.0
    timeForCycle <- 0.0
    nodesState <- list(0, 0, 0, 0, 0, 0, 0, 0, 0)
    rejectedTasks <- list(0, 0, 0, 0, 0, 0, 0, 0, 0)
    idleTimeForWorkingNodes <- list(c(), c(), c(), c(), c(), c(), c())
    currentPerformer <- NA
    scheduleList <- NULL
    transitionNumber <- NA
    balancingMethod <- "dynamicWeightAlgorithm"
    # balancingMethod <- "roundRobin"
    # balancingMethod <- "random"
    for (i in 1:transitionsCount) {
        transposedVector <- t(M)
        allowedTransitions <- getAllowedTransitions(transposedVector, inputFunc)
        result <- getTransitionNumberWithResolving(
            allowedTransitions,
            i,
            balancingMethod,
            detailedPerformanceLog,
            scheduleList
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
                currentTaskWeight <- testRequestsWeights[performedTasksCount %% length(testRequestsWeights) + 1]
                performedTasksCount <- performedTasksCount + 1
                #cat("[main] Task number: ", tasksCount, "\n")
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
        list[newProccesTime, detailedPerformanceLog, newNodesState, newRejectedTasks, newCurrentPerformer] <- performTransition(
            transitionNumber,
            timeForCycle,
            currentTaskWeight,
            detailedPerformanceLog,
            nodesState,
            rejectedTasks
        )
        if (!is.na(newCurrentPerformer)) {
            currentPerformer <- newCurrentPerformer
        }

        nodesState <- newNodesState
        rejectedTasks <- newRejectedTasks
        timeForCycle <- newProccesTime

        startingVector <- createStartingVector(allowedTransitions)
        if (transitionNumber == nrow(commonFunc)) {
            # cat("[main] Idle time for loop:",
            #     timeForCycle,
            #     "minus",
            #     tail(detailedPerformanceLog[[currentPerformer - 1]], 1),
            #     "working node\t№", currentPerformer,
            #     "\n"
            # )
            idleTimeForLoop <- timeForCycle - tail(detailedPerformanceLog[[currentPerformer - 1]], 1)
            idleTimeForWorkingNodes[[currentPerformer - 1]] <- c(
                idleTimeForWorkingNodes[[currentPerformer - 1]], idleTimeForLoop
            )
            # cat("[main] Idle time for loop:", idleTimeForLoop,
            #     "working node\t№", currentPerformer, "\n"
            # )
            processLogForEveryTask <- c(processLogForEveryTask, c(timeForCycle))
            processTime <- processTime + timeForCycle
            timeForCycle <- 0.0
        }

        M <- t(startingVector) %*% commonFunc + M
        if (tasksCount > 0 && performedTasksCount == tasksCount) {
            #cat("[main] Tasks are closed\n")
            break()
        }
    }

    if (TRUE) {
        workingNodesCount <- 3
        # Results
        # cat("\014") # Clear consosle output
        header("System configuration")

        cat("Balancing method:\t", balancingMethod, "\n")
        cat("Working nodes count:\t", workingNodesCount, "\n")
        cat("Input distribution:\t", "`Unknown value`", "\n")
        cat("Transitions count:\t", transitionsCount, "\n")
        cat("Tasks count to perform:\t", tasksCount, "\n")

        header("System characteristics")

        subheader("Idle time")

        cat("Idle time for 1st working node", sum(idleTimeForWorkingNodes[[1]]), "\n")
        cat("Idle time for 2nd working node", sum(idleTimeForWorkingNodes[[2]]), "\n")
        cat("Idle time for 3rd working node", sum(idleTimeForWorkingNodes[[3]]), "\n")

        subheader("Main characteristics")

        cat("Tasks performed in 1st working node", performanceLog[2, 1], "(transition number:", performanceLog[1, 1], ")\n")
        cat("Tasks performed in 2nd working node", performanceLog[2, 2], "(transition number:", performanceLog[1, 2], ")\n")
        cat("Tasks performed in 3rd working node", performanceLog[2, 3], "(transition number:", performanceLog[1, 3], ")\n")

        # rownames(performanceLog) <- c("Transition", "Tasks count", "Loading")
        # print(performanceLog)

        subheader("Rejected tasks per node")

        cat("Node 1 rejected", rejectedTasks[[2]], "tasks\n")
        cat("Node 2 rejected", rejectedTasks[[3]], "tasks\n")
        cat("Node 3 rejected", rejectedTasks[[4]], "tasks\n")

        subheader("Tasks performing per node")

        firstNode <- detailedPerformanceLog[[1]]
        secondNode <- detailedPerformanceLog[[2]]
        thirdNode <- detailedPerformanceLog[[3]]
        if (!is.null(firstNode)) {
            cat("Node 1 task performing mean time: ", signif(mean(firstNode), 3), "\n")
            cat("Node 1 tasks performing median time: ", signif(median(firstNode), 3), "\n")
        } else {
            firstNode <- NULL
        }

        if (!is.null(secondNode)) {
            cat("Node 2 tasks performing mean time: ", signif(mean(secondNode), 3), "\n")
            cat("Node 2 tasks performing median time: ", signif(median(secondNode), 3), "\n")
        } else {
            secondNode <- NULL
        }

        if (!is.null(thirdNode)) {
            cat("Node 3 tasks performing mean time: ", signif(mean(thirdNode), 3), "\n")
            cat("Node 3 tasks performing median time: ", signif(median(thirdNode), 3), "\n")
        } else {
            thirdNode <- NULL
        }
        subheader("Loading per nodes")

        firstNodeLoading <- calculate_loading(performanceLog[3, 1], sum(idleTimeForWorkingNodes[[1]]))
        cat("Loading of first working node is", firstNodeLoading, "\n")

        secondNodeLoading <- calculate_loading(performanceLog[3, 2], sum(idleTimeForWorkingNodes[[2]]))
        cat("Loading of second working node is", secondNodeLoading, "\n")

        thirdNodeLoading <- calculate_loading(performanceLog[3, 3], sum(idleTimeForWorkingNodes[[3]]))
        cat("Loading of third working node is", thirdNodeLoading, "\n")

        subheader("Efficiency per nodes")

        if (!is.null(firstNode)) {
            firstNodeEfficiency <- calculate_efficiency(performanceLog[3, 1], sum(idleTimeForWorkingNodes[[1]]))
            cat("Efficiency of first working node is", firstNodeEfficiency, "\n")
        } else {
            firstNodeEfficiency <- 0
        }

        if (!is.null(secondNode)) {
            secondNodeEfficiency <- calculate_efficiency(performanceLog[3, 2], sum(idleTimeForWorkingNodes[[2]]))
            cat("Efficiency of second working node is", secondNodeEfficiency, "\n")
        } else {
            secondNodeEfficiency <- 0
        }

        if (!is.null(thirdNode)) {
            thirdNodeEfficiency <- calculate_efficiency(performanceLog[3, 3], sum(idleTimeForWorkingNodes[[3]]))
            cat("Efficiency of third working node is", thirdNodeEfficiency, "\n")
        } else {
            thirdNodeEfficiency <- 0
        }

        subheader("Whole system result")

        cat("Result system whole time: ", processTime, "\n")

        wholeSystemEfficiency =  mean(c(firstNodeEfficiency, secondNodeEfficiency, thirdNodeEfficiency))
        cat("Efficiency of whole system: ", signif(wholeSystemEfficiency, 3), "\n")
        cat("Mean loading of whole system: ", signif(median(
            c(median(firstNodeLoading), median(secondNodeLoading), median(thirdNodeLoading))
        ), 3), "\n")
        cat("Mean time of processing task in whole system:", mean(processLogForEveryTask), "\n")
        cat("Rejected tasks in whole system:", rejectedTasks[[1]] + rejectedTasks[[2]] + rejectedTasks[[3]], "\n")

        # makeAPlot(firstNode, secondNode)
    }
}

inputDistribution <- "hola!" # implement mech of input distribution
transitionsCount <- 100000
tasksCount <- 100

main(APlus,
     AMinus,
     startingMarks,
     transitionsCount,
     tasksCount)





















