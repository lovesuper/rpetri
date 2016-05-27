# Эксперимент с сетью Петри

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
    ), nrow=10, ncol=9)

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
  ), nrow=10, ncol=9)

conflictedTransitions = matrix(
  c(
    0,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0
  )
  , nrow=9, ncol=1)

startingMarks = matrix(c( 1, 0, 1, 0, 1, 0, 1, 0, 0, 0), nrow=1, ncol=10)

getAllowedTransitions <- function(currentState, inputFunc) {
  transposedInputFunc <- t(inputFunc)
  rowsCount <- nrow(inputFunc)
  colsCount <- ncol(inputFunc)
  transposedMatrix <- matrix(NA, nrow=10, ncol=9)
  transitionIndicatorVector <- vector('integer')
  for (i in 2:rowsCount - 1) {
    transposedMatrix[,i] <- t(transposedInputFunc[i,])
  }
  allowedTransitions <- 0
  for (i in 2:rowsCount) {
    transposedRow <- t(transposedMatrix[i,])
    transitionIndicatorVector[i] <- 0
    for (j in 2:colsCount) {
    
      if (currentState[j] - transposedRow[j] >= 0) {
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

print(getAllowedTransitions(t(startingMarks), APlus))


















