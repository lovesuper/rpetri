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
    ), nrow=9, ncol=10, byrow=TRUE)

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

print(getAllowedTransitions(t(startingMarks), APlus))
















