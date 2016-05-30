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
  ncol = 10,
  byrow = TRUE
)

testRequestsWeights = matrix(
    c(1, 3, 2, 5, 9, 3, 6, 4, 9, 7, 4, 8, 4, 9, 6, 3, 4, 2, 1, 1),
    nrow = 1,
    ncol = 20,
    byrow = TRUE
)
