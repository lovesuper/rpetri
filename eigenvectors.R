A <- matrix(
    c(1.638, 2.590, 2.795, 3.180,
      3.380, 5.347, 5.458, 5.584,
      5.659, 0.000, 0.000, 0.000,
      7.537, 8.217, 8.392, 10.082)
    , nrow = 4, ncol = 4, byrow = TRUE)

B <- matrix(
    c(1.638, 2.590, 2.795, 3.180,
      3.380, 5.347, 5.458, 5.584,
      5.659, 0.000, 0.000, 0.000,
      7.537, 8.217, 8.392, 10.082,
      0.000, 10.709, 11.901, 13.064),
    nrow = 5, ncol = 4, byrow = FALSE)

G <- matrix(
    c(1.638, 2.590, 2.795, 3.180, 0.05,
      3.380, 5.347, 5.458, 5.584, 0.06,
      5.659, 0.000, 0.000, 0.000, 0.07,
      7.537, 8.217, 8.392, 10.082, 0.08,
      0.000, 10.709, 11.901, 13.064, 0.09),
    nrow = 5, ncol = 5, byrow = TRUE)

getRandomSquareMatrix <- function(x) {
    matrix(
        runif(x^2, 0, 10),
        nrow = x,
        ncol = x,
        byrow = TRUE
    )
}

calculateMatrixCoef <- function(m) {
    eigenResult <- eigen(t(m))
    firstVector <- eigenResult$vectors[, 1]
    sumOfVector <- sum(firstVector)
    f <- function(e) {
        e / sumOfVector
    }
    lapply(firstVector, f)
}

result <- calculateMatrixCoef(
    # getRandomSquareMatrix(5)
    G
    )
print(result)
