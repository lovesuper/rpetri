# Matrix for test
G <- matrix(
    c(1.638, 2.590, 2.795, 3.180, 0.05,
      3.380, 5.347, 5.458, 5.584, 0.06,
      5.659, 0.000, 0.000, 0.000, 0.07,
      7.537, 8.217, 8.392, 10.082, 0.08,
      0.000, 10.709, 11.901, 13.064, 0.09),
    nrow = 5, ncol = 5, byrow = TRUE)

getRandomSquareMatrix <- function(x) {
    fullyRandomX <-  runif(x^2, 0, 10)
    A <- 0
    B <- 1.0:9.99
    L <- sapply(list(A, B), length)
    fullyRandomX <- sample(c(A, B),
                size = 20,
                prob = rep(c(1/40, 1/2) / L, L),
                replace = TRUE)
    round(
        matrix(fullyRandomX, nrow = x, ncol = x, byrow = TRUE),
        digits = 3
    )
}

calculateMatrixCoef <- function(m) {
    eigens <- eigen(t(m))
    vector <- eigens$vectors[, 1]
    sumOfVector <- sum(vector)

    f <- function(e) {
        round(
            Re(e / sumOfVector), digits = 3 # real part
        ) # some rounding for clearity
    }
    unlist(lapply(vector, f))
}

m <- getRandomSquareMatrix(5)
cat("Matrix is:\n"); print(m)

result <- calculateMatrixCoef(m)
cat("Result is:\n"); print(result)
