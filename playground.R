require("clue")

x <- matrix(c(5, 1, 4, 3, 5, 2, 2, 4, 4), nrow = 3)
xx <- matrix(
    c(3, 6, 0, 2,
      0, 2, 0, 2,
     19, 0, 4, 0,
      0, 2, 6, 0),
    ncol = 4,
    nrow = 4,
    byrow = TRUE
)

# Optimal assignment: 1 => 3, 2 => 1, 3 => 2, 4 => 4

resolve <- function(matrix) {
    solve_LSAP(matrix) # <- true resolving!!!
    solve_LSAP(matrix, maximum = TRUE)

    ## To get the optimal value (for now):
    y <- solve_LSAP(matrix)
    z <- sum(matrix[cbind(seq_along(y), y)])

    return(z)
}

result <- resolve(xx)

print(result)
