require("clue")

xx <- matrix(
    c(6, 4, 7,
      2, 5, 8,
      3, 6, 9),
    ncol = 3,
    nrow = 3,
    byrow = TRUE
)
y <- solve_LSAP(xx, maximum = TRUE)
extractList <- function(it) { it[1] }
t <- lapply(y, extractList)
cat(unlist(t))
