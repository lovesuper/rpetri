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
    a <- solve_LSAP(matrix) # <- true resolving!!!
    print(a)
    f <- function(z) {
        z[1]
    }

    print(lapply(a, f))

    solve_LSAP(matrix, maximum = TRUE)

    ## To get the optimal value (for now):
    y <- solve_LSAP(matrix)
    z <- sum(matrix[cbind(seq_along(y), y)])

    return(z)
}

result <- resolve(xx)
#
# print(result)
# require(graphics)
#
# 1 - pt(1:5, df = 1)
# qt(.975, df = c(1:10,20,50,100,1000))
#
# tt <- seq(0, 10, len = 21)
# ncp <- seq(0, 6, len = 31)
# ptn <- outer(tt, ncp, function(t, d)
#     pt(t, df = 3, ncp = d))
# t.tit <- "Non-central t - Probabilities"
# image(tt, ncp, ptn, zlim = c(0, 1), main = t.tit)
# persp(
#     tt,
#     ncp,
#     ptn,
#     zlim = 0:1,
#     r = 2,
#     phi = 20,
#     theta = 200,
#     main = t.tit,
#     xlab = "t",
#     ylab = "non-centrality parameter",
#     zlab = "Pr(T <= t)"
# )
#
# plot(
#     function(x)
#         dt(x, df = 3, ncp = 2),
#     -3,
#     11,
#     ylim = c(0, 0.32),
#     main = "Non-central t - Density",
#     yaxs = "i"
# )

# for (i in 1:12) {
#     cat(dbinom(i, size=12, prob=0.2), "\n")
# }
#



























