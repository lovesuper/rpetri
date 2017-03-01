m <- matrix(c(-1, -6, 2, 6), ncol = 2, nrow = 2, byrow = TRUE)
e <- eigen(m)

# val <- e$values[2]
# vec <- cbind(e$vectors[,2])
#
# print(m %*% vec)
# print(val * vec)
#
# val <- e$values[1]
# vec <- cbind(e$vectors[,1])
#
# print(m %*% vec)
# print(val * vec)
# k <- matrix(c(-1, -6, 2, 6), ncol = 2, nrow = 2, byrow = TRUE)
# kk <- c(2, -1)
# one <- k %*% kk
# two <- 2 * kk
# print(one)
# print(two)

# l <- runif(16, 0, 10)
m <- matrix(c(1,1,-2,
              -1,2,1,
              0,1,-1), nrow = 3, ncol = 3, byrow = TRUE)
print("for matrix:")
print(m)

# print("right vec:")
rightE <- eigen(m)
r1 <- rightE$vectors
# print(r1)

# print("left vec:")
leftE <- eigen(t(m))
l1 <- leftE$vectors
# print(l1)

print("vals")
zapsmall((m %*% r1)/r1) ## right e'vec
zapsmall(t(l1) %*% m/(t(l1)))
print(rightE$values)
print(rightE$vectors)
