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

print("Matrix A values:")
eigenvalsA <- eigen(A, only.values = TRUE)
print(eigenvalsA)

print("Matrix G values and vectors:")
eigen <- eigen(t(G))
print(eigen)
print("egenvec for first eigenval")
vector <- eigen$vectors[, 1]

print(vector)

print("sum of values")

s <- sum(vector[1], vector[2], vector[3], vector[4])

print(s)
print("first")
print(vector[1] / s)
print("second")
print(vector[2] / s)
print("third")
print(vector[3] / s)
print("fourth")
print(vector[4] / s)
