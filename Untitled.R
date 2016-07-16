cat("\014") # Clear consosle output

# points(approx(x, y, method = "constant"), col = 4, pch = "*")

# f <- approxfun(x, y)
# curve(f(x), 0, 11, col = "green2")
# points(x, y)
# is.function(fc <- approxfun(x, y, method = "const")) # TRUE
# curve(fc(x), 0, 10, col = "darkblue", add = TRUE)
# ## different extrapolation on left and right side :
# plot(approxfun(x, y, rule = 2:1), 0, 11,
#      col = "tomato", add = TRUE, lty = 3, lwd = 2)
#
# ## Show treatment of 'ties' :
#
# x <- c(2,2:4,4,4,5,5,7,7,7)
# y <- c(1:6, 5:4, 3:1)
# approx(x, y, xout = x)$y # warning
# (ay <- approx(x, y, xout = x, ties = "ordered")$y)
# stopifnot(ay == c(2,2,3,6,6,6,4,4,1,1,1))
# approx(x, y, xout = x, ties = min)$y
# approx(x, y, xout = x, ties = max)$y
# dat <- data.frame(x=1:10, y=(1:10)^2)
# a <- approx(dat$x, dat$y)
# af <- approxfun(dat$x, dat$y)
#
# plot(dat)
# points(a, pch=2)
# plot(dat)
# curve(af, add=TRUE)


# MAKING FIRST PLOT

# g_range <- range(0, lambdasList, 1)
# plot(
#     lambdasList,
#     type = "o",
#     col = "black",
#     ylim = g_range
# )
# smoothingSpline = smooth.spline(lambdasList, spar=0.35)

# Make x axis using Mon-Fri labels
# axis(1,
#      at = 1:5,
#      lab = c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Make y axis with horizontal labels that display ticks at
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
# axis(2, las = 1, at = 4 * 0:g_range[2])

# Create box around plot
# box()

# Graph trucks with red dashed line and square points
# lines(
#     secondNode,
#     type = "o",
#     pch = 22,
#     lty = 2,
#     col = "red"
# )

# Create a title with a red, bold/italic font
# title(main = "Время нахождения заявки в системе",
#       col.main = "black",
#       font.main = 4)

# Label the x and y axes with dark green text
# title(xlab = "Time", col.lab = rgb(0, 0.5, 0))
# title(ylab = "Value", col.lab = rgb(0, 0.5, 0))

# Create a legend at (1, g_range[2]) that is slightly smaller
# (cex) and uses the same line colors and points used by
# the actual plots
# legend(
#     1,
#     g_range[2],
#     c("Время прохода"),
#     cex = 0.8,
#     col = c("black"),
#     pch = 21:22,
#     lty = 1:2
# )

# dat <- data.frame(x=1:length(lambdasList), y=lambdasList)
# a <- approx(dat$x, dat$y)
# af <- approxfun(dat$x, dat$y)
# points(a, pch=2)
# curve(af, add=TRUE)
# plot(dat)

# dat <- c(10,20,15,135)
# hist(dat, breaks = "Sturges")
# barplot(dat, names.arg=c("RR", "WRR", "Random", "DRR"))









#
# binomD <- rbinom(1:10, size = 50, prob = 1 / 2)
# poisD <- rpois(1:30, 40)
# cuD <- runif(1:20, min = 1, max = 50)
# pexpD <- pexp(1:20, rate = 1 / 3)
# normD <- pnorm(1:30, mean = 5, sd = 5.2, lower.tail = FALSE)
# chiD <- rchisq(1:20, df = 7)
# FD <- rf(1:20, df1 = 5, df2 = 2)
# studD <- rt(1:20, df = Inf) # !
#
#
#
#
# print(normD)

require(EMD)
# y <- c(0, 1, 2, 1, -1, 1:4, 5, 6, 0, -4, -6, -5:5, -2:2)
# y <- c(0, 0, 0, 1, -1, 1:4, 4, 4, 0, 0, 0, -5:5, -2:2, 2, 2)
y <- c(0, 0, 0, 1, -1, 1:4, 4, 4, 0, 0, 0, -5:5, -2:2, 0, 0)

plot(y, type = "b"); abline(h = 0)
extrema(y)
