# (a) Function that prints 2^3
Power <- function() {
  print(2^3)
}

# Calling Power() should print 8
Power()

# (b) Function that takes any x and a and prints x^a
Power2 <- function(x, a) {
  print(x^a)
}

# Example: calling Power2(3,8) prints 6561
Power2(3, 8)

# (c) Using Power2() to compute 10^3, 8^17, and 13^13
Power2(10, 3)   # prints 1000
Power2(8, 17)   # prints 8 raised to the 17th power
Power2(13, 13)  # prints 13 raised to the 13th power

# (d) Function that returns x^a as an R object
Power3 <- function(x, a) {
  result <- x^a
  return(result)
}

# Example: storing the result in an object
res <- Power3(3, 8)
print(res)  # should print 6561

# (e) Using Power3() to create a plot of f(x) = x^2 for x in 1:10
x <- 1:10
y <- Power3(x, 2)
plot(x, y, 
     main = "Plot of f(x) = x^2", 
     xlab = "x", 
     ylab = expression(x^2),
     # Uncomment one of the following to display a log-scale:
     # log = "x" or log = "y" or log = "xy"
     log = "")  # default linear scale

# (f) Function to plot x against x^a for a fixed exponent a
PlotPower <- function(x, a) {
  y <- Power3(x, a)
  plot(x, y, 
       main = paste("Plot of f(x) = x^", a, sep=""), 
       xlab = "x", 
       ylab = paste("x^", a, sep=""))
}

# Example: Calling PlotPower with x as 1:10 and a = 3
PlotPower(1:10, 3)
