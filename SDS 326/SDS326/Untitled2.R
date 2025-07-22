# (a) 
Power <- function() {
  result <- 2^3
  print(result)
}
Power()

# (b) 
Power2 <- function(x, a) {
  result <- x^a
  print(result)
}
Power2(3, 8)

# (c) 
Power2(10, 3)   # Computes 10^3
Power2(8, 17)   # Computes 8^17
Power2(13, 13)  # Computes 13^13

# (d) 
Power3 <- function(x, a) {
  result <- x^a
  return(result)
}
value <- Power3(3, 8)
print(value)  

# (e) 
x <- 1:10
y <- Power3(x, 2)
plot(x, y, xlab = "x", ylab = "x^2", main = "Plot of f(x) = x^2")

# (f) 
PlotPower <- function(x, a) {
  y <- Power3(x, a)
  plot(x, y, xlab = "x", ylab = paste("x^", a, sep=""), 
       main = paste("Plot of f(x) = x^", a, sep=""))
}
PlotPower(1:10, 3)



set.seed(123)

# (a)
n <- 10000
x <- runif(n)
f_a <- sum(x >= 0.55 & x <= 0.65) / n
f_a

# (b)
x1 <- runif(n)
x2 <- runif(n)
f_b <- sum(x1 >= 0.55 & x1 <= 0.65 & x2 >= 0.30 & x2 <= 0.40) / n
f_b

# (c)
f_c <- 0.1^100
f_c

# (d)
print("In high dimensions, very few observations fall within a fixed local region. For example, in 1D about 10% are near a test point, in 2D about 1%, and in 100D nearly 0 (10^-100). This scarcity of nearby points makes KNN unreliable.")

# (e)
pvals <- c(1, 2, 100)
s <- 0.1^(1/pvals)
res <- data.frame(p = pvals, side = s)
res

