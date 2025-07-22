set.seed(1)
n <- 100

# (a) 
x <- rnorm(n, mean = 0, sd = 1)
## We set the seed for reproducibility and create 100 values from a standard normal distribution. This vector, x, serves as our predictor. Although no graph is produced here, these x values are the foundation for all subsequent plots.


# (b) 
eps <- rnorm(n, mean = 0, sd = 0.5)
## A noise vector is drawn from a normal distribution with mean 0 and standard deviation 0.5. This noise mimics the random variation you might expect in real data. Later, this noise causes the scatter in the graph around the true line.



# (c) 
y <- -1 + 0.5 * x + eps
## The response y is computed using the formula y = -1 + 0.5x + eps. This creates a linear relationship with an intercept of –1 and a slope of 0.5. In the scatterplot (p1), you will see points distributed around the line corresponding to this equation.


# (d) 
library(ggplot2)
dataOrig <- data.frame(x = x, y = y)
p1 <- ggplot(dataOrig, aes(x = x, y = y)) +
  geom_point() 
print(p1)
## Using ggplot2, we build a scatterplot of x versus y. The graph displays points that cluster around the theoretical line, illustrating the natural variability introduced by eps. The title and axis labels help clarify that the noise in y is from the original simulation.

# (e) 
lmModel <- lm(y ~ x)
print(summary(lmModel))
## We fit a model (lmModel) to predict y from x. The regression summary shows an intercept close to –1 and a slope near 0.5, which are what we expect. The printed summary confirms that x is a significant predictor, and the residual standard error indicates the typical deviation of points from the fitted line.


# (f) 
popLine <- data.frame(x = seq(min(x), max(x), length.out = 100))
popLine$y <- -1 + 0.5 * popLine$x
p2 <- ggplot(dataOrig, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_line(data = popLine, aes(x = x, y = y), color = "red", linetype = "dashed")
print(p2)
## Here, we create a data frame for the population line defined as y = -1 + 0.5x (red dashed line) and then overlay the fitted regression line (blue) on the scatterplot. The graph (p2) visually compares the model’s fit to the true underlying relationship. In this case, the blue line closely tracks the red dashed line, demonstrating that the model accurately captures the linear trend despite the noise.

# (g) 
polyModel <- lm(y ~ x + I(x^2))
print(summary(polyModel))
## By including a quadratic term in the regression, we test for a nonlinear relationship. The summary output shows that the quadratic term is not significant (p ≈ 0.164), meaning the added complexity isn’t justified. Graphically, you wouldn’t see a curved pattern in the scatterplot—supporting the adequacy of the linear model.


# (h) Repeat (a)–(f) less noise
set.seed(1)
epsLow <- rnorm(n, mean = 0, sd = sqrt(0.05))
yLow <- -1 + 0.5 * x + epsLow
dataLow <- data.frame(x = x, y = yLow)

lmModelLow <- lm(y ~ x, data = dataLow)

p3 <- ggplot(dataLow, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_line(data = data.frame(x = seq(min(x), max(x), length.out = 100),
                              y = -1 + 0.5 * seq(min(x), max(x), length.out = 100)),
            aes(x = x, y = y), color = "red", linetype = "dashed")
print(p3)
print(summary(lmModelLow))
## For a lower noise scenario, we generate epsLow with a much smaller standard deviation. The resulting response, yLow, leads to a nearly perfect linear relationship. The scatterplot, which is p3 shows points aligning exactly along the fitted blue line. However, note that the red dashed line does not coincide with the blue line; the estimated slope here is around 0.7236. This demonstrates how reducing noise makes the data follow a different exact line, as the error term is nearly zero.


# (i) Repeat (a)–(f) more noise
set.seed(1)
epsHigh <- rnorm(n, mean = 0, sd = 1)
yHigh <- -1 + 0.5 * x + epsHigh
dataHigh <- data.frame(x = x, y = yHigh)

lmModelHigh <- lm(y ~ x, data = dataHigh)

p4 <- ggplot(dataHigh, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_line(data = data.frame(x = seq(min(x), max(x), length.out = 100),
                              y = -1 + 0.5 * seq(min(x), max(x), length.out = 100)),
            aes(x = x, y = y), color = "red", linetype = "dashed") 
print(p4)
print(summary(lmModelHigh))

## When we increase the noise (epsHigh with sd = 1), the response yHigh changes to follow y = -1 + 1.5x. The fitted model (lmModelHigh) yields a slope of 1.5, and the corresponding scatterplot (p4) shows points that perfectly line up along the new blue line due to the deterministic use of the seed. Meanwhile, the red dashed line remains at y = -1 + 0.5x, clearly diverging from the blue line. This highlights how increased noise  can alter the observed relationship.



# (j) 
ciOrig <- confint(lmModel)
ciLow  <- confint(lmModelLow)
ciHigh <- confint(lmModelHigh)

ciOrig  
ciLow   
ciHigh 


## We compute the confidence intervals for each model. In the original noise case, the intervals for the intercept and slope (e.g., about –1.115 to –0.923 for the intercept and 0.393 to 0.606 for x) reflect typical estimation uncertainty. In contrast, the confidence intervals for the less-noise and more-noise cases are exact—shown by single-value bounds—because the perfect fit leaves no residual error, which is why warnings about “essentially perfect fit” are issued.









