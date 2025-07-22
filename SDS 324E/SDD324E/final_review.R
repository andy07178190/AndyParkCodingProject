# Fit logistic regression
challenger_glm <- glm(Fail ~ Temperature, 
                      data = challenger, 
                      family = binomial)

# View the summary
summary(challenger_glm)

# Create response matrix for O-ring failures
Y_failures <- cbind(challenger$nFailures, 
                    6 - challenger$nFailures)  # 6 minus failures = non-failures

# Fit model
failures_glm <- glm(Y_failures ~ Temperature, 
                    family = binomial,
                    data = challenger)

summary(failures_glm)

# Create sequence of temperatures for prediction
temps <- seq(from = 25, to = 85, by = 1)

# Get predicted probabilities
probs <- predict(challenger_glm, 
                 newdata = data.frame(Temperature = temps), 
                 type = "response")

# Create visualization
plot(temps, probs,
     type = 'l',
     xlab = "Temperature (°F)", 
     ylab = "Estimated Probability of O-ring Failure",
     main = "Estimated Probability of O-ring Failure vs Temperature")

# Add successes and failures
points(challenger$Temperature, challenger$Fail,
       pch = 19,
       cex = 1.2)

predict(challenger_glm, 
        newdata = data.frame(Temperature = 31), 
        type = "response")

summary(challenger_glm)

drop1(challenger_glm, test = "LRT")

## Fit the intercept-only challenger model
challenger_intercept <- glm(Fail ~ 1, data = challenger, family = binomial)

## Use anova() to compare intercept-only to the model that uses Temperature
anova(challenger_intercept, challenger_glm, test = "LRT")

confint(challenger_glm, level = 0.95)

# Create sequence of temperatures for prediction
temps <- seq(from = 25, to = 85, by = 1)

# Get predicted probabilities
probs <- predict(challenger_glm, 
                 newdata = data.frame(Temperature = temps), 
                 type = "response")

# Get confidence intervals on link scale
preds_link <- predict(challenger_glm,
                      newdata = data.frame(Temperature = temps),
                      type = "link",
                      se.fit = TRUE)

# Convert to response scale
ci_lower <- plogis(preds_link$fit - 1.96 * preds_link$se.fit)
ci_upper <- plogis(preds_link$fit + 1.96 * preds_link$se.fit)

# Create visualization
plot(temps, probs, type = "l",
     xlab = "Temperature (°F)", 
     ylab = "Probability of O-ring Failure",
     main = "Probability of O-ring Failure vs Temperature")

lines(temps, ci_lower, col = 'forestgreen', lty = 2, lwd = 2)
lines(temps, ci_upper, col = 'forestgreen', lty = 2, lwd = 2)

# Add observed data points
points(challenger$Temperature, challenger$Fail,
       pch = 19,
       cex = 1.2)

summary(challenger_glm)

drop1(challenger_glm, test = "LRT")

## Fit the intercept-only challenger model
challenger_intercept <- glm(Fail ~ 1, data = challenger, family = binomial)

## Use anova() to compare intercept-only to the model that uses Temperature
anova(challenger_intercept, challenger_glm, test = "LRT")

confint(challenger_glm, level = 0.95)

# Create sequence of temperatures for prediction
temps <- seq(from = 25, to = 85, by = 1)

# Get predicted probabilities
probs <- predict(challenger_glm, 
                 newdata = data.frame(Temperature = temps), 
                 type = "response")

# Get confidence intervals on link scale
preds_link <- predict(challenger_glm,
                      newdata = data.frame(Temperature = temps),
                      type = "link",
                      se.fit = TRUE)

# Convert to response scale
ci_lower <- plogis(preds_link$fit - 1.96 * preds_link$se.fit)
ci_upper <- plogis(preds_link$fit + 1.96 * preds_link$se.fit)

# Create visualization
plot(temps, probs, type = "l",
     xlab = "Temperature (°F)", 
     ylab = "Probability of O-ring Failure",
     main = "Probability of O-ring Failure vs Temperature")

lines(temps, ci_lower, col = 'forestgreen', lty = 2, lwd = 2)
lines(temps, ci_upper, col = 'forestgreen', lty = 2, lwd = 2)

# Add observed data points
points(challenger$Temperature, challenger$Fail,
       pch = 19,
       cex = 1.2)
log_odds = -1.075e+01-7.149e-01+5.738e-03*2000

# Get confidence intervals on link scale
preds_link <- predict(challenger_glm,
                      newdata = data.frame(Temperature = temps),
                      type = "link",
                      se.fit = TRUE)

# Convert to response scale using plogis() (the logistic function)
ci_lower <- plogis(preds_link$fit - 1.96 * preds_link$se.fit)
ci_upper <- plogis(preds_link$fit + 1.96 * preds_link$se.fit)

