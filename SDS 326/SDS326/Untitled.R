# ============================================================
# Homework 8: Affairs Dataset Analysis and Model Diagnostics
# ============================================================

# ---------- Question 1: Age Coding ----------
# According to the documentation, age is coded by ranges:
#   17.5 = under 20, 22 = 20–24, 27 = 25–29, 32 = 30–34, 37 = 35–39, etc.
# Therefore, if someone reports their age as 33 (which is in the 30–34 range),
# their coded age should be 32.
#
# Below is a function that maps a reported age (in years) to its coded value.

coded_age <- function(age) {
  if (age < 20) {
    return(17.5)
  } else if (age < 25) {
    return(22)
  } else if (age < 30) {
    return(27)
  } else if (age < 35) {
    return(32)
  } else if (age < 40) {
    return(37)
  } else if (age < 45) {
    return(42)
  } else if (age < 50) {
    return(47)
  } else if (age < 55) {
    return(52)
  } else {
    return(57)
  }
}

# Test the function for a reported age of 33
reported_age <- 33
cat("Question 1:\n")
cat("Reported age:", reported_age, "\nCoded age:", coded_age(reported_age), "\n\n")


# ---------- Setup: Load the Dataset and Required Packages ----------
# Install and load AER package if necessary
if (!require("AER")) {
  install.packages("AER")
  library(AER)
} else {
  library(AER)
}

# Load the Affairs dataset
data("Affairs")


# ---------- Question 2: Fit a Linear Model and Overall Test ----------
# Fit a linear model predicting the number of affairs using:
# gender, age, years married, whether there are children,
# religiousness, marriage rating, and education.
lm_affairs <- lm(affairs ~ gender + age + yearsmarried + children + religiousness + rating + education, 
                 data = Affairs)
summary_lm <- summary(lm_affairs)
cat("Question 2:\n")
print(summary_lm)

# Extract and display the overall F statistic (rounded to two decimal places)
f_stat <- summary_lm$fstatistic[1]
cat("Overall F-statistic (rounded to two decimals):", round(f_stat, 2), "\n\n")


# ---------- Question 3: Overall Model Significance ----------
# Conceptual answer (no code needed):
# Because the overall F-test yields a very small P-value, we can reject the null hypothesis 
# that none of the predictors are related to the number of affairs. That is, at least one 
# of the predictors is significantly related to affair frequency.


# ---------- Question 4: Alternative Hypothesis for 'children' in ANOVA ----------
# Refit the model and run ANOVA:
lm_affairs <- lm(affairs ~ gender + age + yearsmarried + children + religiousness + rating + education, 
                 data = Affairs)
anova_table <- anova(lm_affairs)
cat("Question 4:\n")
print(anova_table)
# Conceptual answer:
# The alternative hypothesis for the row corresponding to 'children' is that the 'children' 
# variable is associated with the number of affairs, after controlling for all of the other variables.


# ---------- Question 5: Nested F-Test for 'children' and 'education' ----------
# To test the null hypothesis that children and education are not useful predictors
# (controlling for gender, age, yearsmarried, religiousness, and rating), we create a reduced model:
lm_affairs_reduced <- lm(affairs ~ gender + age + yearsmarried + religiousness + rating, 
                         data = Affairs)
anova_test <- anova(lm_affairs_reduced, lm_affairs)
cat("Question 5:\n")
print(anova_test)
# Extract and display the p-value (rounded to at least three decimal places)
p_value_nested <- anova_test$`Pr(>F)`[2]
cat("Nested F-test p-value (rounded to three decimals):", round(p_value_nested, 3), "\n\n")


# ---------- Question 6: Conclusion from Nested F-Test ----------
# Conceptual answer:
# Since the p-value for the nested test is large (approximately 0.500), we fail to reject the null 
# hypothesis. That is, we do not have enough evidence to conclude that either children or education 
# adds significant predictive power for the number of affairs (when controlling for the other variables).


# ---------- Question 7: Checking Linearity with autoplot ----------
# Install and load ggfortify package if necessary
if (!require("ggfortify")) {
  install.packages("ggfortify")
  library(ggfortify)
} else {
  library(ggfortify)
}

cat("Question 7: A plot window will open showing diagnostic plots for the linear model.\n")
autoplot(lm_affairs)
# Conceptual answer:
# If the residuals vs. fitted plot (or other diagnostic plots) do not show systematic patterns, 
# then there is no obvious issue with the linearity assumption.


# ---------- Question 8: Checking Constant Variance (Homoscedasticity) ----------
# The scale-location plot in the autoplot output is used to assess constant variance.
# Conceptual answer:
# If the scale-location plot shows a systematic deviation (i.e., the square root of the absolute 
# residuals does not form a flat line), it indicates a violation of the constant variance assumption.


# ---------- Question 9: Checking Normality of Residuals ----------
# The QQ-plot in the autoplot output is used to check the normality assumption.
# Conceptual answer:
# If the points in the QQ-plot closely follow the dashed line, the normality assumption is satisfied.
# Otherwise, systematic deviations might indicate issues with normality.


# ---------- Question 10: Interpreting a Residuals vs. Fitted Plot ----------
# Conceptual answer:
# A residuals vs. fitted plot showing a “funnel” shape suggests that the variance of the residuals 
# is not constant across the range of fitted values, indicating a violation of the constant variance 
# (homoscedasticity) assumption.

