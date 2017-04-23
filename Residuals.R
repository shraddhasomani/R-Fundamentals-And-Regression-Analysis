# Residuals
# Part A: Residuals and Transforms
ToyotaPrices <- read.csv("D:/DADM/Assignment/ToyotaPrices.csv")
ToyotaPrices_PKWT = subset(ToyotaPrices, select = c(Price, KM, Weight, Tow_Bar))
head(ToyotaPrices_PKWT)

# Exercise 1: Residual Plot
# Q1 (A) - Obtain plot
# Obtain the Residuals vs Fitted Plot of the fitted model with an added horizontal line at y = 0
# Do the points look randomly distributed about the line?
fit = lm(Price ~ KM + Weight + Tow_Bar, ToyotaPrices_PKWT)
plot(fit$fitted.values, fit$residuals, xlab = "Fitted Values", ylab = "Residual")
abline(h = 0, v = NULL)
# Analysis:
# - Yes, the points look randomly distributed about the line.

# Q1 (B) - Obtain plot using the z-scores of the resiudals
# Repeat the Residuals vs Fitted Plot using z-scores of the residuals. Add empirical rule horizontal lines at +2 and ???2. 
# Use these lines to judge whether or not the residuals are normal or there are outliers. Point out any outliers. Point out any floor or ceiling effects. Do you think the residuals are normal?
a = fit$residuals
mean(a)
sd(a)
b = (a - mean(a))/sd(a)
c = plot(b, fit$fitted.values, xlab = "Z-Score of Residual", ylab = "Fitted Values")
abline(h = NULL, v = -2)
abline(h = NULL, v = 2)
d = plot(floor(b), fit$fitted.values, xlab = "Z-Score of Residual", ylab = "Fitted Values", main = "Floor")
e = plot(ceiling(b), fit$fitted.values, xlab = "Z-Score of Residual", ylab = "Fitted Values", main = "Ceiling")
# Analysis:
# - The residuals are normal.
# - There are outliers present.
# - floor(x) = is the largest integer less than or equal to x. Eg floor(3.456) = 3
# - ceiling(x) = is the smallest integer greater than or equal to x. Eg ceiling(3.456) = 4
# - Floor and ceiling plot is same, however there's a difference between x axis range and outliers.
# - There are more number of outliers present in ceiling compared to that of floor.


# Exercise 2: Residual Normal QQ-Plot
# Q2 (A) - Obtain the normal probability QQ-Plot of the residuals.
qqnorm(fit$residuals)
qqline(fit$residuals)

#Q2 (B) - Do the residuals look normal?
# Analysis:
# - Yes, the residual look normal. But there are few outliers present.


# Exercise 3: Composite goodness-of-fit plots
plot(fit)
# Q3 (A) - Obtain the composite goodness-of-fit plots
# Obtain the composite goodness-of-fit plots for the fitted model. What plots involve the residuals? Do the Residuals vs Fitted Plot and the Normal QQ-Plot look about the same as those obtained earlier?
# Analysis:
# - Plots involving the residual are: Residual vs Fitted Plot, Residual vs Leverage. 
# - Yes, the Residuals vs Fitted Plot and the Normal QQ-Plot look about the same as those obtained earlier.

# Q3 (B) - Outliers
# We examine the Residuals vs Leverage Plot in the composite goodness-of-fit plots. Outliers points will be identified by their row name. Are there any outliers? If so, what are their row names.
# Analysis:
# - Yes, there are outliers present.
# - Row Names - 602, 961 and 222