require(faraway)
require(ggplot2)
require(lmtest)
require(car)
require(gridExtra)
library(scatterplot3d)
head(uswages)
summary(uswages)
# Manipulating data. We see that exper has negative values
uswages$exper[uswages$exper <0] = NA
# Convert race, smsa, and pt to factor variables
uswages$race = factor(uswages$race)
levels(uswages$race) = c("White","Black")
uswages$smsa = factor(uswages$smsa)
levels(uswages$smsa) = c("No","Yes")
uswages$pt = factor(uswages$pt)
levels(uswages$pt) = c("No","Yes")
# Create region, a factor variable based on the four regions ne, mw, so, we
uswages = data.frame(uswages,
                      region =
                        1*uswages$ne +
                        2*uswages$mw +
                        3*uswages$so +
                        4*uswages$we)
uswages$region = factor(uswages$region)
levels(uswages$region) = c("ne","mw","so","we")
# Delete the four regions ne, mw, so, we
uswages = subset(uswages,select=-c(ne:we))
# Take care of NAs
uswages = na.omit(uswages)
summary(uswages)

# Exercise 1 - Nonconstance variance
# 1(a) Using the uswage data, fit the model (m): wage ~ educ + exper + race + smsa + pt + region
m = lm(wage ~ educ + exper + race + smsa + pt + region, data = uswages)

# 1(b) Produce the Residuals vs Fitted plot, and discuss if there may be heteroskedasticiy in the error variance.
mod = fortify(m) 
ggplot(mod, aes(.fitted, .resid)) + geom_point() + geom_hline(yintercept=0, color="red", linetype="dashed") + ggtitle("Residual Plot vs Fitted Plot") + geom_smooth(color = "red", se = F)
# Answer:
# - The red line is slightly curved and the residuals seem to increase as the fitted Y values increase.
# - So, the inference here is, heteroscedasticity exists.

# Statistical Heteroskedasticiy test
bptest(mod)
# Answer:
# - The test have a p-value less that a significance level of 0.05.
# - Therefore we can reject the null hypothesis that the variance of the residuals is constant and infer that heteroskedasticiy is indeed present.

# 1(c) Produce the Scale-Location plot, and discuss if there may be heteroskedasticiy in the error variance.
qplot(.fitted, abs(.resid), data = mod) + geom_hline(yintercept = 0, linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + geom_smooth(method = "gam", color = "red", se = F)
# Answer:
# - Heteroskedasticiy is not present, if the red line is a straight line.
# - But in our case, the red line is not a straight line so the inference here is, heteroscedasticity exists.

# 1(d) Perform the approximate test of noncontant error variance.
summary(lm(abs(residuals(m)) ~ fitted(m)))
# Answer:
# - We look at the t-test for the slope coefficient with null hypothesis that the slope is zero. 
# - At the 10% level of significance, we conclude that the slope is not zero since the p-value, 6.528e-14, is less than 0.10
# - Therefore, we conclude that there is nonconstant error variance.


#Exercise 2 - Non-normal errors
#2(a) Plot the Normal QQ Plot and Histogram of the residuals from model m Exercise 1. Do they indicate non-normal errors?
p1 = qplot(sample = scale(.resid), data = mod) + geom_abline(intercept = 0, slope = 1, color = "red") + labs(title = "Untransformed y", y = "Residuals")
p2 = qplot(scale(.resid), data = mod, geom = "blank") + geom_line(aes(y = ..density.., colour = "Empirical"), stat = "density") + stat_function(fun = dnorm, aes(colour = "Normal")) + geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = "Density", values = c("red", "blue")) + theme(legend.position = c(0.85, 0.85)) + labs(title = "Untransformed y", y = "Residuals")
grid.arrange(p1, p2, nrow = 2)
#Answer:
#Clearly the residuals of the model indicate non-normal error.

#2(b) Perfrom the Shapiro-Wilk test of normality for the residuals of model m. What is the P-value and what does it say about normality?
shapiro.test(residuals(m))
#Answer
#The null hypothesis is that the the residuals are normal. 
#Since the p-value is smaller than the significant value (0.05), we reject the null hypothesis. 
#The residuals are not normal.

#2(c) Find the optimal Box-Cox power transform and apply it to wage, refit model m, replot Normal Q-Q Plot and perform the Shapiro-Wilk test of normality again. Did the Box-Cox Power Transform work?
lambda = powerTransform(m)
lambda
lam = lambda$lambda
mlam = lm(wage ~ educ + exper + race + smsa + pt + region, data = uswages)
modlam <- fortify(mlam)
qplot(sample = scale(.resid), data = modlam) + geom_abline(intercept = 0, slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
shapiro.test(residuals(mlam))
#Answer:
#The Box-Cox Power Transform did not work for our model.


#Exercise 3 -  Influential outliers
#3(a) Produce the influence plot for model m. Are there any really large CookD values?
influencePlot(m)
#Answer:
#There are large cookD values.

#3(b) Produce the half-normal plot of the leverage values. Are they any high leverage data points?
islands <- row.names(uswages)
halfnorm(lm.influence(mlam)$hat, labs = islands, ylab = "Leverages")
#Answer:
#Yes, there are high leverage data points

#3(c) Produce the half-normal plot of the Cook's distance. Are they any high Cook's distance points?
cook <- cooks.distance(mlam)
halfnorm(cook, 3, labs = islands, ylab = "Cook's distance")
#Answer:
#Yes, there are high Cook's distance points

#3(d) Fit model excluding observation with largest Cook's Distance. Do the coeficients change? Are there any coeficients with notable changes?
mlam1 = lm(wage ~ educ + exper + race + smsa + pt + region, data = uswages, subset = (cook < max(cook)))
compareCoefs(mlam, mlam1)
#Answer:
#Yes, the coefficients change.
#There are no notable changes.

#3(e) Produce the omnibus diagnotic plot for model m. Which observation consistantly stands out as an outlier-influential point in all four plots?
oldpar = par(mfrow = c(2, 2))
plot(mlam, main = "Uswages Data")
#Answer:
#Observations that consistently stands out as an outlier-influential point in all four plots are - 2780, 15387 and 25909


#Exercise 4 - Model structure
#4(a) Produce the CERES plots for model m. Do the factor varibles stop the plots from printing?
ceresPlots(m, terms = ~.)
#Answer:
#Yes, the factor variables stop the plots from printing.

#4(b) How many plots are there? Why these?
#Answer:
#There are two plots - Educ and Exper. Only these two plots are plotted because the rest of the variable are converted into factor variables.

#4(c) Do the plots indicate a polynomial model should be considered?
#Answer:
#Yes, a polynomial model should be considered.


#Exercise 5 - Interaction model
#5(a) Fit an interaction model using the region and the two numeric variables. Is the model useful?
uswages$dummy = factor(uswages$exper < 18)
summary(uswages)
m_interaction = lm(wage ~ educ + exper * dummy + race + smsa + pt + region, data = uswages)
m_1 = lm(wage ~ exper + region + educ, data = uswages)
anova(m_1, m_interaction)
#Answer:
#The model is useful as p value is less than 0.05

#5(b) Test the interaction model versus model m. What is the p-value and which model does it indicate?
anova(m, m_interaction)
#Answer:
#P-value = 2.2e-16
#It indicates model 2


#Exercise 6 - Collinearity
#6(a) Find the variance inflation factors for model m.
vif(m)

#6(b) Do they indicate collinerairty in the predictors?
a.df = data.frame(uswages)
b = subset(a.df, select = c(wage, educ, exper))
summary(b)
round(cor(b),1)