# Use the Faraway gala data to first a Poisson regresssion model to Species using all the predictors.
# Compare the coefficients form this model to the Box-Cox Tranformed Normal Errors Linear Model that we used in the class notes.
require(faraway)
require(arm)
require(car)
require(ggplot2)
head(gala)
pois.mod = glm(Species ~ ., data = gala, family = poisson(link = "log"))
display(pois.mod)
g = lm(Species ~ ., data = gala)
lambda = powerTransform(g)
lambda
lam = lambda$lambda
glam = lm(Species^lam ~ ., gala)
modlam = fortify(glam)
p1 = qplot(sample = scale(.resid), data = modlam) + geom_abline(intercept = 0, slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
p1
summary(glam)
compareCoefs(pois.mod, glam)


# Fit the fpe data using a GLM-Poisson model.
# Compare coefficients form this model to the Weighted Least Squares Normal Errors Linear Model that we used in the class notes.
head(fpe)
pois.mod = glm(EI ~ ., data = fpe, family = poisson(link = "log"))
display(pois.mod)