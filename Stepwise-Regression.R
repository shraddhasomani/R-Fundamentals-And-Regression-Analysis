require(car)
head(Salaries)
Salaries$rownames = rownames(Salaries)
Salaries$rownames = NULL
Salaries.old = Salaries
Salaries = na.omit(Salaries)
levels(Salaries$discipline) = c("Theorectical", "Applied")
some(Salaries)

# Perform stepwise regression strating with the full model using all the predictors of salary.
g1 = lm(salary ~ ., data = Salaries)
summary(g1)
# - We eliminate Sex, since it has the highest p-value.
# - Next we re-run the model without this variable.
g2 = update(g1, . ~ . - sex)
summary(g2)
# - There are no more variables with p-values higher than 0.05.
# - The final model is stored in the object g2.
# - Stepwise Regression
g3 = step(g1)


# Compare the coefficients of the stepwise model and the full model.
coefs = compareCoefs(g1, g2, se = FALSE)
colnames(coefs) = c("g.OLS", "g.OLS.step")
coefs
g.OLS = g1
g.OLS.step = g2

# Which variable of variables did stepwise drop from the full model?
# Answer:
# - Variable sex was dropped by stepwise from the full model


# Perform a cross-validation of the stepwise model.
require(DAAG)
CVlm(data = Salaries, form.lm = g.OLS, printit = F)
CVlm(data = Salaries, form.lm = g.OLS.step, printit = F)


# Compare the two models using the mse's from the cross-validations with number of folds equal to 3. Which model gives the better mse?
oldpar = par(mfrow = c (1, 2))
mse.g.OLS = CVlm(data = Salaries, form.lm = g.OLS, m = 3, main = "Prediction Plot: g.OLS")
mse.g.OLS.step = CVlm(data = Salaries, form.lm = g.OLS.step, m = 3, main = "Prediction Plot: g.OLS")
df = data.frame(mse.g.OLS = attr(mse.g.OLS, "ms"), mse.g.OLS.step = attr(mse.g.OLS.step, "ms"))
df