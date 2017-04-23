# Confidence Interval
# Q1) For the prostate data infaraway, fit a model with lpsa as the response and the other variables as predictors. Compute 90% and 95% CIs for the parameter associated with age
require(faraway)
head(prostate)
g = lm(lpsa ~ ., data = prostate) 
confint(g, "age", level = 0.90)
confint(g, "age", level = 0.95)


# Q2) Compute and display a 95% joint confidence region for the parameters associated with age and lbph. Plot the origin and report the outcome of the appropriate hypotheses test. Affirm this conclusion with an appropriate partial F-test.
require(ellipse)
plot(ellipse(g, c("age", "lbph")), level = 0.95, type = "l", main = "Joint Confidence Region")
points(0,0)
points(coef(g)["age"], coef(g)["lbph"], pch = 18)
abline(v = confint(g)["age",], lty = 2)
abline(h = confint(g)["lbph",], lty = 2)


# Q3) Predict lpsa (95%) for a new patient with lcavol = 1.22692, lweight = 3.62301, age = 65, lbph = -0.3001, svi = 0.0, lcp = -0.79851, gleason = 7.0, pgg45 = 15.0. Do this again for the mean response. Using the exp() function, obtain the new prediction and mean response for psa.
x0 = data.frame(lcavol = 1.22692, lweight = 3.62301, age = 65, lbph = -0.3001, svi = 0.0, lcp = -0.79851, gleason = 7.0, pgg45 = 15.0, stringsAsFactors = FALSE)
predict(g, x0, level = 0.95, interval = "prediction")
exp(predict(g, x0, level = 0.95, interval = "prediction"))


# Q4) Repeat the above exercise with new patient age = 20
x0 = data.frame(lcavol = 1.22692, lweight = 3.62301, age = 20, lbph = -0.3001, svi = 0.0, lcp = -0.79851, gleason = 7.0, pgg45 = 15.0, stringsAsFactors = FALSE)
predict(g, x0, level = 0.95, interval = "prediction")
exp(predict(g, x0, level = 0.95, interval = "prediction"))
#Analysis:
#The origin is inside the ellipse, so we do not reject the null hypothesis.


# Q5) For the model in exercise 1, remove all the predictors that are not significant at the 5% level. Recompute the predictions for exercises 3 and 4. Compare CIs. On the psa scale, which CIs do you prefer?
g.sm = lm(lpsa ~ lcavol + lweight + age + svi , data = prostate)
# For age = 65
x1 = data.frame(lcavol = 1.22692, lweight = 3.62301, age = 65, svi = 0.0, stringsAsFactors = FALSE)
predict(g.sm, x1, level = 0.95, interval = "prediction")
exp(predict(g.sm, x1, level = 0.95, interval = "prediction"))
# For age = 20
x2 = data.frame(lcavol = 1.22692, lweight = 3.62301, age = 20, svi = 0.0)
predict(g.sm, x2, level = 0.95, interval = "prediction")
exp(predict(g.sm, x2, level = 0.95, interval = "prediction"))
# Analysis:
# - The prediction intervals from the second model should be narrower than those from the original model theoretically because all significant values have been removed
# - Therefore,the second model one should explain the response more accurately than the first model.
# - The length of the second prediction interval is longer than the first because of age.
# - The narrower prediction intervals are preferred.


# Test the "small" model in exercise 5 against the "big"" model in exercise 1 at probability type I error ??=0.05??=0.05. Which model is preferred?
# g = lm(lpsa ~ ., data = prostate)
# g.sm = lm(lpsa ~ lcavol + lweight + age + svi , data = prostate)
anova(g, g.sm)
# Analysis:
# - Since the P-Value for the F-stat is 0.1805 and is larger than the significance level 0.05, we accept the small model.
# - Small model is preferred because it is simple.