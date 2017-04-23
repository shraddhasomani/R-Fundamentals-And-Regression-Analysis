require(faraway)
head(uswages)
uswages$exper[uswages$exper < 0] = NA
uswages$race = factor(uswages$race)
levels(uswages$race) = c("White","Black")
uswages$smsa = factor(uswages$smsa)
levels(uswages$smsa) = c("No","Yes")
uswages$pt = factor(uswages$pt)
levels(uswages$pt) = c("No","Yes")
uswages = data.frame(uswages,
                     region =
                       1*uswages$ne +
                       2*uswages$mw +
                       3*uswages$so +
                       4*uswages$we)
uswages$region = factor(uswages$region)
levels(uswages$region) = c("ne","mw","so","we")
uswages = na.omit(uswages)
summary(uswages)


# Compute OLS fit to model log(wage)~.
# Perform the Shapiro-Wilk Test of Normality for the residuals, what is the conclusion?
require(car)
m = lm(log(wage) ~ ., uswages)
summary(m)
shapiro.test(residuals(m))
# Answer
# - The null hypothesis is that the the residuals are normal. 
# - Since the p-value is smaller than the significant value (0.05), we reject the null hypothesis. 
# - Hence, we can say that the residuals are not normal.


# Compute WLS fit to model log(wage)~. and weights = 1/(1+educ)
# Perform the Shapiro-Wilk Test of Normality for the residuals, what is the conclusion?
m1 = lm(log(wage) ~ ., uswages, weight = 1/(1 + educ))
shapiro.test(residuals(m1))
# Answer
# - The null hypothesis is that the the residuals are normal. 
# - Since the p-value is smaller than the significant value (0.05), we reject the null hypothesis. 
# - Hence, we can say that the residuals are not normal.


# Compute Robust fit to model log(wage)~. using Huber, Hampel, Biquare, LTS, and LAD
# Compare coefficients of the above fits using OLS, WLS, Huber, Hampel, Biquare, LTS, and LAD
# Which would you recommend?
# Why?
require(MASS)
# Huber M-Estimation
m2 = rlm(log(wage) ~ educ + exper + race + smsa + pt, psi = psi.huber, uswages)
# Hample M-estimation
m3 = rlm(log(wage) ~ educ + exper + race + smsa + pt, psi = psi.hampel, init = "lts", maxit = 100, uswages)
# Tukey Bisquare M-estimation
m4 = rlm(log(wage) ~ educ + exper + race + smsa + pt, psi = psi.bisquare, init = "lts", maxit = 100, uswages)
# Least Trimmed Squares (LTS)
require(robustbase)
m5 = ltsReg(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
m6 = ltsReg(log(wage) ~ educ + exper + race + smsa + pt, data = uswages, nsamp = "exact")
# Least Absolution Deviation (LAD)
require(quantreg)
m7 = rq(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
# Comparing Coefficients
coefs <- compareCoefs(m, m2, m3, m4, m5, m6, m7, se = FALSE)
colnames(coefs) <- c("OLS", "Huber", "Bisquare", "Hample", "LTS", "LTS-exact", "LAD")
coefs
# Answer:
# - We see that LTS and LTS-exact appear to agree with each other and both are very different from OLS.
# - All three M-estimation methods, Huber, Bisquare, and Hample are different from each other, and different from OLS and both LTS's.
# - LAD is similar to OLS.
# - LTS is recommended since it has the best breakdown.