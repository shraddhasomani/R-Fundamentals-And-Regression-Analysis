require(faraway)
require(ggplot2)
require(GGally)
require(gridExtra)
require(e1071)

head(uswages,10)
summary(uswages)


# We see that exper has negative values.
uswages$exper[uswages$exper < 0] = NA
summary(uswages$exper)


# Convert categorical variables to factors
uswages$race = factor(uswages$race)
levels(uswages$race) = c("White", "Black")
uswages$smsa = factor(uswages$smsa)
levels(uswages$smsa) = c("No", "Yes")
uswages$pt = factor(uswages$pt)
levels(uswages$pt) = c("No", "Yes")


# Convert set of dummy variables to one variable
uswages = data.frame(uswages, region = 1*uswages$ne + 2*uswages$mw + 3*uswages$so + 4*uswages$we)
uswages$region = factor(uswages$region)
levels(uswages$region) = c("ne", "mw", "so", "we")


# Deleting four regions ne, mw, so and we 
uswages = subset(uswages, select = -c(ne:we))


# Take care of NA's
uswages = na.omit(uswages)

# 5 - Number Summary
summary(uswages)
# Analysis:
# - If Mean and Median are unequal, skewness is present.
# - Skewed - Wage
# - Not Skwed - Educ and Exper
# - In Race, smsa, pt and region there is no concept of skewness because it has binary values.
# - There is an unbalanced counts for race, smsa and pt.
# - This would tend to weaken the strength of a factor to predict the wages.


# Correlation 
cor(uswages$wage, uswages$educ)
# - There is a weak positive correlation between wage and educ. 
cor(uswages$wage, uswages$exper)
# - There is a weak positive correlation between wage and exper. 
cor(uswages$educ,uswages$exper)
# - There is a weak negative correlation between educ and exper. 


# Distribution of wages
m = mean(uswages$wage, na.rm = TRUE)
std = sd(uswages$wage, na.rm = TRUE)
n = length(uswages$wage)
p = 1:n/(n+1)
oldpar = par(mfrow = c(2,2))
hist(
  uswages$wage, 
  density = 20, 
  breaks = 20,
  freq = FALSE,
  prob=TRUE, 
  xlab = "uswages$wage",
  main = "Normal curve over histogram")
curve(
  dnorm(x, mean = m, sd = std),
  col = "darkblue", 
  lwd = 0.25, 
  add=TRUE)
plot(
  density(uswages$wage),
  main = "Normal curve overlay")
curve(
  dnorm(x, mean = m, sd = std), 
  col = "darkblue", 
  lwd = 0.25, 
  add = TRUE)
plot(
  p, 
  sort(uswages$wage), 
  pch = ".", 
  cex = 2,
  main = "Sort plot w/ normal curve overlay")
curve(
  qnorm(x, mean = m, sd = std), 
  col = "darkblue", 
  lwd = 0.25, 
  add = TRUE)
qqnorm(
  uswages$wage, 
  pch = ".", 
  cex = 2, 
  main = "Normal Probability QQ Plot")
qqline(uswages$wage)
# Analysis:
# - There are outliers present.
# - From the density graph, it is clear that wage is positively skewed.
# - Majority of the wage lies between 50 to 2000.


# Boxplots
plot(wage ~ pt, data = uswages)
# Analysis:
# - The wages for part time are not that spread out compared to the people who are working full time.
# - People doing full time have more wages compared to people working part time.

plot(wage ~ region, data = uswages)
# Analysis:
# - The max wage value & the interquartile range for the people living in we(West) is slightly more compared to the rest of the regions.
# - There are outliers present in all the regions except for mw(Middle West).

plot(wage ~ smsa, data = uswages)
# Analysis:
# - The max wage value & the interquartile range for the people living in smsa(Standard Metropolitan Statistical Area) is slightly more compared to the people not living in smsa.
# - There are outliers present in boxplot for the people living in smsa(Standard Metropolitan Statistical Area)

plot(wage ~ race, data = uswages)
# Analysis:
# - Whites earn more compared to blacks.
# - There are outliers present in whites.


# Experience vs Wage with respect to Race
ggplot(uswages,aes(x = exper, y = wage, shape = race, na.rm ='TRUE')) +geom_point() +facet_grid(~ race)
# - Analysis:
# - We observe that compared to blacks there are more no of whites. Apart from that what we observe that the wages of whites is also higher compared to black.


# Education vs Wage with respect to Race
ggplot(uswages, aes(educ, wage, shape = race, na.rm = 'TRUE')) +geom_point() +facet_grid(~race)
# Analysis:
# - The distribution of education in whites is spread out compared to blacks and whites receive more wages.


# Part time vs Wage with respect to Race
ggplot(uswages,aes(x=pt,y= wage, shape= race, na.rm='TRUE'))+geom_point()+facet_grid(~ race)
# Analysis:
# - Whites who dont work in Part Time are more in numbers compared to blacks and earn more wages.
# - This statement holds good even for part time


# Education vs Wage with respect to Part time
ggplot(uswages, aes(educ, wage)) +facet_grid(~pt) +geom_point() +geom_jitter()
# Analysis:
# - People who are not working as part time are more and their wages are more spread out compared to people who are working as part time.
# - People with no part time are distributed from 0 to 18 years of education. 


# Experience vs Wage with respect to Part time
ggplot(uswages, aes(exper, wage)) +geom_point() +facet_grid(~pt)
# - Analysis:
# - People who are not working as part time are more and their wages are more spread out compared to people who are working as part time.
# - People with no part time are distributed from 0 to 50 years of experience. 


# Scatter plots for all the attributes
ggpairs(uswages, columns = c(1:3))
# Analysis:
# - Correlation between wage and educ is 0.26 which shows a weak positive linear relationship. 
# - Correlation between wage and exper is 0.16 which shows a weak positive linear relationship.
# - Correlation between educ adn exper is -0.29 which shows a weak negative linear relationsip.

pairs(~ wage + educ + exper, data = uswages, col = uswages$race)


ggpairs(uswages, mapping = aes(colour = race))
# Analysis 1: Skewness
skewness(uswages$wage)
skewness(uswages$educ)
skewness(uswages$exper)
# - For wages, the graph is highly distributed and the data is positively skewed i.e towards the right.
# - For educ, the graph is moderately distributed and the data is negatively skewed i.e towards the left.
# - For exper, the graph is moderately distributed and the data is positively skewed i.e towards the right.

# Analysis 2: Wages vs Factor variables
# - wage vs race - Whites earn more than the black do and the spread of wages for whites is more.
# - wage vs smsa - The count and the wages of whites in smsa is very high compared the blacks. The count of whites not living in smsa is high compared to blacks but the wages are almost similar.
# - wage vs pt - The count and the wages of whites who work part-time is very high compared the blacks.
# - wages vs region - The wages for all the people living in different regions is almost the same.

# Analysis 3: Educ vs Factor variables
# - educ vs race - Whites are more educated than blacks.
# - educ vs smsa - Whites staying in smsa are more educated than blacks. Whites not staying in smsa are more educated than blacks.
# - educ vs pt - There are more number of blacks who are educated and are working as part time compared to whites. Whites not working as part time are more educated than blacks.
# - educ vs region - In all the region Whites are more educated than blacks

# Analysis 4: Exper vs Factor variables
# - exper vs race - There are almost equal number of whites and blacks who have same years of experience.
# - exper vs smsa - Whites living in smsa and not living in smsa an are slightly more experienced than blacks.
# - exper vs pt - Whites working as part time have much more experience than blacks. Whites and blacks have almost the same experience for those who are not working as part time.
# - exper vs region - In all the region Whites are more experienced than blacks apart from the ones staying in Middle West where the whites and  blacks have almost the same experience.


ggpairs(uswages, lower = list(continuous = "smooth", combo = "facetdensity", mapping = aes(color = race)))
# Analysis:
# - The slope coefficient is based on two predictors - educ and exper.


# Fit Model
fit = lm(wage ~ educ + exper, uswages)
summary(fit)
# Analysis:
# - The slope coefficient are positive. So with increase in education and experience there will be increase in wages.

deviance = deviance(fit)
deviance
y = uswages$wage
totalss = sum((y-mean(y))^2)
totalss
1 - deviance/totalss

summary(fit)$r.square
# Analysis:
# - The model is not a good fit because the value of R^2 is 0.13 which is much less than 1.
# - 13% of the variance in the response variable can be explained by the explanatory variables. The remaining 87% can be attributed to unknown, lurking variables or inherent variability

c(summary(fit)$r.square, cor(fitted.values(fit), uswages$wage)^2)
# Analysis:
# - The pearson correlation is equal to model summary.


# Fit Plot
qplot(fit$fitted.value, wage, data = uswages) +geom_abline(intercept = 0, slope = 1, color="green") +ggtitle("Fit Plot")
# Analysis:
# - It is not a good fit plot

# Residual Plot
ggplot(fit, aes(.fitted, .resid)) +geom_point() +geom_hline(yintercept = 0, color = "red", linetype = "dashed") + ggtitle("Residual Plot")
# Analysis:
# - The points in a residual plot are randomly dispersed around the horizontal axis, a linear regression model is appropriate for the data.


# Exploring model structure
cor(fit$resid, uswages$wage)
# Analysis:
# - As the correlation is high, it indicates that there is some trouble with our model.

plot1 = qplot(race, fit$resid, geom = "boxplot", data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
plot2 = qplot(smsa, fit$resid, geom = "boxplot", data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
plot3 = qplot(pt, fit$resid, geom = "boxplot", data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
plot4 = qplot(region, fit$resid, geom = "boxplot", data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
plot5 = qplot(educ, fit$resid, data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
plot6 = qplot(exper, fit$resid, data = uswages) +geom_hline(yintercept = 0, color = "dark blue", linetype = "dashed")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3)
# Analysis:
# - We see prounanced patters indicating we do not need to include square of the predictors or other transforms of the predictors.


# Normailty of the Residual
mod = fortify(fit)
plot7 = qplot(.stdresid, data = mod, geom = "histogram")
plot8 = qplot(.stdresid, data = mod, geom = "density")
plot9 = qplot(sample = .stdresid, data = mod, geom = "qq") +geom_abline()
grid.arrange(plot7, plot8, plot9, nrow = 1)
# Analysis:
# - We see that the residual do not look as though they come from a normal distribution.

g = lm(log(wage) ~ educ + exper + race + smsa + pt + region, data = uswages)
confint(g, level = 0.95)


# Compare Model
g1 = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
confint(g1)
tab1 = anova(g1,g)
tab1


# P-value and F-value Calculation
g.bg = lm(log(wage) ~ educ + exper + race + smsa + pt + region, data = uswages)
g.sm = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
sse.sm = deviance(g.sm)
df.sm = df.residual(g.sm)
sse.bg = deviance(g.bg)
df.bg = df.residual(g.bg)
mse.prt = (sse.sm-sse.bg)/(df.sm-df.bg)
mse.bg = sse.bg/df.bg
f.ratio = mse.prt/mse.bg
f.ratio
p.value = pf(f.ratio, df.sm-df.bg, df.bg, lower.tail=FALSE)
p.value
# Analysis:
# - The P-value is 0.634 which is greater than 0.05. 
# - Therefore, model 2 is better.


# Joint Confidence Region
install.packages("ellipse", repos = "http://cran.us.r-project.org", type = "source")
library(ellipse)
plot(ellipse(g1, c("educ", "exper")), type = "l", main = "Joint Confidence Region")
points(0,0)
points(coef(g1)["educ"], coef(g)["exper"])

g2 <- lm(log(wage) ~ race + smsa + pt, data = uswages)
plot(ellipse(g1, c("educ", "exper")), type = "l", main = "Joint Confidence Region")
points(0,0)
points(coef(g)["educ"], coef(g)["exper"], pch=18)
abline(v=confint(g)["educ",], lty=2)
abline(h=confint(g)["exper",], lty=2)
compareg2g1 <- anova(g2, g1)
compareg2g1
# Analysis:
# - If the p-value is less than or equal to the alpha (p < .05), then we reject the null hypothesis, and we say the result is statistically significant. 
# - If the p-value is greater than alpha (p > .05), then we fail to reject the null hypothesis, and we say that the result is statistically nonsignificant (n.s.).
# - The F-Ratio 238.9 is big and since the p-value 2.2e-16 is much less than 0.05, we reject the null hypothesis H0 : ??educ = ??exper = 0.


# Prediction
g1 = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
x0 = data.frame(educ = 12, exper = 5, race = "White", smsa = "Yes", pt = "No", stringsAsFactors = FALSE)
predict(g1, x0, level = 0.95, interval = "confidence")


x0 <- rbind(x0, data.frame(educ = 12, exper = 5, race = "Black", smsa = "Yes", pt = "No"))
predict(g1, x0, level = 0.95, interval = "confidence")