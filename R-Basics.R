# R Basics
# Q1) Compute the 5 Number Summary of all the vairables. Do any of the variables exhibit some skewness? Determine if any values could be declared missing, and then convert the values to missing NA. Rerun the 5Number Summary.
ToyotaPrices <- read.csv("D:/DADM/Assignment/ToyotaPrices.csv")
summary(ToyotaPrices)
library(e1071)
skewness(ToyotaPrices$Id)
skewness(ToyotaPrices$Price)
skewness(ToyotaPrices$Age_08_04)
skewness(ToyotaPrices$Mfg_Month)
skewness(ToyotaPrices$Mfg_Year)
skewness(ToyotaPrices$KM)
skewness(ToyotaPrices$HP)
skewness(ToyotaPrices$Automatic)
skewness(ToyotaPrices$cc)
skewness(ToyotaPrices$Doors)
skewness(ToyotaPrices$Cylinders)
skewness(ToyotaPrices$Gears)
skewness(ToyotaPrices$Quarterly_Tax)
skewness(ToyotaPrices$Weight)
skewness(ToyotaPrices$Mfr_Guarantee)
skewness(ToyotaPrices$BOVAG_Guarantee)
skewness(ToyotaPrices$Guarantee_Period)
skewness(ToyotaPrices$ABS)
skewness(ToyotaPrices$Airbag_1)
skewness(ToyotaPrices$Airbag_2)
skewness(ToyotaPrices$Airco)
skewness(ToyotaPrices$Automatic_airco)
skewness(ToyotaPrices$Boardcomputer)
skewness(ToyotaPrices$CD_Player)
skewness(ToyotaPrices$Central_Lock)
skewness(ToyotaPrices$Powered_Windows)
skewness(ToyotaPrices$Power_Steering)
skewness(ToyotaPrices$Radio)
skewness(ToyotaPrices$Mistlamps)
skewness(ToyotaPrices$Sport_Model)
skewness(ToyotaPrices$Backseat_Divider)
skewness(ToyotaPrices$Metallic_Rim)
skewness(ToyotaPrices$Radio_cassette)
skewness(ToyotaPrices$Tow_Bar)
# Analysis:
# - Skewness is the measure of symmetry.
# - The following varibles exhibit Positive Skewness - ID; Price; Mfg_Month; Mfg_Year; KM; HP; Automatic; cc; Gears; Quarterly_Tax; Weight; Mfr_Gurantee; Gurantee_Period; Automatic_airco; Boardcomputer; CD_Player; Radio; Mistlamps; Sport_Model; Metallic_Rim; Radio_cassette and Tow_Bar.
# - Positive Skewness is also called as Left Skew.
# - The following variables exhibit Negative Skewness - Age_08_04; Doors; BOVAG_Gurantee; ABS; Airbag_1; Airbag_2; Airco; Central_Lock; Power_Windows; Power_Steering and Backseat_Divider
# - Negative Skewness is also called as Right Skew.
# - No Skew : Cylinder
# - is.na(ToyotaPrices)         #returns TRUE is any values are missing
# - There are no values that can be declared as missing values


# Q2) Convert categorical variables to factor. After doing the conversions rerun the 5 Number Summary. Do any of the factor variables have "unbalanced" counts; ie, more of one kind than another? Unbalanced counts would tend to weaken the strength of a factor to predict the price of a Toyota.
ToyotaPrices$Automatic = factor(ToyotaPrices$Automatic)
ToyotaPrices$Doors = factor(ToyotaPrices$Doors)
ToyotaPrices$Cylinders = factor(ToyotaPrices$Cylinders)
ToyotaPrices$Gears = factor(ToyotaPrices$Gears)
ToyotaPrices$Mfr_Guarantee = factor(ToyotaPrices$Mfr_Guarantee)
ToyotaPrices$BOVAG_Guarantee = factor(ToyotaPrices$BOVAG_Guarantee)
ToyotaPrices$ABS = factor(ToyotaPrices$ABS)
ToyotaPrices$Airbag_1 = factor(ToyotaPrices$Airbag_1)
ToyotaPrices$Airbag_2 = factor(ToyotaPrices$Airbag_2)
ToyotaPrices$Airco = factor(ToyotaPrices$Airco)
ToyotaPrices$Automatic_airco = factor(ToyotaPrices$Automatic_airco)
ToyotaPrices$Boardcomputer = factor(ToyotaPrices$Boardcomputer)
ToyotaPrices$CD_Player = factor(ToyotaPrices$CD_Player)
ToyotaPrices$Central_Lock = factor(ToyotaPrices$Central_Lock)
ToyotaPrices$Powered_Windows = factor(ToyotaPrices$Powered_Windows)
ToyotaPrices$Power_Steering = factor(ToyotaPrices$Power_Steering)
ToyotaPrices$Radio = factor(ToyotaPrices$Radio)
ToyotaPrices$Mistlamps = factor(ToyotaPrices$Mistlamps)
ToyotaPrices$Sport_Model = factor(ToyotaPrices$Sport_Model)
ToyotaPrices$Backseat_Divider = factor(ToyotaPrices$Backseat_Divider)
ToyotaPrices$Metallic_Rim = factor(ToyotaPrices$Metallic_Rim)
ToyotaPrices$Radio_cassette = factor(ToyotaPrices$Radio_cassette)
ToyotaPrices$Tow_Bar = factor(ToyotaPrices$Tow_Bar)
summary(ToyotaPrices)
# Analysis:
# - Unbalanced count are present in the following variables:
# - Automatic; Mrf_Guarantee; BOVAG_Guarantee; ABS; Airbag_1; Airbag_2; Airco; Automatic_airco; Boardcomputer; CD_Player; Central_Lock; Powered_Windows; Power_Steering; Radio; Mistlamps; Sport_Model; Backseat_Divider; Metallic_Rim; Radio_Cassette and Tow_Bar


# Q3) Explore the distribution of Price. Prepare the necessary plots, such as histogram, density plot, sort plot, QQplot. Is the variable normal? Is the variable skewed? Are there any clusters?
require(ggplot2)
# Histogram
hist(ToyotaPrices$Price, main = "Histogram for Price", xlab = "Price")
# Density Plot
plot(density(ToyotaPrices$Price), main = "Density Plot for Price")
# Sort Plot
plot(sort(ToyotaPrices$Price), main = "Normal Curve", ylab = "Price")
# QQPlot
qqnorm(ToyotaPrices$Price)
# Analysis:
# - The variable is normal.
# - The variable "Price" is positively skewed since the distribution is concentrated on the left side of the figure.
# - There are  two clusters present.


# Q4) Produce the scatterplot of Price versus the number of KM (kilometers). Use both the plot() and the qplot() functions. Does the relations look like a line or a curve?
# plot()
plot(ToyotaPrices$Price, ToyotaPrices$KM, xlab = "Price", ylab = "KM")
# qplot()
qplot(ToyotaPrices$Price, ToyotaPrices$KM, xlab = "Price", ylab = "KM")
# Analysis:
# - The relation looks like a curve.


# Q5) BoxWhisker plot of Price versus ABS
boxplot(Price ~ ABS, data = ToyotaPrices, xlab = "ABS", ylab = "Price")
# Analysis:
# - Yes, automobiles with anti-locking breaks tend to have a higher price. 
# - Yes, there are outliers present in ABS.


# Q6) Compute the correlation between Price and KM. Is it a strong or weak correlation? Is it positive or negative? If it is positive, what does it mean? Or If it is negative, what does it mean?
cor(ToyotaPrices$Price, ToyotaPrices$KM)
# Analysis:
# - The correlation between Price and KM is negative.
# - It is a weak correlation because as the number of kilometer decreases, the price increases.