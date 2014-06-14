#q1
# Consider the following data with x as the predictor and y as as the outcome. 
# Give a P-value for the two sided hypothesis test of whether beta1 from a linear regression model is 0 or not.

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)$coefficients[2,4] #p-value

#q2
# Consider the previous problem, give the estimate of the residual standard deviation.
summary(fit)$sigma #sigma. really

#q3
# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
data(mtcars)

x <- mtcars$wt
y <- mtcars$mpg

fit <- lm(y ~ x)
intercept <- fit$coeff[1]
slope <- fit$coeff[2]
df.residual <- fit$df.residual
sigma <- summary(fit)$sigma

plot(x, y)
abline(fit)

xmean <- mean(x)
yest <- intercept + slope * xmean

yest + c(-1,1) * qt(0.975, df.residual) * sigma / sqrt(length(x)) #confidence interval
predict(fit, data.frame(x=mean(x)), interval = ("confidence")) #obtains the same value

#q4
# Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?
?mtcars

#q5
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint? 
ssx <- sum((x - mean(x))^2)
n <- length(x)
intercept + slope * 3 + c(-1,1) * qt(0.975, df.residual) * sigma * sqrt(1 + 1/n + (3 - mean(x))^2 / ssx) #prediction interval
predict(fit, data.frame(x=3), interval = ("prediction")) #same

#q6
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. Give the lower endpoint.
sumCoef <- summary(fit)$coeff
2 * (sumCoef[2,1] + c(-1, 1) * qt(0.975, df.residual) * sumCoef[2,2])

#q9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. About what is the ratio of the the sum of the squared errors, sum((y-mean(y))^2)  when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)? 
e1 <- y - intercept
e2 <- y - intercept - slope * x

sum(e2^2) / sum(e1^2)