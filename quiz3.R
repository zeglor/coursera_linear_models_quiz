data(mtcars)
# q1
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.

fit <- lm(mpg ~ as.factor(cyl) + wt, data = mtcars)
summary(fit)$coef[3,1] #the answer

#not for quiz
plot (as.factor(mtcars$cyl), mtcars$mpg)
plot (mtcars$wt, mtcars$mpg, col = mtcars$cyl)
cor(mtcars$cyl, mtcars$wt)

#q2
fit2 <- lm(mpg ~ as.factor(cyl), data = mtcars)
summary(fit)$coef
summary(fit2)$coef
#Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

#q3
fitInter <- lm(mpg ~ as.factor(cyl) * wt, data = mtcars)
anova(fit, fitInter)

#q4
?mtcars

#q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
plot(x, y)
max(hatvalues(fit))

#q6
dfbetas(fit)