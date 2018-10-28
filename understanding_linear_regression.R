install.packages("MASS")
install.packages("ISLR")
library(MASS)
library(ISLR)

# we will use the Boston dataset, it contains medv, a value for the mean house value and various other attributes such
# as rm rooms in the house and age of the house, we want to make a linear regression model that uses those values as
# predictors to predict the response, value of the house
?Boston


# to identify the predictors that can give us the best price prediction we can start by bringing in one by one our
# attributes and comparing performance (forward approach), starting with all and removing one by one (backward), 
# or a mix

# we will start with the first approach and analyse our model
# we are bringing in rm
# this creates a linear model for medv as a function of rm
lm.RmFit <- lm(medv~rm, data = Boston)
# this will give as the basic information of the model
summary(lm.RmFit )
# we can see that rm is of high significance, it has a very small p-value which makes for a large F-statistic
# (this means there is only a very small probability that the H0, rm has no effect on price is true)
# we also get the intercept and coefficient of the fit line so we can see that the approximated line is of the form:
# y = 9x - 35



###################################################### R-squared #######################################################

########################################################################################################################
### We can use the R-square value (coefficient of determination), as a performance indicator. It measures the        ###
### percentage of variance of our points that cannot be described by the regression line.                            ###
########################################################################################################################

# We could calculate it by:
# measuring the square error. That is how far our points are by the line (how far y for each x is from estimated y):
# according to our estimated line we can get the estimated y from:
# we could also use the build in function 'predict' but this makes it simpler for now
yForX <- function(x){
  9 * x - 35
}
# the squared error would be their squared sum
# where Boston$medv is the actual y 
SELine <- sum( (Boston$medv - yForX(Boston$rm) ) ^ 2)
SELine
# we can calculate the total variation of y, that would be the squared difference of each y from the mean of y
meanOfY <- mean(Boston$medv)
SEy <- sum( (Boston$medv - meanOfY ) ^ 2)
SEy
# now we can calculate the percentage of the total variation SEy not described by SELine, the variation from our line
DescribedByLine <- SELine / SEy
DescribedByLine
# so the R-square, the percentage not described would be :
rSquared <- 1 - DescribedByLine
rSquared
# we can see this matches the number calculated by R
summary(lm.RmFit )$r.squared

# if about 40% of the mean is described by the rm, the rest is various other predictors that we have not accounted for
# and the fact that the actual relation may not exactly be fit, part of this is described by e (the unreducible error)
# where Y(the house value) = F(x)(some function that described it ) + e (some random error)
# to check how linear the actual relation is we can plot the graph
plot(Boston$medv, Boston$rm)
# so if we bring more variable we expect this to increase
# this is a multivariant regression which would like : y= b1*x1 + b2*x2 + b3*x3 ... + bn*xn + c 
lm.fitAll <- lm(medv~., data = Boston)
# we can see that the model now described about 74% of the variation, using a couple more significant coefficients,
# such crime in the area, taxes and so on
summary(lm.fitAll)

################################################# CONFIDENCE INTERVALS #################################################

########################################################################################################################
### this is important in order to determine if a coefficient is significant (far enough from zero b !=0) so that     ###
### we can assume the attribute is a predictor (y = bx...).Since  we are only using a sample to draw our conclusions ###
### we need to account some error before making that conclusion. Confidence intervals can measure the range for the  ###
### true mean/expected value (y given by the regression) of the population we are studying with a given % accuracy   ###
### for each x                                                                                                       ###
########################################################################################################################

# the intercept and coefficient are approximated using a sample of nrow(Boston) samples
# we cannot say that those values equal the true ones. There is an error in using a sample to draw conclusions, called
# the standard error. The true value would be our coefficient +/- the error, this is given by the confidence interval
# in R we can get this by:
confint(lm.RmFit)

# lets see how this is calculated for the coefficient of rm
# first we need to calculate the residual standard error
# (residual means, what is left after we are done explaining the value of a point using our regression line, so the error)
# it measures how far our actual values are from the regression line so it is given by:
# the square root of the sum of all the differences of the estimated y values to the actual y values squared, divided by
# total values minus 1, lets apply this:
standError <- sqrt(sum( (yForX(Boston$rm) - Boston$medv ) ^ 2) / (nrow(Boston) - 1))
# we can see this value matches the one given by R on the summery statistics of our model
standError
# in order to get the specific standard error due to the rm coefficient we need to divide by the difference due to 
# x for rm
xFory <- function(y){
  (y + 35) / 9
}
# given by this formula
COSE <- standError / (sqrt(sum( (xFory(Boston$medv) - Boston$rm) ^ 2)) )
# this matches the value calculated from R by:
coef(summary(lm.RmFit))[, "Std. Error"]


# knowing that we can construct a confidence interval, we will use a 97.5% to match the one used by R for the
# coefficient true value. This would be our value 9 +/- a critical value (driven by the confidence interval selected)
# multiplied by the standard error

# we use a t value for our critical value since we do not know standard deviation, our error is an estimate,
# the t value can be found in a table, or using software. To find it we need to know the degrees of freedom
# (total rows -2) and our selected confidence interval
t <- qt(0.975, df = nrow(Boston) - 2)
t
# so the confidence interval would be:
# get the calculated coefficient
coef <- as.numeric(summary(lm.RmFit)$coefficients[2, 1])
coef
left <- coef + t * COSE
left
right <- coef - t * COSE
right
# left and right matches the confidence intervals given by R
confint(lm.RmFit)

################################################# PREDICTION INTERVALS #################################################

########################################################################################################################
### Those are intervals for an INDIVIDUAL point, unlike confidence intervals that are used for the mean/expected     ###
### value (of all y's that have that x). It tries to answer, what is the particular value of y given some x          ###
### They are useful when predicting a new y, from given x values. They have wider range than confidence intervals    ###
### That is because not only do they depend on the mean error but also the individual random e error y=f(x)+e        ###
########################################################################################################################

#we can see that the prediction intervals are wider when comparing them in R
predict(lm.RmFit, newdata = list(rm = 8), interval = "confidence")
predict(lm.RmFit, newdata = list(rm = 8), interval = "prediction")

# to calculate the prediction we use a similar approach to confidence intervals, however this time the error is both
# dependant on the standard error (variation of the mean) and the error of each individual point due to e( the fact
# that our model is not a perfect fit for the truth) (in other words...
# our mean (expected value of y for an x given by the regression) 
# is not accurate and our point is not guaranteed to be exactly the same as the mean)
# same as the mean), this error is given by adding those to up:
# we already have the standard error, so we can find the total error relevant to our specific x point given by:
# and its distance from the mean, given by (for rm=8):
rooms <- as.numeric(Boston$rm)
SEpedY <- sqrt(standError ^ 2 * (1 + (1 / nrow(Boston)) + ( (8 - mean(rooms) ) ^ 2) / sum( (rooms - mean(rooms) ) ^ 2) ))
SEpedY

# we can now get the prediction intervals for rm =2 by:
PredictLeft <- yForX(8) - t * SEpedY
PredictLeft
PredictRight <- yForX(8) + t * SEpedY
PredictRight

# left and right matches the prediction intervals given by R
