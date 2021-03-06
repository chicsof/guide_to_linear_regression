
# GUIDE TO LINEAR REGRESSION

################################################## Creating the model ##################################################

########################################################################################################################
### the linear is usufull for studing the relationship of predictors (dependant variable, x values) to a reaction    ###
### (independant variable, y value). This is called interference, the coefficiant of each predictor will show the    ###
### strenght and direction of that predictor (how mach of an effect it has and whether it is a possitive or negative ###
### relationship). It may also be used for predicting the y for specified x values. It is of the form                ###
### y= a + bx + cx+...+nx + e, where  a is the intercept to the y axis, c to n are the coefficients, x are the       ###
### predictors and e is some error. We gather a sample of data wich we use to estimate our coefficients. The         ###
### coefficents are estimated using the least squared method. We plot a line of the form y= a + bx + cx+...+nx + e,  ###
### that minimizes the distance of our true y values for each x from the predicted by that line y(basically the line ###
### that is on average closer to each data point). That line represent the mean/ expected value for each x.          ###
########################################################################################################################

# We will create a linear model in this example
# Libraries
install.packages("MASS")
install.packages("ISLR")
library(MASS)
library(ISLR)

# We will use the Boston dataset, it contains medv, a value for the mean house value and various other attributes such
# as rm rooms in the house and age of the house, we want to make a linear regression model that uses those values as
# predictors to predict the response, value of the house.
?Boston
# not all the avalable attributes always make for good predictors. For example we might have something like first name of landlord
# that does not change the price as much. We say that those attributes will not have a significant coefficient, their coefficient 
# will be close to zero.

# To identify the predictors that can give us the best price prediction we can start by bringing in one by one our
# attributes and comparing performance (forward approach), or starting with all and removing one by one (backward), or even a
# mix.

# We will start with the first approach and analyse our model
# We are bringing in rm (rooms)
# This creates a linear model for medv as a function of rm
lm.rm_fit <- lm(medv~rm, data = Boston)
# This will give as the basic information of the model, such us the coefficient of our predictors and the intercept
summary(lm.rm_fit )
# We can see that rm is of high significance, it has a very small p-value 
# (this means there is only a very small probability that the H0, rm has no effect on price is true).
# We also get the intercept and coefficient of the fit line so we can see that the approximated line is of the form:
# y = 9x - 35

###################################################### R-squared #######################################################

########################################################################################################################
### We can use the R-square value (coefficient of determination), as a performance indicator. It measures the        ###
### percentage of variance of our points, that can be described by the regression line.                              ###
### To find that value we calculate the total variation not decribed by the line: by deviding a meause of the        ###
### disctance of our points to the line, to a measure of the distance of our points from the mean.The rest of the    ###
### variation whill be discribed by our model so we can simple take 1 minus the result. This basicly will show       ###
### how much better our model is from just predicting using the mean.                                                ###
########################################################################################################################



# We can get the meausere of the disctance of our points to the line by calculating the sum of squared error.
# That is how far our points are by the line (how far the true y for each x is from estimated by the line y):
# Since we previously calculated our coeefients for the line we can use the following to get the y estimated by the line 
yForX <- function(x){
  9 * x - 35
}# we could also use the build in function 'predict' but this makes it simpler for now

# The squared error would be their squared sum, where Boston$medv is the actual y 
SELine <- sum( (Boston$medv - yForX(Boston$rm) ) ^ 2)
SELine

# The measure of the distance of our points from the mean is given by the squared difference of each y from the mean of y
meanOfY <- mean(Boston$medv)
SEy <- sum( (Boston$medv - meanOfY ) ^ 2)
SEy

# Now we can calculate the percentage of the total variation not described by our line,
DescribedByLine <- SELine / SEy
DescribedByLine
# So the R-square, the percentage not described would be :
rSquared <- 1 - DescribedByLine
rSquared
# We can see this matches the number calculated by R
summary(lm.rm_fit )$r.squared

# If about 40% of the mean is described by the rm, the rest is various other predictors that we have not accounted for
# and the fact that the actual relation may not exactly be linear. This is described partially by e (the unreducible
# error) where Y(the house value) = F(x)(some function that described it ) + e (some random error).
# To check how linear the actual relation is we can plot the graph
plot(Boston$medv, Boston$rm)
# So if we bring more variable we expect this to increase
# This is a multivariant regression which would like : y= b1*x1 + b2*x2 + b3*x3 ... + bn*xn + c 
lm.fitAll <- lm(medv~., data = Boston)
# We can see that the model now described about 74% of the variation, using a couple more significant coefficients, such
# as crime in the area, taxes and so on.
summary(lm.fitAll)
# we should note that a multivariate model is far superior than regressing each predictor with the price using
# individual models, since a change in price could be the result of other predictors or combinations of predictors.
# Separate models could result in inaccurate calculations of significant coefficients.

################################################# CONFIDENCE INTERVALS #################################################

########################################################################################################################
###       **They help us find a range whithin which the true regression line of the population would be.**           ###
### This is because we have used a SAMPLE to approximate the true coefficients and draw our regression line.         ###
### Doing that creates some error, our true mean is +/- that error, this is called the confidence interval.          ###
### It is important to get the confidence interval for each of our estimated coefficients in order to determine      ###
### how significant their true values are(how far from zero).                                                        ###
########################################################################################################################

# The intercept and coefficient are approximated using a sample of nrow(Boston) samples
# We cannot say that those values equal the true ones. There is an error in using a sample to draw conclusions, called
# the standard error. The true value would be our coefficient +/- the error, this is given by the confidence interval.
# In R we can get this by:
confint(lm.rm_fit)

# Lets see how this is calculated for the coefficient of rm
# First we need to calculate the residual standard error
# (residual means, what is left after we are done explaining the value of a point using our regression line, so the
# error)
# It measures how far our actual values are from the regression line so it is given by:
# the square root of the sum of all the differences of the estimated y values to the actual y values squared, divided by
# total values minus 1, lets apply this:
standError <- sqrt(sum( (yForX(Boston$rm) - Boston$medv ) ^ 2) / (nrow(Boston) - 1))
# We can see this value matches the one given by R on the summery statistics of our model
standError
# In order to get the specific standard error due to the rm coefficient we need to divide by the difference due to x for 
# rm
xFory <- function(y){
  (y + 35) / 9
}
# Given by this formula
COSE <- standError / (sqrt(sum( (xFory(Boston$medv) - Boston$rm) ^ 2)) )
# This matches the value calculated from R by:
coef(summary(lm.rm_fit))[, "Std. Error"]


# Knowing that we can construct a confidence interval, we will use a 97.5% to match the one used by R for the
# coefficient true value. This would be our value 9 +/- a critical value (driven by the confidence interval selected)
# multiplied by the standard error.

# We use a t value for our critical value since we do not know standard deviation, our error is an estimate,
# the t value can be found in a table, or using software. To find it we need to know the degrees of freedom
# (total rows -2) and our selected confidence interval.
t <- qt(0.975, df = nrow(Boston) - 2)
t
# So the confidence interval would be:
# get the calculated coefficient
coef <- as.numeric(summary(lm.rm_fit)$coefficients[2, 1])
coef
left <- coef + t * COSE
left
right <- coef - t * COSE
right
# Left and right matches the confidence intervals given by R
confint(lm.rm_fit)

################################################# PREDICTION INTERVALS #################################################

########################################################################################################################
### Those are intervals for an INDIVIDUAL point, unlike confidence intervals that are used for the mean/expected     ###
### value (of all y's that have that x). It tries to answer, what is the particular value of y given some x          ###
### They are useful when predicting a new y, from given x values. They have wider range than confidence intervals    ###
### That is because not only do they depend on the mean error but also the individual random e error y=f(x)+e        ###
########################################################################################################################

# We can see that the prediction intervals are wider when comparing them in R
predict(lm.rm_fit, newdata = list(rm = 8), interval = "confidence")
predict(lm.rm_fit, newdata = list(rm = 8), interval = "prediction")

# To calculate the prediction we use a similar approach to confidence intervals, however this time the error is both
# dependant on the standard error (variation of the mean) and the error of each individual point due to e (the fact that
# our model is not a perfect fit for the truth) in other words ... our mean (expected value of y for an x given by the 
# regression) is not accurate and our point is not guaranteed to be exactly the same as the mean),
# this error is given by adding those to up.
# We already have the standard error, so we can find the total error relevant to our specific x point given by SEpedY.
# We will use the example where rm=8:
rooms <- as.numeric(Boston$rm)
SEpedY <- sqrt(standError ^ 2 * (1 + (1 / nrow(Boston)) + ( (8 - mean(rooms) ) ^ 2) / sum( (rooms - mean(rooms) ) ^ 2) ))
SEpedY

# We can now get the prediction intervals for rm =2 by:
PredictLeft <- yForX(8) - t * SEpedY
PredictLeft
PredictRight <- yForX(8) + t * SEpedY
PredictRight

# Left and right matches the prediction intervals given by R

######################we can plot prediction and confidence intervals
install.packages('ggplot2')
library(ggplot2)

ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE)

temp_var <- predict(lm.rm_fit, interval = "prediction")
new_df <- cbind(Boston, temp_var)

ggplot(new_df, aes(rm, medv)) +
  geom_point() +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_smooth(method = lm, se = TRUE)

# The shaded area shows where the true regression line of the population would be with a 95% confidence.
# The fact that it is wider on the ages just means that the standard error for the intercept is higher than that of the
# coefficient of our x (rm). If we imagine moving the line along the shaded area we notice that the intercept changes
# more than the slope. The line represents the mean values of y for every x, however the particular x we are studying
# may not exactly fall into the mean. This is where the dotted red lines are useful. 95% of the values will fall within
# the dotted lines (prediction interval)


################################################# Heteroscedasticity #################################################

########################################################################################################################
### One important assumption we have taken when calculating confidence and prediction intervals is that the variance ###
### of the errors is constant (equal scatter of the data points). However the variance may change as the response    ###
### (y) is changing. We have heteroscedasticity, when that change is systematic, follows a pattern. Typically, it    ###
### produces a distinctive fan or cone shape in residual plots. It can also impact the accuracy of the coefficients  ###                                                 ###
########################################################################################################################
# For this example we will use the cars dataset instead, as our data does not have clear indications of
# heteroscedasticity not enough variance to justify changes in data.
install.packages("Ecdat")
library(Ecdat)
#load our data set
data("Bwages", package = "Ecdat") 
?Bwages
lm.het_fit <- lm(wage ~ school, data=Males)
# We can use the following visualizations
# Scatterplot
# We can see a cone shape which may indicate heteroscedasticity
plot( Bwages$edu, Bwages$wage)
# Residuals/fitted is of interest
par(mfrow = c(2,2)) 
plot(lm.het_fit)
# It indicates heteroscedasticity, as there is a curve, the X values do not look random

# Breusch-Pagan test for a more algorithmic approach
install.packages("lmtest")
library(lmtest)
# p value is very small so we can reject the H0, there is heteroscedasticity
bptest(lm.het_fit)

# So what do we do? We can try and identify why this is happening, why the variance increase with the education and 
#include this change in our model in order to optimize it.

# From domain knowledge we can say that individuals with higher education, have more choice in what to do. For example 
# they may prioritize making money or having a more relaxed job.
# So we can calculate the rate at which it increases and add this as a coefficient of x that describes the variance:
# for yi = a + bxi + ei (where ei is the error) Var(ei|xi) = s^2 * kxi, and not just the standard deviation s^2, this is
# called weighted regression/least squares WLS. The difference in WLS from OLS (ordinary least squares) is that how much
# a point can affect the position of the best fit line is not equal, it is dependant on the variance associated with the
# particular x if it is a point of high variance it will affect the line less, if it is of low variance it will have a
# greater affect.
# As a result we get a better model.

# Other approaches where the heteroscedasticity is less systematic we can try data transformations such us logs, or sqrt
# or even a Box-Cox transformation. However, we have to keep in mind how this affects our original model.
########################################################################################################################

####################################################### Outliers #######################################################

########################################################################################################################
### Points that are far from the predicted y values (maybe due to errors in taking the measurements or very          ### 
### extreme/special cases) they may not always massively affect the best fit line, however they could have a         ###
### significant effect in the calculations of errors because they are very far from the predicted y, the R-squared   ### 
### value will decreased if we had more information about the data and how it was gathered or maybe additional       ###
### domain knowledge to interpret the outcomes, we could potentially just remove them or adjust them towards the     ###
### mean.                                                                                                            ###
########################################################################################################################


# We can see a few of them in the plot
plot(Boston$rm, Boston$medv)
abline(lm.rm_fit, col = "red")

# Simple way to find outliers on a set of a single variable is the univariate approach. Outliers are defined as values
# below or above 1.5*IQR (Inter Quartile Range = Q3 -Q1)
# We can visualise that in a boxplot where everything out of the whiskers is treated as an outlier
boxplot(Boston$rm)
# We can list them using the following code
outlier_values <- boxplot.stats(Boston$rm)$out

# There are also a few multivariant approaches in defining outliers. For example the Cooks Distance which measures the
# influence of a row on the sample to the line. In general, points that have about 4 times the mean may be
# classified as influential. Those are also called leverage points, they may not affect the R-square value as much as
# other outlier but affect the fit of the regression line.

# We can calculate them in R using the following code, for 2 or more variables
cooksd <- cooks.distance(lm.rm_fit)
influential <- as.numeric(names(cooksd)[(cooksd > 4 * mean(cooksd, na.rm = T))])
BostonSubSet <- c(Boston$rm, Boston$medv)
BostonSubSet[influential ]

################################################## Multicollinearity ###################################################

#######################################################################################################################
# When dealing with multivariate regression, some of the values may describe each other. For example, if in our case###
# we were to add house condition and house age as two separate factors, we might find that there is some relation #####
# between age and condition (we are unsure how strong that relation is), but in some cases we might need to consider###
# whether or not we should include both the two (or three or more..) variables in our regression. This could have #####
# a negative affect on the stability of our results. There will be issues in identifying the effect of each individual#
# predictor, and changes in x would result in greater (that the true) changes in Y since the same effect is now ######
# accounted in multiple predictors ####################################################################################
#######################################################################################################################


########### Correlation ###########################
# We can first look at some bivariate approaches (measuring the correlation of each pair of two predictors) the
# correlation factor is a simple way of doing this, a value from -1 to 1 showing direction and strength of the
# relationship
?cor
# Identical so cor = 1, positive linear relationship
cor(1:5, 1:5)
# Negative cor = -1
cor(1:5, 5:1)
# Less correlation cor < 1
cor(1:5, c(1, 2, 3, 4, 4))
# Lets use the example of tax and criminal activity
# Variables need to be numerical to be compared
typeof(Boston$crim)
typeof(Boston$tax)
cor(Boston$crim, Boston$tax)
# This is calculated using the following formula
x <- Boston$crim
y <- Boston$tax
meanX <- mean(x)
meanY <- mean(y)

nominator <- sum( (x - meanX) * (y - meanY) )
denominator <- sum( (x - meanX) ^ 2) * sum((y - meanY) ^ 2 )
r <- nominator/sqrt(denominator)
# We can see this matches exactly with the value given by cor() function
r
# A few things to note about the formula:
# The equation is bound by 1 
# If there is a positive correlation where xi > meanX and yi > meanY then, form the formula we can see that we will
# have some positive outcome, the other way around we will get a negative value. 
# If there is no relationship, the x is not described by the y then the sum of (x - meanX) will approximately
# cancel out with the sum of ( y - meanY) and we will get a very small value.


########## Sample Correlation Coef to True value #########################################
# We have calculated a the correlation coefficient of a sample, we need to test if that is enough evidence to prove that
# the true correlation coefficient is far from zero (Ha)

# In R we can easily complete a two tail test on our Hypothesis
cor.test(Boston$crim, Boston$tax) 

# What the cor.test does is it calculates a p-value for the cor coefficient we have just calculated, to show
# what would the probability of getting such a number at random (if the H0 was true and there is no correlation)
# if that probability is very small usually less than 0.05 we can reject the H0. We can calculate it using the 
# following steps (see project hypothesis testing for the theory):

t <- (r * sqrt(nrow(Boston) - 2))/sqrt(1 - r ^ 2)
# Area > t and Area < t
p <- pt(q = t/2, df = (nrow(Boston) - 1), lower.tail = FALSE) + pt(q = -t/2, df = (nrow(Boston) - 1), lower.tail = TRUE)
################# Corelation Matrix################

# To get the complete correlation matrix for (all the pairs) 
cor(Boston)
# To visualize this
install.packages("corrplot")
library(corrplot)
corrplot(cor(Boston))

# We can also see each scatter plot using the following
# We will study correlation between the following predictors, to not overflow the graphs
lm.fitMultivariet <- lm(medv~ rm + crim + tax + crim, data = Boston)
install.packages("GGally")
library(GGally)
ggpairs(lm.fitMultivariet)

######################### Variance Inflation ###########################
# Scatterplots and correlation matrixes are useful, however they only look at the relations between pairs (bivariate),
# we can use variance inflation VIF to account for interaction within multiple attributes

# The idea here is to find whether a combination of predictors can describe another predictor xi, to find that we can
# run a regression line for each xi, xi = d0 + d1 * x2i + d2 * x3i ... , where x2i, x3i ... are the rest of the
# predictors. We can then asses the fitness of that line using the R-squared value. If the line is a good fit then that
# predictor is well described by the rest of the predictors and we have multicollinearity.

# Lets take the example of tax
# First we regress the tax as a function of the rest of the predictors
lm.tax <- lm(tax~ . -medv, data = Boston)
# We can then get the r squared
Rsquared <- summary(lm.tax)$r.squared
# The variance inflation value is given by the following equation, where if VIF is large, usually greater 
# than 5 we can say there is multicollinearity present
VIF <- 1 / (1 - Rsquared)
# We can see that in this case the VIF is quite large, this is explained by the fact that predictors that are location 
# related ones, rooms and house age, how green the area is, criminal activity, surrounding population social/economical
# statues and so on, that are used for predicting house price are also ideal for predicting tax 
VIF
# Last we can get the VIF values in R using the following code
install.packages("car")
library(car)
vif(lm.fitAll)

# Lets see what happens to our original line if we remove tax
lm.fitAllMinusTax <- update(lm.fitAll, ~ . -tax)
summary(lm.fitAll)$r.squared
summary(lm.fitAll2)$r.squared
# The r squared has slightly gone down. However, tax is a result of factors that we have accounted (e.g location) a
# change in those factors would affect tax, and price would be affected both by the change in tax (which is only the 
# result of the original change) and by the original change, multiplying its affects and 'inflating' the true price
# value. For example a change is the location of the house would result in an increase in the tax and therefore an
# increase in the price greater than the true. What would be interesting to do is find out why we are loosing some
# fitness when we remove the tax, as seen for the r squared.
# Are there other factors describing the tax, that we have not included. Could we use them for the price prediction
# instead of tax?

# Statistically, we can find out if the change in a restricted model (where a variable is removed, in this case a model
# without tax) is significant using the F-statistic.
# First we would calculate the SSR (regression sum of squared errors) for the unrestricted and restricted model
summary(lm.fitAll)
SSRun <- anova(lm.fitAll)["Residuals","Sum Sq"]
SSRre <- anova(lm.fitAllMinusTax)["Residuals","Sum Sq"]
# The f stat is given by:
n <- nrow(Boston)
p <- length(Boston)
Fstat <- ((SSRre - SSRun)/p) / (SSRun/(n - p - 1))
# From the f distribution we can find a critical value that represents the point separating the curve to the rejection 
# area of a=0.05 like we do with t distribution (see hypothesis testing repo)
Fcrit <- qf(.95, p, n - p - 1)
# The Fstat falls under the rejection area and so we can accept the H0, removing the tax does produce a significant
# change.
# We can do this in r simply using the following code.
anova(lm.fitAll, lm.fitAllMinusTax)
################################################## Interaction terms ###################################################

########################################################################################################################
### Interaction terms just refers to the effect that certain combinations of predictors can have on the y. Sometimes ###
### we can optimize our regression model by accounting for the effect of predictor combinations, we can add their    ###
### products multiplied by a coefficient that shows the effect of the combination ( y = a + bx1 +cx2 + dx1x2 + e). A ###
### good example of where this is useful is the synergy effect, studied when analysing the effects of advertising    ###
### using deferent means (TV, web, redio ...). It was found that it was most effective to spread the advertising     ###
### budget across multiple means, rather than focusing it on the most profitable one. To measure such affects,       ###
### interaction terms are necessary                                                                                  ###
########################################################################################################################

# Simple way to do this in R for interaction of crim and tax
lm.simpleInteraction <- lm(medv~ crim + tax + crim*tax, data = Boston)
# We get the coefficient of the interaction and its significance the same way we get any other coefficient
summary(lm.simpleInteraction)
# Easier way to write the above
lm.simpleInteraction2 <- lm(medv~crim*tax, data = Boston)
summary(lm.simpleInteraction2)
# This returns all two-way interactions
lm.allPairsInteractions <- lm(medv~ .*., data = Boston)
# We can see that the R-squared when using the 2-way interactions has increased
summary(lm.allPairsInteractions)
# Lets try and explain some of the significant interactions found:
#  * Combination of chas(close to river) and nox(greenery), that combination may indicate that the house is build in a
#    very graphical area and therefore more expensive
#  * crim (criminal activity) and lstat (%lower status of population) may indicate how degraded an area is and therefore
#    less expensive
#  * rm (room per dwelling) and age, that combination may explain a lot of the variance in our dataset since, most
#    recent houses tend to have less space yet are more expensive due to modern equipment and built

# We need to consider which interactions are more significant and make the most sence from domain knowledge. Also keep
# in mind that we need to sustain an efficient and realistic model, not just a model with a very good R squared value

####################################### Non-linear transformations of predictors #######################################

########################################################################################################################
### A common optimization technique when dealing with linear regression is polynomial transformation of some the     ###
### predictor (e.g y = a + b*x +c*x^2). This is polynomial regression, and is still a linear model. This is useful   ###
### since often a relationship between variables is non linear, and this may be realized using scatterplots.         ###
########################################################################################################################

# Lets use the example of lstat and medv
plot(Boston$lstat, Boston$medv)
# We can see that the scatterplot follows a curve, which indicates that polynomials would be effective
# Lets compare the two

lm.linearRegression <- lm(medv~ lstat, data = Boston)
lm.PolynomialRegression <- lm(medv~ lstat + I(lstat^2), data = Boston)
anova(lm.linearRegression, lm.PolynomialRegression)
# The anova tested the H0 where the models both fit the data equally(as explained above). The F-statistic associated 
# produced a very small providing enough evidence to reject the Ho. 

# We can visualise this:
newdat = data.frame(lstat = seq(min(Boston$lstat), max(Boston$lstat), length.out = 100))
newdat$pred = predict(fit, newdata = newdat)
plot(medv ~ lstat, data = Boston)
with(newdat, lines(x = lstat, y = pred))
abline(lm(medv~ lstat, data = Boston), col="red")
# It is clear from the graph that the black line fits the data much better



################################################ Qualitative Predictors ################################################

########################################################################################################################
### Up until now we have dealt with numbers, what if one of our predictors was qualitative (e.g. sex, colour, level  ###
### of satisfaction). To use such a variable in LR we have to assign a dummy number/factor/enumeration, just some    ###
### short of number that will always be associated with one particular response (e.g female = 1, male = 0).          ###
########################################################################################################################

library("ISLR")
# We use this dataset for this example, this is taken from the book cited bellow
?Carseats
# R creates dummy values for qualitative predictors automatically
lm.fitQual <- lm(Sales ~., data = Carseats)
summary(lm.fitQual)
#we can see those dummy values using the following code
attach(Carseats)
contrasts(ShelveLoc)


