#### This file is for an illustration of linear regression with the built-in "mtcars" data in R

### First thing to do is load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)

### Take a look at mtcars data
head(mtcars, 6)

##################### Y = miles per galon (mpg), X = (shape of engine (vs), transmission (am))
my_lm <- lm(mpg~vs + am, mtcars)
summary(my_lm)

##################### Y = miles per galon (mpg), X = (weight (wt), shape of engine (vs))
my_lm <- lm(mpg~wt + vs, mtcars)
summary(my_lm)

########### Visualize the linear models

########### We create the linear equations for V-shaped and straight cars

### Linear equation for V-shaped cars, i.e., vs = 0
vshape_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt #average miles for v-shaped cars

straight_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt + coef(my_lm)[3] #average miles for straight cars

### Visualization
visual <- ggplot(mtcars, aes(y = mpg, x = wt, color = factor(vs))) + geom_point() + geom_line(aes(x = wt, y = vshape_equation), color = "#F8766D", size = 1.5) + geom_line(aes(x = wt, y = straight_equation), color = "#00BFC4", size = 1.5)

#Change the legend
visual + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(0, 1), labels=c("V-shaped", "Straight"))


###################### Y = miles per galon (mpg), X = (weight (wt), transmission (am))
my_lm <- lm(mpg~wt + am, mtcars)
summary(my_lm)

########### We create the linear equations for automatic and manual cars
### Linear equations based on am values, i.e., am = 0 and am = 1
automatic_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt #average miles for automatic cars

manual_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt + coef(my_lm)[3] #average miles for manual cars

### Visualization
visual <- ggplot(mtcars, aes(y = mpg, x = wt, color = factor(am))) + geom_point() + geom_line(aes(x = wt, y = automatic_equation), color = "#F8766D", size = 1.5) + geom_line(aes(x = wt, y = manual_equation), color = "#00BFC4", size = 1.5)

#Change the legend
visual + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(0, 1), labels=c("Automatic", "Manual"))


################################################################################ Interactions of predictors

###### Y = miles per galon, X = (weight (wt), shape of engine (vs))

my_lm <- lm(mpg~wt + vs + wt * vs, data=mtcars)
summary(my_lm)

## Visualize the models
vshape_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt #average miles for v-shaped cars

straight_equation = ( coef(my_lm)[1] + coef(my_lm)[3]) + (coef(my_lm)[2] + coef(my_lm)[4]) * mtcars$wt  #average miles for straight cars

visual <- ggplot(mtcars, aes(y = mpg, x = wt, color = factor(vs))) + geom_point() + geom_line(aes(x = wt, y = vshape_equation), color = "#F8766D", size = 1.5) + geom_line(aes(x = wt, y = straight_equation), color = "#00BFC4", size = 1.5)

visual + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(0, 1), labels=c("V-shaped", "Straight"))


##### Y = miles per galon, X = (weight (wt), transmission (am))

my_lm <- lm(mpg~wt + am + wt * am, data=mtcars)
summary(my_lm)

automatic_equation = coef(my_lm)[1] + coef(my_lm)[2] * mtcars$wt #average miles for v-shaped cars

manual_equation = ( coef(my_lm)[1] + coef(my_lm)[3]) + (coef(my_lm)[2] + coef(my_lm)[4]) * mtcars$wt  #average miles for straight cars

visual <- ggplot(mtcars, aes(y = mpg, x = wt, color = factor(am))) + geom_point() + geom_line(aes(x = wt, y = automatic_equation), color = "#F8766D", size = 1.5) + geom_line(aes(x = wt, y = manual_equation), color = "#00BFC4", size = 1.5)

visual + theme(legend.title=element_blank()) + scale_colour_discrete(breaks=c(0, 1), labels=c("Automatic", "Manual"))





