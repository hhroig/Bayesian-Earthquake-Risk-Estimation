## Bayesian Variable Selection and Model Averaging using Bayesian Adaptive Sampling

library(BAS)

## Load Data and construct data frame.
## Remember: we have lots of zero entries, and everithing (but zeros) are in log-scale!
load("transf_quakes.RData")
count_quakes = data.frame("y" = y, "ave_depth" = x[,1], "ave_mag" = x[,2], "ave_st" = x[,3]) 

# let's adjust areas with actual observed data! (same as excluding values when y == 0)
ind_nonzero = count_quakes$y>0

nonzero_count = count_quakes[count_quakes$y>0,]

## Adjust a Poisson linear model!

quakes.bas = bas.glm(y ~ ave_depth + ave_mag + ave_st, data=nonzero_count,
                     family=poisson(),
                     betaprior=EB.local(), modelprior=uniform(),
                     method='MCMC+BAS',n.models=2^3, MCMC.iterations=100000) 

## PLOTS ans SUMMARY!!!: some graphical summaries of the output 
## Desription: This plot shows the marginal posterior inclusion probabilities (pip) for each
#              of the covariates, with marginal pips greater than 0.5 shown in red. The 
#              variables with pip > 0.5 correspond to what is known as the median probability 
#              model. Variables with high inclusion probabilities are generally important for
#              explaining the data or prediction, but marginal inclusion probabilities may be
#              small if there are predictors that are highly correlated, similar to how p-values
#              may be large in the presence of multicollinearity.

plot(quakes.bas, which = 4, ask = FALSE, caption = "", sub.caption = "")

# Result: the plot indicates alll the estimated coefficientns have
#         high posterior inclusion probabilities (pip). This can be seen in detail in:

quakes.bas # summary of marginal pip.

options(width = 1000)
summary(quakes.bas) # some other results for the "top" models!

## Visualization of the Model Space
require(graphics)
image(quakes.bas,top.models = 20, intensity = TRUE,
      prob = TRUE, log = TRUE, rotate = FALSE, color = "rainbow",
      subset = NULL, drop.always.included = FALSE) # this should give deeper knowledge and should show (max) top 20 models...

## Posterior Distributions of Coefficients

coef.quakes.bas = coef(quakes.bas) # extract info about estimated betas

# Description: vertical bar represents the posterior probability that the coefficient is 0 while
#              the bell shaped curve represents the density of plausible values from all the models
#              where the coefficient is non-zero. This is scaled so that the height of the density 
#              for non-zero values is the probability that the coefficient is non-zero.

plot(coef.quakes.bas, ask = FALSE) 

# Highest Posterior Density intervals:
confint(coef.quakes.bas) # third column is the posterior mean

# We could also obtain this in a graph:
plot(confint(coef.quakes.bas))

## FITTED VALUES:

muhat.BMA = fitted(quakes.bas, type = "response", estimator = "HPM")

## Check the FITTED RESPONSE with ggplot: 

library(ggplot2)
library(reshape2)

n_a = 58
n_b = 47

Y = count_quakes$y

Y[ind_nonzero] = muhat.BMA

conv_Y = matrix(Y, nrow=n_a,byrow=T)

try_Y = melt(conv_Y)

try_Y<-try_Y[try_Y$value != 0,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Fitted Earthquake Counts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)

## Compare to observed counts:

Y = count_quakes$y

conv_Y = matrix(Y, nrow=n_a,byrow=T)

try_Y = melt(conv_Y)

try_Y<-try_Y[try_Y$value != 0,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Observed Earthquake Counts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)