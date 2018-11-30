## Fitting a Poisson Regression Model to observed number of quakes!

## Usage of weakly informative priors on beta parameters, as described in:
## https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations

rm(list=ls(all=TRUE))       # cleaning memory

## Load & Select Data:
load("transf_quakes.RData")
K = 3 # number of covariates (betas rather than intercept)
Y = y # just storing the origianl outcomes


# let's adjust areas with actual observed data! (same as excluding values when y == 0)

ind_nonzero = y>0 # save areal-grid-indices of nonzero counts 

x = x[y > 0,]
y = y[y > 0]
N = length(y) # update the number of observations

# Scale (only) "input" variables so they have mean 0 and sd 0.5

true.means.x = colMeans(x, na.rm = FALSE, dims = 1)

x = scale(x, center = TRUE, scale = FALSE)

sd.after.center = c(sd(x[,1]), sd(x[,2]), sd(x[,3]))

x = scale(x, center = FALSE, scale = 2*sd.after.center)

# let's check it: (... all good...)
colMeans(x, na.rm = FALSE, dims = 1)
c(sd(x[,1]), sd(x[,2]), sd(x[,3]))

# Optional savings...
# nonzero_scaled_quakes = data.frame("y" = y, "ave_depth" = x[,1], "ave_mag" = x[,2], "ave_st" = x[,3])
# save(nonzero_scaled_quakes, file = "nonzero_scaled_quakes.RData")

## Fitting the model with R-Stan...
library(rstan)   
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores()-1)

stan.fit.quakes = stan("poi_regr_weak_prior.stan",  data=list(N, y, x, K), chains=3, warmup=5000, iter=6000, save_warmup=TRUE)

# Optional savings...
# save(stan.fit.quakes, file = "weak_prior_poi_fit.RData")
# load("weak_prior_poi_fit.RData")

# some summaries for a couple of parameters...
print(stan.fit.quakes , pars=c("beta0", "betas", "eta[1]", "mu[1]"), probs=c(0.025, 0.5, 0.975))

## Checking some quantities from my fit!

# if required:
# load("weak_prior_poi_fit.RData")

list_of_draws = extract(stan.fit.quakes)
betas = list_of_draws[["betas"]]
beta0 = list_of_draws[["beta0"]] # drops x 3 regarding the 3 beta estimates
eta = list_of_draws[["eta"]]     # log(Y_i) I mean the response in log scale!
mu = list_of_draws[["mu"]]       # Y_i now in the exp sclae!

mean_fitted_Y = colMeans(mu, na.rm = FALSE, dims = 1)

## Check the FITTED RESPONSE with ggplot:

library(ggplot2)
library(reshape2)

n_a = 58
n_b = 47

# Original counts! I already stored original outcomes in Y!

conv_Y = matrix(Y, nrow=n_a,byrow=T)

try_Y = melt(conv_Y)

try_Y<-try_Y[try_Y$value != 0,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Earthquake Counts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)

# Now we check the fitted!

Y[ind_nonzero] = mean_fitted_Y

conv_Y = matrix(Y, nrow=n_a,byrow=T)

try_Y = melt(conv_Y)

try_Y<-try_Y[try_Y$value > 0.01,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Fitted Earthquake Counts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
