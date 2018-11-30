## Fitting a Poisson Regression Model + ICAR component
## It runs over all space (so it's zero-inflated data)
## We only fit coefficiente beta_0 and spatial and heterogeneity parameters.

rm(list=ls(all=TRUE))       # cleaning memory

library(rstan)   
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores()-1)

library(INLA)

load("transf_quakes.RData")
K = 3; # number of covariates

# Save all this data in data.frame (to be used in Diagnose!)

zero_infl_quakes = data.frame("y" = y, "ave_depth" = x[,1], "ave_mag" = x[,2], "ave_st" = x[,3])
save(zero_infl_quakes, file = "zero_inflated_dataframe.RData")

#Build the adjacency matrix using INLA library functions
adj.matrix = sparseMatrix(i = node1,j = node2) # , symmetric = TRUE
#The ICAR precision matrix (note! This is singular)
Q =  Diagonal(N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
# See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))

quakes_stanfit  = stan("bym2_no_offset_no_beta.stan", data=list(N,N_edges,node1,node2,y,scaling_factor), chains=3, warmup=5000, iter=6000, save_warmup=TRUE)

# Optional savings...
# save(quakes_stanfit, file = "quakes_zero_inflated_fit_beta0.RData")
# load("quakes_zero_inflated_fit_beta0.RData")

# basic summary for a couple of parameters...
print(quakes_stanfit , pars=c("beta0", "rho", "sigma", "logit_rho", "mu[5]", "phi[5]", "theta[5]"), probs=c(0.025, 0.5, 0.975))

## Checking stanfint through web-browser-shiny:
library(shinystan)
launch_shinystan(quakes_stanfit)

## Checking some quantities from my fit!

# if required:
#load("quakes_zero_inflated_fit_beta0.RData")

list_of_draws = extract(quakes_stanfit)
betas = list_of_draws[["betas"]]
beta0 = list_of_draws[["beta0"]] # drops x 3 regarding the 3 beta estimates
eta = list_of_draws[["eta"]]     # log(Y_i) I mean the response in log scale!
mu = list_of_draws[["mu"]]       # Y_i now in the exp sclae!

mean_fitted_Y = colMeans(mu, na.rm = FALSE, dims = 1)

## Check the FITTED RESPONSE with ggplot: NOTE: still need to change axis values!

library(ggplot2)
library(reshape2)

n_a = 58
n_b = 47

conv_Y = matrix(mean_fitted_Y, nrow=n_a,byrow=T)

try_Y = melt(conv_Y)

try_Y<-try_Y[try_Y$value > 0.1,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Fitted (Means) Earthquake Counts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)

## Original counts!

conv_Y = matrix(y, nrow=n_a,byrow=T)

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
