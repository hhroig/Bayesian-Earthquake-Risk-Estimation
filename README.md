# Bayesian-Earthquake-Risk-Estimation
Final Project: Bayesian Inference (2018-2019)

Author: Harold A. Hernández Roig
November 30, 2018

## Introduction

In this study we face the problem of estimating the earthquake risk at a cube near Fiji. We have data from 1000 seismic events that occurred since 1964, although we have no knowledge or dependency on time for them. In order to describe better our data we introduce some terminology from earthquake research field [11]:

* \emph{Epicenter:} The latitude and longitude of the point of first motion of the earthquake. We have this information in variables \emph{lat} $\in [-38.59\degree ; -10.72\degree] $ and \emph{long} $\in [165.7\degree
	 ; 188.1\degree]$ respectively. 
* \emph{Depth:} The depth below the surface of the point of first motion. For our data, every observation was recorded in the variable \emph{depth}, with range $ [40; 680]  $ km. 
* \emph{Magnitude:} A measure of the size of the event, quoted in this case in the Richter scale. All the earthquakes reported in our dataset have the corresponding magnitude in variable \emph{mag} $\geq 4.0$, being $6.40$ the highest value reported (all in $\log$ scale).
* \emph{Stations:} Usually the number and locations of stations reporting the event. We only have the number in variable \emph{stations}, with a range of values in $[10; 132]$.

The term e"arthquake risk" is in fact loosely used. In order to distinguish from other studies, we follow the terminology from \cite{Vere-Jones1995} and define as risk: 

"The \emph{geophysical risk} (or \emph{earthquake hazard}) is the expected rate of occurrence (number of events per unit time) within a prescribed region and over certain magnitude level."

As we have no information regarding the origin time we focus on estimating the geophysical earthquake risk of the observed area, without dependence on time. This could be thought as the long-term average, static, or background risk. 

We propose two approaches based on Bayesian Poisson Regression. First, to regresses the non-zero responses in terms of the covariates in matrix $X$, including a (previous) Bayesian Variable Selection procedure. Second, to take into account the spatial structure of the observed area, thus we include an Intrinsic Conditional Auto-Regressive (ICAR) component, in order to explain the correlation between neighboring regions.

## REFERENCES
[1] Prior choice recommendations, https://github.com/stan-dev/stan/wiki/Prior-ChoiceRecommendations. Consulted on November 29th, 2018.

[2] Julian Besag, Jeremy York, and Annie Mollié. Bayesian image restoration, with two applications in spatial statistics. Annals of the Institute of Statistical Mathematics, 43(1):1–20,
mar 1991.

[3] Stephen P Brooks and Andrew Gelman. General Methods for Monitoring Convergence
of Iterative Simulations. Journal of Computational and Graphical Statistics, 7(4):434–
455, 1998.

[4] Merlise A. Clyde, Joyee Ghosh, and Michael L. Littman. Bayesian adaptive sampling
for variable selection and model averaging. Journal of Computational and Graphical
Statistics, 20(1):80–101, 2011.

[5] Andrew Gelman, Aleks Jakulin, Maria Grazia Pittau, and Yu Sung Su. A weakly informative default prior distribution for logistic and other regression models. Annals of Applied
Statistics, 2(4):1360–1383, 2008.

[6] Andrew Gelman and Donald B. Rubin. Inference from Iterative Simulation Using Multiple Sequences. Statistical Science, 7(4):457–472, 1992.

[7] Mitzi Morris. Spatial models in stan: Intrinsic auto-regressive models for areal data. Stan
Case Studies, http://mc-stan.org/users/documentation/case-studies/icar_stan.html.
Consulted on November 29th, 2018.

[8] Bent Natvig and Ingunn Fride Tvete. Bayesian hierarchical space-time modeling of
earthquake data. Methodology and Computing in Applied Probability, 9(1):89–114, 2007.

[9] Andrea Riebler, Sigrunn H. Sørbye, Daniel Simpson, Håvard Rue, Andrew B. Lawson,
Duncan Lee, and Ying MacNab. An intuitive Bayesian spatial model for disease mapping that accounts for scaling. Statistical Methods in Medical Research, 25(4):1145–1165,
2016.

[10] Stan Development Team. RStan: the R interface to Stan, 2018. http://mc-stan.org.

[11] David Vere-Jones. Forecasting earthquakes and earthquake risk. International Journal
of Forecasting, 11(4):503–538, 1995.
