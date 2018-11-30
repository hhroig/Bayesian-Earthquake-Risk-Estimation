## Transform Data: crete grid-cells, as well as count events, average covariates and create
##                 nodes indicating neighbooring relationship...

data(quakes)
quakes
# A data frame with 1000 observations on 5 variables.
# 
# [,1]	lat	numeric	Latitude of event
# [,2]	long	numeric	Longitude
# [,3]	depth	numeric	Depth (km)
# [,4]	mag	numeric	Richter Magnitude
# [,5]	stations	numeric	Number of stations reporting

require(graphics)
pairs(quakes, main = "Fiji Earthquakes, N = 1000", cex.main = 1.2, pch = ".")

summary(quakes)

a = seq(-39, -10.5, by = 0.5)     # grid for latitude
b = seq(165.5, 188.5 , by = 0.5)  # grid for longitude
 
n_a = length(a)     # number of latitude grid
n_b = length(b)     # number of longitude grid

N = n_a * n_b   # number of nodes

Y = matrix(0,n_a, n_b) # matrix with counts of earthquakes
Xd = Y                 # matrix with sum of depths for giving region (i,j)
Xm = Y                 # matrix with sum of magnitudes for giving region (i,j)
Xs = Y                 # matrix with sum of stations for giving region (i,j)
#y = array(0, N)       # array with counts of earthquakes

for (i in 1:(n_a-1)) {
  for (j in 1:(n_b-1)) {
    for (k in 1:1000) {
      if (quakes$lat[k]>=a[i] & quakes$lat[k]<a[i+1] & quakes$long[k]>=b[j] & quakes$long[k]<b[j+1]) {
        Y[i,j] = Y[i,j] + 1
        Xd[i,j] = Xd[i,j] + quakes$depth[k]
        Xm[i,j] = Xm[i,j] + quakes$mag[k]
        Xs[i,j] = Xs[i,j] + quakes$stations[k]
      }
    }
  }
}

y = as.vector(t(Y))           # array with counts of earthquakes
xd = as.vector(t(Xd))         # array with sum of depths by region i,j
xm = as.vector(t(Xm))         # array with sum of magnitudes by region i,j
xs = as.vector(t(Xs))         # array with sum of stations by region i,j

# Now I do the "average" of those quantities, and take logarithms in depths and station
# to retain variables in the same scale...

for (kk in 1:N) {
  if ( y[kk] > 0) {
    xd[kk] = log(xd[kk]/y[kk]) # = xd[k]/y[k] Without scalling!
    xm[kk] = xm[kk]/y[kk]
    xs[kk] = log(xs[kk]/y[kk]) # = xs[k]/y[k] Without scalling!
  }
}

# The "design matrix" 
x = cbind(y, xd, xm, xs)

# Check the counts with ggplot: NOTE: still need to change axis values!

library(ggplot2)
library(reshape2)
try_Y = melt(Y)

try_Y<-try_Y[try_Y$value!=0,]

ggplot(try_Y, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="long", y="lat", title="Counting Earthquakes") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11)) + 
                     scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)

# Constructing Nodes/Edges to denote neighbors: I'll asume as neighborhood all 8 blocks around.

MM = t(matrix(1:N, n_b, n_a)) # array of indices

# function to find the 8 neighbors values for a given entry (i,j) of matrix m:
# took it from: https://stackoverflow.com/questions/36073795/get-neighbors-function-in-r

get.nbhd <- function(m, i, j) {
  # get indices
  #idx <- matrix(c(i-1, i+1, i, i, i+1, i-1, i+1, i-1, j, j, j+1, j-1, j+1, j-1, j-1, j+1), ncol = 2)
  idx <- matrix(c(i-1, i-1, i-1, i, i, i+1, i+1, i+1, j-1, j, j+1, j-1, j+1, j-1, j, j+1), ncol = 2)
  # set out of bound indices to 0
  idx[idx[, 1] > nrow(m), 1] <- 0
  idx[idx[, 2] > ncol(m), 2] <- 0
  return (m[idx])
}

node1 = vector()
node2 = vector()

for (ind_a in 1:n_a) {
  for (ind_b in 1:n_b) {
    temp2 = get.nbhd(MM, ind_a, ind_b)
    temp1 = array(MM[ind_a, ind_b], length(temp2))
    node1 = c(node1, temp1)
    node2 = c(node2, temp2)
  }
}

see.the.nodes = cbind(node1, node2)

N_edges = length(node1) 

# Let's save the data we'll need!
save(y, x, node1, node2, N, N_edges, file = "transf_quakes.RData")