---
samplingMethod: Sobol
numSamples: 6000
neighborhoodMethod: Gabriel graph
---

The 6D Ackley function has many local minima and maxima and this is reflected
in the complexity of the tree. The graph layout algorithm tries to lay out the
tree so that the y-position of the nodes relates to the function value. The
contour tree only shows extrema and saddle points so these are the only points
possible for which we can find the function value. In addition, since the
points have no particular organization with respect to their input values
one must scan through all the points to find a particular input value 
configuration.

