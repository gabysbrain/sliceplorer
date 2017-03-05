---
dataset: Ackley
dims: 6
samplingMethod: Sobol
numSamples: 6000
neighborhoodMethod: Gabriel graph
---

While the contour tree does not take the spatial locations of the input 
samples into account, we would expect to see repeating patterns of saddle 
and extrema due to the periodic behavior of the Ackley function. One can see
some of this, for example, in the upper part of the graph where we can see 
similar-looking 3 branch tree structures. However, without knowing beforehand
that these patterns exist it is not easy to understand what these repeating
patterns mean.

