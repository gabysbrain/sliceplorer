---
task: Find anomalies
technique: Contour tree
dataset: Ackley
dims: 6
samplingMethod: Sobol
numSamples: 6000
neighborhoodMethod: Gabriel graph
---

The graph layout algorithm tries to lay out the tree so that the y-position of
the nodes relates to the function value. Because the spatial locations of the
points are not taken into account it is very difficult to see if there are
areas of the manifold that stand out from others.


