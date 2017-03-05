---
dataset: Fuel
dims: 3
samplingMethod: Given (Cartesian)
numSamples: 64x64x64
neighborhoodMethod: Cartesian
---

The graph layout algorithm tries to lay out the tree so that the y-position of
the nodes relates to the function value. The contour tree only shows extrema
and saddle points so these are the only points possible for which we can find
the function value. In order to get some sense of what function values are
over a particular value we first find the node with that value in the tree.
Then we can estimate the proportion of input values above this value by the
proportion of nodes that are above the one chosen.



