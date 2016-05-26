
import numpy as np
from scipy.cluster import vq

def cluster_slices(focus_points):
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  nsamples = focus_points[0].num_slice_samples
  obs = np.zeros((len(focus_points)*dims, nsamples))
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      for j,x in enumerate(s.slice):
        obs[(i*dims)+d, j] = x['y']
  wobs = vq.whiten(obs) # make everything uniform
  centroids,_ = vq.kmeans(wobs, 6)
  cluster_ids,_ = vq.vq(wobs, centroids)
  return cluster_ids

def identify_clusters(focus_points):
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  cluster_ids = cluster_slices(focus_points)
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      s.cluster_id = int(cluster_ids[i*dims+d])
  return focus_points

