
import numpy as np
from scipy.cluster import vq

def cluster_slices(focus_points):
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  obs = np.zeros((len(focus_points)*dims, 21))
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      for j,x in enumerate(s.slice):
        obs[(i*dims)+d, j] = x['y']
  wobs = vq.whiten(obs) # make everything uniform
  centroids,_ = vq.kmeans(wobs, 7)
  cluster_ids,_ = vq.vq(wobs, centroids)
  return cluster_ids

def assign_clusters(focus_points):
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  cluster_ids = cluster_slices(focus_points)
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      s.cluster_id = cluster_ids[i*dims+d]
  return focus_points

