
import numpy as np
from scipy.cluster import vq

def cluster_slices_across_dims(focus_points):
  """ Cluster the slices across dimensions (ignoring dimension)
  """
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  nsamples = focus_points[0].num_slice_samples
  # cluster based on the input values as well as the outputs
  # this is a matrix where each row is a 1D slice
  obs = np.zeros((len(focus_points)*dims, nsamples))
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      for j,x in enumerate(s.slice):
        obs[(i*dims)+d, j] = x['y']
  wobs = vq.whiten(obs) # make everything uniform
  centroids,_ = vq.kmeans(wobs, 6)
  cluster_ids,_ = vq.vq(wobs, centroids)
  return cluster_ids

def cluster_slices_within_dims(focus_points):
  """ Cluster the slices within dimensions (separate clusters per dimension)
  """
  focus_points = list(focus_points) # the generator will break things
  dims = focus_points[0].dims
  nsamples = focus_points[0].num_slice_samples
  # cluster based on the input values as well as the outputs
  # this is list of matrice where each row is a 1D slice
  dim_obs = [np.zeros((len(focus_points), nsamples)) for d in range(dims)] # need to create a separate matrix per dimension
  cluster_ids = []
  for i,fp in enumerate(focus_points):
    for d,(s,obs) in enumerate(zip(fp.slices, dim_obs)):
      for j,x in enumerate(s.slice):
        obs[i, j] = x['y']
  # now cluster each dimension individually
  for obs in dim_obs:
    wobs = vq.whiten(obs) # make everything uniform
    centroids,_ = vq.kmeans(wobs, 3) # 3 centroids for now
    cids,_ = vq.vq(wobs, centroids)
    cluster_ids.append(cids)
  return cluster_ids # this is an array of cluster ids per dimension

def identify_clusters(focus_points):
  focus_points = list(focus_points) # the generator will break things
  cluster_ids = cluster_slices_within_dims(focus_points)
  for i,fp in enumerate(focus_points):
    for d, s in enumerate(fp.slices):
      s.cluster_id = int(cluster_ids[d][i])
  return focus_points

