
import numpy as np
from scipy import spatial

def slice_neighbors(slices):
  slices = list(slices)
  # dims = slices[0].dims
  # n = len(slices)
  # if n == 1:
    # slices[0].neighbor_group_ids = []
    # return slices
  # elif n == 2:
    # slices[0].neighbor_group_ids = [slices[1].group_id]
    # slices[1].neighbor_group_ids = [slices[0].group_id]
    # return slices
  # focus_points = np.zeros((n, dims))
  # for i,fp in enumerate(slices):
    # focus_points[i,:] = np.array(fp.slice)

  # delaunay = spatial.Delaunay(focus_points)

  for i,fp in enumerate(slices):
    # nbr_ids = [slices[i].group_id for i in neighbor_indices(delaunay, i)]
    # fp.neighbor_group_ids = nbr_ids
    fp.neighbor_group_ids = []
  return slices

def neighbor_indices(dt, idx):
  (indices, indptr) = dt.vertex_neighbor_vertices
  return indptr[indices[idx]:indices[idx+1]]

