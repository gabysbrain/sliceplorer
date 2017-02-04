
import pandas as pd
import numpy as np
import os
import gzip

class SliceGroup(object):
  def __init__(self, group_id, sample_n, xs):
    dims = len(xs.columns) - 1

    # compute the sample location
    s1,s2 = xs.iloc[0,0:dims], xs.iloc[sample_n,0:dims]
    s = s1.copy()
    s[0] = s2[0]

    self.group_id = group_id
    self.slice = list(s)
    self.dim_names = [x.strip() for x in xs.columns[:-1]]
    self.slices = list(dim_groups(sample_n, xs))

  @property
  def num_slice_samples(self):
    return len(self.slices[0].slice)

  @property
  def dims(self):
    return len(self.dim_names)

  @property
  def to_json(self):
    return { 'group_id': self.group_id,
             'slice': self.slice,
             'dim_names': self.dim_names,
             'slices': self.slices }

  @classmethod
  def from_dict(cls, o):
    self = SliceGroup.__new__(SliceGroup)
    self.group_id = o['group_id']
    self.slice = o['slice']
    self.dim_names = o['dim_names']
    self.slices = [Slice.from_dict(s) for s in o['slices']]

    # optional things
    if 'neighbor_group_ids' in o:
      self.neighbor_group_ids = o['neighbor_group_ids']
    return self

class Slice(object):
  def __init__(self, d, samples):
    self.d = d
    self.dims = len(samples.columns) - 1
    self.variance = samples.iloc[:,-1].var()
    self.min_value = samples.iloc[:,-1].min()
    self.max_value = samples.iloc[:,-1].max()
    self.avg_value = samples.iloc[:,-1].mean()
    g = gradients(samples, d)
    self.avg_gradient = g.mean()
    self.avg_pos_gradient = g.abs().mean()

    self.slice = list(samples.apply(lambda x: {'x': float(x.iloc[d]), 
                                               'y': float(x.iloc[-1])}, 
                                    axis=1))

  @property
  def to_json(self):
    return { 'd': self.d,
             'dims': self.dims,
             'variance': self.variance,
             'min_value': self.min_value,
             'max_value': self.max_value,
             'avg_value': self.avg_value,
             'avg_gradient': self.avg_gradient,
             'avg_pos_gradient': self.avg_pos_gradient,
             'slice': self.slice }

  @classmethod
  def from_dict(cls, o):
    self = Slice.__new__(Slice)
    self.d = o['d']
    self.dims = o['dims']
    self.variance = o['variance']
    self.min_value = o['min_value']
    self.max_value = o['max_value']
    self.avg_value = o['avg_value']
    self.avg_gradient = o['avg_gradient']
    self.avg_pos_gradient = o['avg_pos_gradient']
    self.slice = o['slice']
    # optional things
    if 'cluster_id' in o:
      self.cluster_id = o['cluster_id']
    return self

def slice_groups(samples):
  dims = len(samples.columns) - 1

  start_idxs = np.where(samples.iloc[:,0]==samples.iloc[0,0])
  sample_count = start_idxs[0][1]

  step_size = dims * sample_count

  for i in range(0, len(samples), step_size):
    yield SliceGroup(i, sample_count, samples[i:(i+step_size)])

def dim_groups(sample_n, slices):
  dims = len(slices.columns) - 1
  for i,d in zip(range(0, len(slices), sample_n), range(dims)):
    yield Slice(d, slices[i:(i+sample_n)])

def gradients(slice, d):
  diffs = slice.diff()
  gs = diffs.iloc[:,-1] / diffs.iloc[:,d]
  return gs

def convert_slices(fname, dims):
  csv_fname = 'slice_samples/%s_%s_slices.csv' % (fname, dims)
  gz_fname = 'slice_samples/%s_%s_slices.csv.gz' % (fname, dims)
  # TODO: check for file existence
  f = None
  try:
    if os.path.isfile(gz_fname):
      f = gzip.open(gz_fname, 'r')
    else:
      f = open(csv_fname, 'r')

    #s = pd.read_csv(fname, dtype='float64')
    s = pd.read_csv(f)
  finally:
    f.close()

  return slice_groups(s)

def run():
  s = convert_slices('fuel', 3)

  print([x for x in s][0:2])

if __name__ == '__main__':
  run()

