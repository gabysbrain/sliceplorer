
import pandas as pd
from ortho_slices import SAMPLE_N

class SliceGroup(object):
  def __init__(self, xs):
    dims = len(xs.columns) - 1

    # compute the sample location
    s1,s2 = xs.iloc[0,0:dims], xs.iloc[SAMPLE_N,0:dims]
    s = s1.copy()
    s[0] = s2[0]

    self.slice = list(s)
    self.dims = dims
    self.slices = list(dim_groups(xs))

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

def slice_groups(samples):
  dims = len(samples.columns) - 1
  step_size = dims * SAMPLE_N

  for i in range(0, len(samples), step_size):
    yield SliceGroup(samples[i:(i+step_size)])

def dim_groups(slices):
  dims = len(slices.columns) - 1
  for i,d in zip(range(0, len(slices), SAMPLE_N), range(dims)):
    yield Slice(d, slices[i:(i+SAMPLE_N)])

def gradients(slice, d):
  diffs = slice.diff()
  gs = diffs.iloc[:,-1] / diffs.iloc[:,d]
  return gs

def convert_slices(fname, dims):
  fname = 'slice_samples/%s_%s_slices.csv' % (fname, dims)
  # TODO: check for file existence

  s = pd.read_csv(fname, header=None)

  # set the column names
  col_names = ['x%s' % i for i in range(1, len(s.columns))] + ['y']
  s.columnns = col_names

  return slice_groups(s)

def run():
  s = convert_slices('ackley', 2)

  print([x for x in s][0:2])

if __name__ == '__main__':
  run()

