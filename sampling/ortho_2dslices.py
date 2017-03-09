
import argparse
import json
import sampler
import numpy as np
#from scipy.interpolate import interp1d
from sobol import i4_sobol
from sys import stdout

FUNCTIONS = ["spherical", "ackley", "rosenbrock", "schwefel", "zakharov"]
SAMPLE_N = 21 # sampling density

parser = argparse.ArgumentParser(description='Generate 1D orthogonal slices.')
parser.add_argument('--seed', default=0, type=int)
parser.add_argument('function', choices=FUNCTIONS)
parser.add_argument('n', type=int)
parser.add_argument('d', type=int)
parser.add_argument('dest', type=str)

def sobol(n, d, seed):
  x = np.zeros((n,d))
  for i in range(0,n):
    [x[i,0:d], seed] = i4_sobol(d, seed)
  return x

def sample_points(fname, n, d, seed=0):
  (mn, mx) = sampler.get_limits(fname)
  vals = sobol(n, d, seed)
  vals = np.interp(vals, [0,1], [mn,mx])
  #dd = np.random.randint(0, d, n)
  #return (dd, vals)
  return vals

def sample_slice(fname, dims, d1, d2, focuspt):
  (mn, mx) = sampler.get_limits(fname)
  outputs = np.zeros((SAMPLE_N*SAMPLE_N, dims+1))
  outputs[:,:-1] = np.repeat(focuspt, SAMPLE_N*SAMPLE_N, axis=0)
  samp = np.linspace(mn,mx,num=SAMPLE_N)
  outputs[:,d1] = np.repeat(samp, SAMPLE_N, axis=0)
  outputs[:,d2] = np.tile(samp, SAMPLE_N)
  f = sampler.get_func(fname)
  outptus[:,dims] = np.apply_along_axis(f, 1, outputs[:,:-1])
  return outputs

def slice_samples(fname, dims, focuspt):
  out = []
  for d1 in range(dims-1):
    for d2 in range(d+1, dims):
      out.append({
        focusPoint: focusPt,
        dim1: d1,
        dim2: d2,
        slice: sample_slice(fname, dims, d1, d2, focuspt)
      })
  return out

def samples(fname, n, dims, seed=0):
  (mn, mx) = sampler.get_limits(fname)
  #dd, X = sample_points(fname, n, dim, seed)
  X = sample_points(fname, n, dims, seed)
  ret = [slice_samples(fname, dims, sp) for sp in X]
  newseed = seed + n
  return ret, newseed

def chunk(x, n):
  for i in range(int(x/n)):
    yield n
  if x % n != 0:
    yield x % n

if __name__ == '__main__':
  args = parser.parse_args()
  #print(sampler.get_limits('ackley'))
  #np.set_printoptions(threshold=np.nan)
  #print(samples(args.function, args.n, args.d, args.seed))
  seed = args.seed
  with open(args.dest, 'wb') as outf:
    for numsamples in chunk(args.n, 1000): # keep things small
      x,seed = samples(args.function, numsamples, args.d, seed)
      json.dump(outf, x)
  print("final seed: %s" % (seed))

