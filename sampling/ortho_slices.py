
import argparse
#import sampler
import numpy as np
#from scipy.interpolate import interp1d
from sobol import i4_sobol
from sys import stdout
import math

FUNCTIONS = ["spherical", "ackley", "rosenbrock", "schwefel", "sinc", "zakharov"]
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
  #(mn, mx) = sampler.get_limits(fname)
  (mn, mx) = (-5, 5)
  vals = sobol(n, d, seed)
  vals = np.interp(vals, [0,1], [mn,mx])
  #dd = np.random.randint(0, d, n)
  #return (dd, vals)
  return vals

def sinc(x):
  out = 1.0
  for xx in x:
    if xx != 0.0:
      out *= math.sin(math.pi*xx) / (math.pi*xx)
  return out

def samples(fname, n, dim, seed=0):
  #(mn, mx) = sampler.get_limits(fname)
  (mn, mx) = (-5,5)
  #dd, X = sample_points(fname, n, dim, seed)
  X = sample_points(fname, n, dim, seed)
  outputs = np.zeros((n*dim*SAMPLE_N, dim+1))
  outputs[:,:-1] = np.repeat(X, dim*SAMPLE_N, axis=0)
  for i in range(0, outputs.shape[0], dim*SAMPLE_N): # samples
    for d,j in zip(range(dim), range(i,i+dim*SAMPLE_N,SAMPLE_N)): # dimensions
      slice_samples = np.linspace(mn, mx, num=SAMPLE_N)
      outputs[j:(j+SAMPLE_N),d] = slice_samples
  #f = sampler.get_func(fname)
  f = sinc
  #print(outputs)
  #print(dd)
  outputs[:,dim] = np.apply_along_axis(f, 1, outputs[:,:-1])
  newseed = seed + n
  return outputs, newseed

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
    dim_names = ["x%s" % dd for dd in range(1,args.d+1)] + ['y']
    outf.write((",".join(dim_names) + "\n").encode('utf-8'))
    for numsamples in chunk(args.n, 1000): # keep things small
      x,seed = samples(args.function, numsamples, args.d, seed)
      np.savetxt(outf, x, fmt='%10.5f', delimiter=',')
  print("final seed: %s" % (seed))

