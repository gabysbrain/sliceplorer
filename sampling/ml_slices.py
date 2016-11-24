
import argparse
import numpy as np
from ortho_slices import sobol, chunk, SAMPLE_N

from rpy2.robjects import r
from rpy2.robjects.packages import importr
#from rpy2 import robjects
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

importr('mlegp')
importr('neuralnet')
importr('e1071')

parser = argparse.ArgumentParser(description='Generate 1D orthogonal slices from various ml models built in R.')
parser.add_argument('--seed', default=0, type=int)
parser.add_argument('mlfile', type=str)
parser.add_argument('n', type=int)
parser.add_argument('dest', type=str)

def sample_points(n, d, mn, mx, seed=0):
  vals = sobol(n, d, seed)
  for dd in range(d):
    vals[:,dd] = np.interp(vals[:,dd], [0,1], [mn[dd], mx[dd]])
  return vals

def run_samples(env_file, n, seed=0):
  r.load(env_file)
  mn = list(r.inputs[0])
  mx = list(r.inputs[1])
  if r.m.rclass[0] == "nn": # rclass is a tuple
    dim_names = list(r.colnames(r.get('data', r.m)))[:-1]
  elif r.m.rclass[0] == "svm":
    dim_names = list(r.colnames(r.X))
  else: # gp
    dim_names = list(r.colnames(r.get('X', r.m)))
  dim = len(dim_names)
  X = sample_points(n, dim, mn, mx, seed)
  outputs = np.zeros((n*dim*SAMPLE_N, dim+1))
  outputs[:,:-1] = np.repeat(X, dim*SAMPLE_N, axis=0)
  for i in range(0, outputs.shape[0], dim*SAMPLE_N):
    for d,j in zip(range(dim), range(i,i+dim*SAMPLE_N,SAMPLE_N)):
      slice_samples = np.linspace(mn[d], mx[d], num=SAMPLE_N)
      outputs[j:(j+SAMPLE_N),d] = slice_samples
  outputs[:,dim] = np.array(r.predict(r.m, outputs[:,:-1]))[:,0]
  newseed = seed + n
  return dim_names, outputs, newseed

if __name__ == '__main__':
  args = parser.parse_args()
  #print(sampler.get_limits('ackley'))
  #np.set_printoptions(threshold=np.nan)
  #print(samples(args.function, args.n, args.d, args.seed))
  seed = args.seed
  with open(args.dest, 'wb') as outf:
    for numsamples in chunk(args.n, 1000):
      dim_names,x,seed = run_samples(args.mlfile, numsamples, seed)
      outf.write((",".join(dim_names + ['y']) + "\n").encode('utf-8'))
      np.savetxt(outf, x, fmt="%10.5f", delimiter=",")
  print("final seed: %s" % (seed))


