
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr, SignatureTranslatedAnonymousPackage
from rpy2.robjects import pandas2ri
from rpy2.robjects import default_converter
from rpy2.robjects.conversion import localconverter

s = None
with open('s.R') as f:
  s = SignatureTranslatedAnonymousPackage(f.read(), 'sampling')

def sample(fname, method, N):
  methodf = method + '.sample'
  with localconverter(default_converter + pandas2ri.converter) as cv:
    d = s.sample(fname, methodf, int(N), 2)
    pd = pandas2ri.ri2py(d)
  return pd

def get_limits(fname):
  r = s.range(fname)
  rr = dict(zip(r.names, list(r)))
  return (float(rr['min'][0]), float(rr['max'][0]))

def get_func(fname):
  r = s.getf(fname)
  # def tmp(x):
    # print(x.shape)
    # print(r(robjects.FloatVector(x)))
    # return r(robjects.FloatVector(x))[0]
  # return tmp
  return lambda x: r(robjects.FloatVector(x))[0]
  #print(r)

if __name__ == '__main__':
  print(sample('spherical', 'lh', 10))
  print(type(sample('spherical', 'lh', 10)))

