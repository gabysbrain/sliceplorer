
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr, SignatureTranslatedAnonymousPackage
from rpy2.robjects import pandas2ri
from rpy2.robjects import default_converter
from rpy2.robjects.conversion import localconverter

def sample(fname, method, N):
  methodf = method + '.sample'
  with open('s.R') as f:
    s = SignatureTranslatedAnonymousPackage(f.read(), 'sampling')
  with localconverter(default_converter + pandas2ri.converter) as cv:
    d = s.sample(fname, methodf, int(N), 2)
    pd = pandas2ri.ri2py(d)
  return pd

if __name__ == '__main__':
  print(sample('spherical', 'lh', 10))
  print(type(sample('spherical', 'lh', 10)))

