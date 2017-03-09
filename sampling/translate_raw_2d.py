
import json
import numpy as np
from sys import argv
from translate_raw import accessor_func, sobol_samples, data_sizes

def slice2d_func(xsize, yzise, zsize, data):
  af = lambda x,y,z: float(accessor_func(xsize, ysize, zsize, data)(x,y,z))
  def slice(d1, d2, x, y, z):
    slices = None
    if d1 == 0 and d2 == 1:
      slices = [[af(xx, yy, z) for xx in range(xsize)] \
                  for yy in range(ysize)]
    if d1 == 0 and d2 == 2:
      slices = [[af(xx, y, zz) for xx in range(xsize)] \
                  for zz in range(zsize)]
    if d1 == 1 and d2 == 2:
      slices = [[af(x, yy, zz) for yy in range(ysize)] \
                  for zz in range(zsize)]
    return slices
  return slice

def slice_samples(file, xsize, ysize, zsize, n):
  get_slice = slice2d_func(xsize, ysize, zsize, file)
  for [x,y,z] in sobol_samples(n, xsize, ysize, zsize):
    for d1,d2 in [(0,1),(0,2),(1,2)]:
      s = get_slice(d1, d2, x, y, z)
      yield {
        'focusPoint': [np.asscalar(v) for v in [x,y,z]],
        'dim1': d1,
        'dim2': d2,
        'slice': s
      }

if __name__ == '__main__':
  xsize,ysize,zsize = data_sizes(argv[2])
  sample_n = int(argv[1])

  with open(argv[2], "rb") as file:
    slices = list(slice_samples(file, xsize, ysize, zsize, sample_n))
    print(json.dumps(slices))

