
from sys import argv
import numpy as np
from ortho_slices import sobol

def data_sizes(fname):
  tmp = fname.split('.')
  return [int(x) for x in tmp[-3].split('x')]

def accessor_func(xsize, ysize, zsize, data):
  def getval(x, y, z):
    datacoord = x * (ysize*zsize) + y * zsize + z
    data.seek(datacoord)
    rawval = data.read(1)
    return ord(rawval)
  return getval

def slice_func(xsize, ysize, zsize, data):
  af = accessor_func(xsize, ysize, zsize, data)
  def slice(d, x, y, z):
    if d == 0:
      slices = [(xx, y, z, af(xx, y, z)) for xx in range(xsize)]
    elif d == 1:
      slices = [(x, yy, z, af(x, yy, z)) for yy in range(ysize)]
    elif d == 2:
      slices = [(x, y, zz, af(x, y, zz)) for zz in range(zsize)]
    return slices
  return slice

def sobol_samples(n, xsize, ysize, zsize):
  vals = sobol(n, 3, 0)
  vals[:,0] = np.interp(vals[:,0], [0,1], [0, xsize-1])
  vals[:,1] = np.interp(vals[:,1], [0,1], [0, ysize-1])
  vals[:,2] = np.interp(vals[:,2], [0,1], [0, zsize-1])
  #print(max(vals))
  vals = np.intc(np.round(vals))
  # FIXME: there's a rounding problem here...
  return vals

if __name__ == '__main__':
  xsize, ysize, zsize = data_sizes(argv[2])
  #print("x1,x2,x3,y1")
  sample_n = int(argv[1])
  with open(argv[2], "rb") as file:
    get_slice = slice_func(xsize, ysize, zsize, file)
    print("x1,x2,x3,y")
    for [x,y,z] in sobol_samples(sample_n, xsize, ysize, zsize):
      for d in range(3):
        for s in get_slice(d, x, y, z):
          print("%s,%s,%s,%s" % s)
    # for x in range(xsize): # there are quite a few repeats in the slices this way
      # for y in range(ysize):
        # for z in range(zsize):
          # for d in range(3):
            # for s in slices(d, x, y, z):
              # print("%s,%s,%s,%s" % s)

    # x slices
    # for y in range(ysize):
      # for z in range(zsize):
        # for s in slices(0, 0, y, z):
          # print("%s,%s,%s,%s" % s)
    # # y slices
    # for x in range(xsize):
      # for z in range(zsize):
        # for s in slices(0, 0, y, z):
          # print("%s,%s,%s,%s" % s)
    # # z slices
    # for x in range(xsize): # there are quite a few repeats in the slices this way
      # for y in range(ysize):
        # for s in slices(0, 0, y, z):
          # print("%s,%s,%s,%s" % s)

