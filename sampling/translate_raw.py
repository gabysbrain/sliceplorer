
from sys import argv

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

if __name__ == '__main__':
  xsize, ysize, zsize = data_sizes(argv[1])
  #print("x1,x2,x3,y1")
  with open(argv[1], "rb") as file:
    slices = slice_func(xsize, ysize, zsize, file)
    for x in range(xsize):
      for y in range(x, ysize):
        for z in range(y, zsize):
          for d in range(3):
            for s in slices(d, x, y, z):
              print("%s,%s,%s,%s" % s)

