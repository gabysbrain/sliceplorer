
from sys import argv
from translate_raw import data_sizes, accessor_func

if __name__ == '__main__':
  xsize,ysize,zsize = data_sizes(argv[1])

  #print("x1,x2,x3,y1")
  with open(argv[1], "rb") as file:
    get_val = accessor_func(xsize, ysize, zsize, file)
    for x in range(xsize):
      for y in range(ysize):
        for z in range(zsize):
          print("%s,%s,%s,%s" % (x, y, z, get_val(x, y, z)))

