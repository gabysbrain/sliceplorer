
from sys import argv
import os
import json
import gzip
import csv
import re

import locale
# set to austria to get euro formatted numbers
locale.setlocale(locale.LC_NUMERIC, "de_DE.UTF-8")

def sliceids():
  i = 0
  while True:
    yield i
    i += 1

idgen = sliceids()

def csvwriter(f):
  return csv.writer(f, delimiter=';')

def euronums(nums):
  return [locale.str(n) for n in nums]

def writeslice(data):
  os.makedirs('curves', exist_ok=True)
  slicefile = "curves/curve%s.csv" %(next(idgen))
  with open(slicefile, 'w') as f:
    slicecsv = csvwriter(f)
    slicecsv.writerow(['x','y'])
    for sd in data['slice']:
      slicecsv.writerow(euronums([sd['x'], sd['y']]))
  return slicefile

if __name__ == '__main__':
  data = None
  with gzip.open(argv[1], 'rt') as f:
    data = json.load(f)

  dims = data[0]['dim_names']
  with open('mainslices.csv', 'w') as f:
    maincsv = csvwriter(f)
    mainheader = dims + (['CurveNameXAxis@NameYAxis']*len(dims))
    maincsv.writerow(mainheader)

    for slicept in data:
      sliceinfo = euronums(slicept['slice'])

      # write the sub-slices first
      filenames = ["%s@x@y" % (writeslice(sd)) for sd in slicept['slices']]
      maincsv.writerow(sliceinfo + filenames)


