
from glob import iglob
from os.path import basename
import os
import re
import json
import numpy

from compute_stats import convert_slices, SliceGroup, Slice
from cluster_slices import identify_clusters
from slice_neighbors import slice_neighbors
from myio import MyEncoder

JSONDIR = "json_cache"

def csv_slices():
  for fname in sorted(iglob("slice_samples/*_*_slices.csv")):
    m = re.match(r"(.*?)_([0-9]*?)_slices\.csv", basename(fname))
    group_name = m.group(1)
    dims = int(m.group(2))
    yield (group_name, dims)

def gz_csv_slices():
  for fname in sorted(iglob("slice_samples/*_*_slices.csv.gz")):
    m = re.match(r"(.*?)_([0-9]*?)_slices\.csv\.gz", basename(fname))
    group_name = m.group(1)
    dims = int(m.group(2))
    yield (group_name, dims)

def slice_list():
  curname = None
  dimlist = []
  for group_name, dims in sorted(list(csv_slices()) + list(gz_csv_slices())):
    if curname != group_name:
      dimlist.sort()
      if curname != None: yield (curname, dimlist)
      curname = group_name
      dimlist = []
    dimlist.append(dims)
  dimlist.sort()
  if curname != None:
    yield (curname, dimlist)

def get(function, dims):
  cachefile = JSONDIR + "/cache_%s_%s.json" % (function, dims)
  # TODO: check code hash and data mod date
  f = None
  try:
    f = open(cachefile, 'r')
    s = json.load(f)
    s = [SliceGroup.from_dict(sg) for sg in s]
  except OSError:
    s = convert_slices(function, dims)
    s = identify_clusters(s)
    s = slice_neighbors(s)

    # save the cache file
    if not os.path.exists(JSONDIR):
      os.makedirs(JSONDIR)
    f = open(cachefile, 'w')
    json.dump(s, f, cls=MyEncoder)
  finally:
    if f: f.close()
  return s

if __name__ == '__main__':
  print(list(slice_list()))

