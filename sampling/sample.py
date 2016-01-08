
import pandas as pd
import numpy as np
import os
import json
from sys import argv, stderr
import pygraphviz as pgv
import re
from tempfile import mkdtemp
from time import process_time
from sampler import sample
from pymonad import *

SAMPLESET = {
  "function":  ["spherical"],
  "sampling":  ["lh", "hexagonal", "torus", "sobol", "halton", "random"],
  "N":         [5, 10, 15, 25, 50, 75, 100, 250, 500, 1000],
  "meshing":   ["betaskeleton", "knn", "radius"],
  "meshparam": [x/10 for x in range(0, 11)],
  "distance":  ["euclidean", "gradient", "invGradient", "dist3d", "totalDist"],
  "pathGen":   ["mst", "greedy"],
  "pathAccum": ["onlysum", "posneg"]
}

MARGMAP = {
  "betaskeleton": ("beta", 0.5, 1.5),
  "knn": ("k", 1, 10),
  "radius": ("radius", 0.1, 5.0)
}

CLIMBER = '/Users/tom/Projects/mountain_climber_vis/src/lib/dist/build/Climber/climber'

class Graph(object):
  def __init__(self, nodes, mesh_edges, path_edges):
    self.nodes = nodes
    self.mesh_edges = mesh_edges
    self.path_edges = path_edges

class Times(object): # times are in seconds!
  def __init__(self, sampling, pathing):
    self.sampling = sampling
    self.pathing = pathing
  def __str__(self):
    return "sampling: %ssec pathing: %ssec" % (self.sampling, self.pathing)

def sample_dict(d):
  # random sampling
  return {k: np.random.choice(v) for k,v in d.items()}

def write_samples(basename, samples):
  csvfile = basename + '.csv'
  #print(samples.columns)
  #samples = samples.rename({'y1':'y'}) # rename y column
  #print(samples.columns)
  samples.to_csv(csvfile, index=False)
  print(csvfile)
  return csvfile

def convert_meshparam(mesh, meshparam):
  (argname, mn, mx) = MARGMAP[mesh]
  argval = mn + meshparam * (mx-mn)
  if mesh == 'knn': argval = int(round(argval))
  return argname, argval

def meshargs(mesh, meshparam):
  argname, argval = convert_meshparam(mesh, meshparam)
  return "--neighborhood=%s --%s=%s" % (mesh, argname, argval)

def climber(basename, samples, mesh, meshparam, distFunName, pathGen, pathAccum):
  inputcsv = write_samples(basename, samples)
  pathcsv = basename + '.path.csv'
  climber_args = " ".join([
    "--jitter", 
    meshargs(mesh, meshparam),
    "--distanceFunction=%s" % (distFunName),
    "--pathGen=%s" % (pathGen),
    "--pathAccum=%s" % (pathAccum),
    "--graph=%s" % (basename),
    "--pathdata=%s" % (pathcsv),
    inputcsv
  ])
  start_t = process_time()
  os.system("%s %s" % (CLIMBER, climber_args))
  end_t = process_time()
  mesh = pgv.AGraph(basename + '_bskel.dot')
  pathgr = pgv.AGraph(basename + '_path.dot')
  pathcsv = pd.read_csv(basename + '.path.csv')

  gg = combine_graphs(mesh, pathgr, pathcsv)

  return (end_t-start_t), gg

def combine_graphs(mesh, pathgraph, pathcsv):
  # The mesh is guaranteed to have all the nodes
  nodes = [[nid, mesh.get_node(nid).attr['pid'], mesh.get_node(nid).attr['label']] \
           for nid in mesh.nodes()]
  nodes = pd.DataFrame(nodes, columns=['id', 'pid', 'label'])
  #print(nodes)
  # computer the x1 and x2 values
  nodes['x1'] = pd.Series(
    nodes['label'].map(lambda l: float(re.search(r'\(([-0-9.]*?),', l).group(1))),
    index=nodes.index)
  nodes['x2'] = pd.Series(
    nodes['label'].map(lambda l: float(re.search(r', ([-0-9.]*?)\)', l).group(1))),
    index=nodes.index)

  # add the node info from the path graph
  nodes['distance'] = pd.Series(
    nodes['id'].map(lambda nid: float(pathgraph.get_node(nid).attr['d'])),
    index=nodes.index)
  nodes['value'] = pd.Series(
    nodes['id'].map(lambda nid: float(pathgraph.get_node(nid).attr['v'])),
    index=nodes.index)

  mesh_edges = [[i, e[0], e[1]] for i,e in enumerate(mesh.edges())]
  mesh_edges = pd.DataFrame(mesh_edges, columns=['id', 'source', 'target'])

  path_edges = [[i, e[0], e[1], 
                 float(re.search(r'd=(.*)', e.attr['label']).group(1)),
                 float(pathgraph.get_node(e[0]).attr['d']), 
                 float(pathgraph.get_node(e[0]).attr['v']),
                 float(pathgraph.get_node(e[1]).attr['d']), 
                 float(pathgraph.get_node(e[1]).attr['v'])] \
                     for i,e in enumerate(pathgraph.edges())]
  path_edges = pd.DataFrame(path_edges, 
    columns=['id', 'source', 'target', 'distance', 'd_1', 'v_1', 'd_2', 'v_2'])

  return Graph(nodes, mesh_edges, path_edges)

def run_sample():
  s = sample_dict(SAMPLESET)
  # also append the mesh info
  argname, meshval = convert_meshparam(s['meshing'], s['meshparam'])
  s[argname] = meshval
  print("sampling", s)
  st_start = process_time()
  samples = sample(s['function'], s['sampling'], s['N'])
  st_end = process_time()
  dirname = mkdtemp()
  #print(s)
  try:
    pathtime, g = climber(dirname + "/climber", samples,
      s['meshing'], s['meshparam'], 
      s['distance'], 
      s['pathGen'], s['pathAccum'])
    t = Times(st_end-st_start, pathtime)
    return Right((s, t, g))
  except Exception as e:
    return Left((s, str(type(e)) + str(e)))

def to_dict(rM):
  if isinstance(rM, Right):
    sample, times, graphs = rM.getValue()
    return {
      "sample": sample,
      "times": times,
      "success": True,
      "nodes": graphs.nodes.to_dict(orient='records'),
      "mesh_edges": graphs.mesh_edges.to_dict(orient='records'),
      "path_edges": graphs.path_edges.to_dict(orient='records')
    }
  else:
    sample, errmsg = rM.getValue()
    return {
      "sample": sample,
      "success": False,
      "errmsg": errmsg
    }

def to_json(samples):
  #print(to_dict(*samples[0]))
  return json.dumps([to_dict(s) for s in samples], default=str)

if __name__ == '__main__':
  if len(argv) < 2:
    print("number of samples is required", file=stderr)
    exit(-1)

  num_samples = int(argv[1])
  samples = [run_sample() for i in range(num_samples)]
  print(to_json(samples))
  # TODO: generate contour tree too
  # TODO: generate images for the graphs (maybe)
  # TODO: generate html browser for all the graphs (maybe)

