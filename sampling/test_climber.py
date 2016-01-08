
import pandas as pd
import numpy as np
#import os
import subprocess
import json
from sys import argv, stderr
import pygraphviz as pgv
import re
from tempfile import mkdtemp
from time import process_time
from sampler import sample
from pymonad import *
import ct
from multiprocess import Pool

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
  def __init__(self, nodes, mesh_edges, path_edges, ct_edges):
    self.nodes = nodes
    self.mesh_edges = mesh_edges
    self.path_edges = path_edges
    self.ct_edges = ct_edges

class Times(object): # times are in seconds!
  def __init__(self, sampling, pathing, contourtreeing):
    self.sampling = sampling
    self.pathing = pathing
    self.contourtreeing = contourtreeing
  def __str__(self):
    return "sampling: %ssec pathing: %ssec ct: %ssec" % \
        (self.sampling, self.pathing, self.contourtreeing)

def sample_dict(d):
  # random sampling
  return {k: np.random.choice(v) for k,v in d.items()}

def write_samples(basename, samples):
  csvfile = basename + '.csv'
  #print(samples.columns)
  #samples = samples.rename({'y1':'y'}) # rename y column
  #print(samples.columns)
  samples.to_csv(csvfile, index=False)
  print(csvfile, file=stderr)
  return csvfile

def convert_meshparam(mesh, meshparam):
  (argname, mn, mx) = MARGMAP[mesh]
  argval = mn + meshparam * (mx-mn)
  if mesh == 'knn': argval = int(round(argval))
  return argname, argval

def meshargs(mesh, meshparam):
  argname, argval = convert_meshparam(mesh, meshparam)
  return "--neighborhood=%s" % (mesh), "--%s=%s" % (argname, argval)

def climber(basename, samples, mesh, meshparam, distFunName, pathGen, pathAccum):
  inputcsv = write_samples(basename, samples)
  pathcsv = basename + '.path.csv'
  mesharg, meshval = meshargs(mesh, meshparam)
  climber_args = [
    CLIMBER,
    "--jitter", 
    mesharg, 
    meshval,
    "--distanceFunction=%s" % (distFunName),
    "--pathGen=%s" % (pathGen),
    "--pathAccum=%s" % (pathAccum),
    "--graph=%s" % (basename),
    "--pathdata=%s" % (pathcsv),
    inputcsv
  ]
  start_t = process_time()
  subprocess.run(climber_args, timeout=60)
  #os.system("%s %s" % (CLIMBER, climber_args))
  end_t = process_time()
  mesh = pgv.AGraph(basename + '_bskel.dot')
  pathgr = pgv.AGraph(basename + '_path.dot')
  pathcsv = pd.read_csv(basename + '.path.csv')

  return (end_t-start_t), mesh, pathgr, pathcsv

def contourtree(basename, mesh):
  start_t = process_time()
  ctg = ct.contourtree(mesh)
  end_t = process_time()
  ct.savetree(basename + '_ct.dot', ctg)
  ctgraph = pgv.AGraph(basename + '_ct.dot')
  return (end_t-start_t), ctgraph

def combine_graphs(mesh, pathgraph, pathcsv, ct):
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

  ct_edges = [[i, e[0], e[1]] for i, e in enumerate(ct.edges())]
  ct_edges = pd.DataFrame(ct_edges, columns=['id', 'source', 'target'])

  return Graph(nodes, mesh_edges, path_edges, ct_edges)

def run_sample():
  s = sample_dict(SAMPLESET)
  # also append the mesh info
  argname, meshval = convert_meshparam(s['meshing'], s['meshparam'])
  s[argname] = meshval
  print("sampling", s, file=stderr)
  st_start = process_time()
  samples = sample(s['function'], s['sampling'], s['N'])
  st_end = process_time()
  dirname = mkdtemp()
  #print(s)
  try:
    pathtime, mesh, pathg, path1d = climber(dirname + "/climber", samples,
      s['meshing'], s['meshparam'], 
      s['distance'], 
      s['pathGen'], s['pathAccum'])
    cttime, ctg = contourtree(dirname + '/climber', mesh)
    t = Times(st_end-st_start, pathtime, cttime)
    gg = combine_graphs(mesh, pathg, path1d, ctg)
    return Right((s, t, gg))
  except Exception as e:
    #raise
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
      "path_edges": graphs.path_edges.to_dict(orient='records'),
      "ct_edges": graphs.ct_edges.to_dict(orient='records')
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
  return json.dumps([to_dict(s) for s in samples], default=str, indent=2)

def sample_sync(n):
  return [run_sample() for i in range(n)]

def _fake_run(x):
  return run_sample()

def sample_async(n):
  pool = Pool()
  result = pool.map_async(_fake_run, range(n))
  print(type(result), file=stderr)
  print(result, file=stderr)
  samples = result.get(1)
  return samples

if __name__ == '__main__':
  if len(argv) < 2:
    print("number of samples is required", file=stderr)
    exit(-1)

  num_samples = int(argv[1])
  #samples = sample_sync(num_samples)
  samples = sample_sync(num_samples)
  print(to_json(samples))
  # TODO: generate contour tree too
  # TODO: generate images for the graphs (maybe)
  # TODO: generate html browser for all the graphs (maybe)

