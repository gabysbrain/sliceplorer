
import pygraphviz as pgv
from tempfile import NamedTemporaryFile
from sys import argv
import os 
import re

def _ctree_node(n):
  label = n.attr['label']
  #pid = int(n.attr['pid'])
  m = re.search(r"-> \(([^)]+)\)", label)
  value = m.group(1)
  return "%s\n" % (value)

def _ctree_edge(graph, e):
  (n1, n2) = (graph.get_node(e[0]), graph.get_node(e[1]))
  (pid1, pid2) = (int(n1.attr['pid']), int(n2.attr['pid']))
  return "%s\t%s\n" % (pid1, pid2)

def ctree_nodes(graph):
  return [_ctree_node(graph.get_node(nid)) for nid in graph.nodes()]

def ctree_edges(graph):
  return [_ctree_edge(graph, e) for e in graph.edges()]

def _convert_node(line):
  (pid, value) = line.split("\t")
  pid = int(pid)
  return 'u%s[pid="%s",label="(%s)"];' % (pid+1,pid,value)

def _convert_edge(line):
  (pid1, pid2) = int(line.split("\t")[0]), int(line.split("\t")[1])
  return 'u%s -> u%s[label=""];' % (pid1+1, pid2+1)

def convert_nodes(nodelines):
  return [_convert_node(line) for line in nodelines]

def convert_extra_nodes(edgelines):
  out = []
  for line in edgelines:
    tmp = line.split("\t")
    for i in range(2, len(tmp), 2):
      ntxt = tmp[i] + "\t" + tmp[i+1]
      n = _convert_node(ntxt)
      out.append(n)
  return out

def convert_edges(edgelines):
  return [_convert_edge(line) for line in edgelines]

def convert_graph(treelines):
  numNodes = int(treelines[0])
  nodes = convert_nodes(treelines[1:numNodes+1])
  edges = convert_edges(treelines[numNodes+1:])
  extra_nodes = convert_extra_nodes(treelines[numNodes+1:])
  return "digraph G {\n%s\n%s\n%s\n}" % ("\n".join(nodes), 
                                         "\n".join(extra_nodes), 
                                         "\n".join(edges))

def contourtree(graph):
  nodefile = NamedTemporaryFile(mode='w', delete=False)
  nodefile.writelines(ctree_nodes(graph))
  nodefile.close()
  edgefile = NamedTemporaryFile(mode='w', delete=False)
  edgefile.writelines(ctree_edges(graph))
  edgefile.close()
  treefile = NamedTemporaryFile(mode='r', delete=False)
  #treefile.close()

  ctree = "/Applications/Denali.app/Contents/MacOS/ctree"
  cmd = "%s %s %s %s" % (ctree, nodefile.name, edgefile.name, treefile.name)
  print(cmd)
  os.system(cmd)

  graph_txt = None
  with open(treefile.name) as f:
    lines = [line.rstrip('\n') for line in f.readlines()]
    graph_txt = convert_graph(lines)

  # clean up
  treefile.close()
  os.remove(nodefile.name)
  os.remove(edgefile.name)
  os.remove(treefile.name)

  return graph_txt

def savetree(filename, tree_txt):
  with open(filename, 'w') as f:
    f.write(tree_txt)

if __name__ == '__main__':
  dotfile = argv[1]
  graph = pgv.AGraph(dotfile)
  print(contourtree(graph))

