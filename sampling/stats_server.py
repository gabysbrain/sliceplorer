
from flask import Flask, Response, json
import itertools

from compute_stats import convert_slices
from cluster_slices import identify_clusters
from slice_neighbors import slice_neighbors
from slice_list import slice_list

app = Flask(__name__)

class MyEncoder(json.JSONEncoder):
  def default(self, o):
    try:
      return o.__dict__
    except AttributeError:
      return str(o)

@app.route('/slice', methods = ['GET'])
def slices():
  s = [{'dataset': ds, 'dims': d} for ds,d in slice_list()]
  resp = Response(response=json.dumps(s, cls=MyEncoder),
      status=200, mimetype="application/json")
  resp.headers['Access-Control-Allow-Origin'] = 'http://localhost:3000'
  return resp

@app.route('/slice/<function>/<int:dims>/<int:limit>', methods = ['GET'])
def slice_req(function, dims, limit):
  s = convert_slices(function, dims)
  s = list(itertools.islice(s, limit))
  s = identify_clusters(s)
  s = slice_neighbors(s)
  resp = Response(response=json.dumps(s, cls=MyEncoder),
      status=200, mimetype="application/json")
  resp.headers['Access-Control-Allow-Origin'] = 'http://localhost:3000'
  return resp

if __name__ == '__main__':
  app.debug = True
  app.run()

