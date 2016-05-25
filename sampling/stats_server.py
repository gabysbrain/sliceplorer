
from flask import Flask, Response, json
import itertools

from compute_stats import convert_slices
from cluster_slices import identify_clusters

app = Flask(__name__)

class MyEncoder(json.JSONEncoder):
  def default(self, o):
    try:
      return o.__dict__
    except AttributeError:
      return str(o)

@app.route('/slice/<function>/<int:dims>', methods = ['GET'])
def slice_req(function, dims):
  s = convert_slices(function, dims)
  s = list(itertools.islice(s, 50))
  cs = identify_clusters(s)
  resp = Response(response=json.dumps(cs, cls=MyEncoder),
      status=200, mimetype="application/json")
  resp.headers['Access-Control-Allow-Origin'] = 'http://localhost:3000'
  return resp

if __name__ == '__main__':
  app.debug = True
  app.run()

