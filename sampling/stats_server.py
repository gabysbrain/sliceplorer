
from flask import Flask, Response, json
import itertools

from slice_list import slice_list
from slice_list import get as get_slices
import optim_trace

app = Flask(__name__)

class MyEncoder(json.JSONEncoder):
  def default(self, o):
    try:
      return o.__dict__
    except AttributeError:
      return str(o)

def tojson(obj):
  resp = Response(response=json.dumps(obj, cls=MyEncoder),
      status=200, mimetype="application/json")
  resp.headers['Access-Control-Allow-Origin'] = 'http://localhost:3000'
  return resp
  

@app.route('/slice', methods = ['GET'])
def slices():
  s = [{'dataset': ds, 'dims': d} for ds,d in slice_list()]
  return tojson(s)

@app.route('/slice/<function>/<int:dims>/<int:limit>', methods = ['GET'])
def slice_req(function, dims, limit):
  s = get_slices(function, dims)
  s = list(itertools.islice(s, limit))
  return tojson(s)

@app.route('/trace', methods = ['GET'])
def optim_traces():
  s = list(optim_trace.trace_list())
  return tojson(s)

@app.route('/trace/<function>/<int:dims>/<int:run>', methods = ['GET'])
def get_trace(function, dims, run):
  s = optim_trace.load_trace(function, dims, run)
  return tojson(json.loads(s.to_json()))

if __name__ == '__main__':
  app.debug = True
  app.run(threaded=True)

