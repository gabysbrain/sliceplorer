
from flask import Flask, Response, json

from compute_stats import convert_slices

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
  return Response(response=json.dumps(list(s), cls=MyEncoder),
      status=200, mimetype="application/json")

if __name__ == '__main__':
  app.debug = True
  app.run()

