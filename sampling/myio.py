
import json
import numpy as np

class MyEncoder(json.JSONEncoder):
  def default(self, o):
    if hasattr(o, '__dict__'):
      return o.__dict__
    if type(o) == np.int64:
      return np.asscalar(o)
    return repr(o)
    #return json.JSONEncoder.default(self, o)
    #except AttributeError:
      #return str(o)

