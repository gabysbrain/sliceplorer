
from glob import iglob
from os.path import basename
import pandas as pd
import re

def trace_list():
  trace = []
  for fname in sorted(iglob("optim_traces/start_*.csv")):
    m = re.match(r"start_([0-9]*?)\.csv", basename(fname))
    run_id = m.group(1)
    trace.append(run_id)
  yield {'ackley': {5: trace}}

def load_trace(fname, dims, run_id):
  fname = 'optim_traces/start_%s.csv' % (run_id)
  s = pd.read_csv(fname)
  return s

if __name__ == '__main__':
  print(list(slice_list()))

