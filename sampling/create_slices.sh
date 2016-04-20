#!/bin/sh

SEED=${1:-0}
FUNCTION="spherical ackley rosenbrock schwefel zakharov"
N=10000
D="2 3 4 5 6 7 8 9 10"

for fname in ${FUNCTION}; do
  for d in ${D}; do
    #echo ${fname} - ${d}
    (bin/python ortho_slices.py ${fname} ${N} ${d} --seed ${SEED} slice_samples/${fname}_${d}_slices.csv; echo ${fname}-${d} done) &
  done
done
wait

