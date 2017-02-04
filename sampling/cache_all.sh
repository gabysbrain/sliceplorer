#!/bin/sh

n=500
host="http://localhost:5000"

for file in `ls slice_samples/*_slices.csv*`; do
  tmp=${file#slice_samples/}
  tmp=${tmp%_slices.csv*}
  name=${tmp%_*}
  dim=${tmp##*_}

  url="${host}/slice/${name}/${dim}/${n}"
  echo $url
  curl ${url} > /dev/null
done

