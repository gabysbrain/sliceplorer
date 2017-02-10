#!/bin/bash

n=500
if [ "$DEPLOY_ENV" == "production" ]; then
  host="http://sliceplorer.cs.univie.ac.at"
else
  host="http://localhost:5000"
fi

for file in `ls slice_samples/*_slices.csv*`; do
  tmp=${file#slice_samples/}
  tmp=${tmp%_slices.csv*}
  name=${tmp%_*}
  dim=${tmp##*_}

  url="${host}/slice/${name}/${dim}/${n}"
  echo $url
  curl ${url} > /dev/null
done

