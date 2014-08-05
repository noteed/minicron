#! /usr/bin/env bash

docker run \
  -v `pwd`:/source \
  -t -i images.reesd.com/reesd/stack \
  sh -c 'cd /source ; ghci bin/minicron.hs'
