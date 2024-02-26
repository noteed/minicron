#! /usr/bin/env bash

STACK_IMAGE=${1:-8.0.2}

docker run \
  -v `pwd`:/source \
  -t -i images.reesd.com/reesd/stack:$STACK_IMAGE \
  sh -c 'cd /source ; ghc --version ; cabal install'
