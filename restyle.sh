#!/usr/bin/env bash

find {src,test} -iname '*.hs' -exec stylish-haskell -i {} \;

git diff -q
