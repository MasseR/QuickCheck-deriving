#!/usr/bin/env bash

cabal2nix ./. > default.nix

git diff -q
