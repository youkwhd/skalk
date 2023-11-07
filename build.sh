#!/bin/sh

BIN=skalk
SRC=$(find -name "*.hs")

ghc $SRC -o $BIN
