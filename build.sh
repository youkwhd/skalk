#!/bin/sh

BIN=skalk
SRC=$(find src/ -name "*.hs")

ghc $SRC -o $BIN
