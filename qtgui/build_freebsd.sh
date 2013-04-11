#! /usr/bin/env bash

export MAKE=gmake
export C_INCLUDE_PATH=/usr/local/include
export CPLUS_INCLUDE_PATH=/usr/local/include

ocaml build.ml
