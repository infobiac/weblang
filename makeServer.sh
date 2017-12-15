#!/bin/bash

rm -rf .libs

cd ./libmicrohttpd-0.9.55/src/examples; make;

echo "copying executable"
cp post_example ./runWeblangServer

echo "copying libs"
cd -
cp -rf ./libmicrohttpd-0.9.55/src/examples/.libs .
