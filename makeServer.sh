#!/bin/bash

rm -rf .libs

cd ./libmicrohttpd-0.9.55/src/examples; make;

cd -
echo "copying executable"
cp ./libmicrohttpd-0.9.55/src/examples/post_example ./runWeblangServer

echo "copying libs"
cp -rf ./libmicrohttpd-0.9.55/src/examples/.libs .
