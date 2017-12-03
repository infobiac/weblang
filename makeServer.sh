#!/bin/bash

cd ./libmicrohttpd-0.9.55/src/examples; make;

cp post_example ./runWeblangServer
cp -rf .libs/ ./plt/
