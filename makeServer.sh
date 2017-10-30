#!/bin/bash

cd ~/plt/libmicrohttpd-0.9.55/src/examples; make;

cp post_example ~/plt/runWeblangServer
cp -rf .libs/ ~/plt/
