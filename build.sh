#!/bin/bash

loc="$PWD"
cd "$loc"

cd DirServer
stack build

cd ../FileServer
stack build

cd ../AuthServer
stack build

cd ../Client
stack build
