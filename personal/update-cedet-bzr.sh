#!/bin/bash

cd cedet-bzr
bzr update
make clean-all
make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
