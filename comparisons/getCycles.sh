#!/bin/bash

flite -r8:4:2:2:4 -i4 -s $1 > /tmp/templs
reduceron_emu -v /tmp/templs # | grep Ticks
