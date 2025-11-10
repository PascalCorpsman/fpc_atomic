#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa -weak_framework UserNotifications      -order_file /Users/pavelzverina/AiProjects/fpc_atomic_macos/symbol_order.fpc -multiply_defined suppress -L. -o /Users/pavelzverina/AiProjects/fpc_atomic_macos/cd_data_extractor_nogui `cat /Users/pavelzverina/AiProjects/fpc_atomic_macos/link91806.res` -filelist /Users/pavelzverina/AiProjects/fpc_atomic_macos/linkfiles91806.res
if [ $? != 0 ]; then DoExitLink ; fi
IFS=$OFS
