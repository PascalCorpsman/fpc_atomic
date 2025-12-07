#!/bin/bash
echo
echo This is the fpc_atomic sound installation script for Linux
echo you only have to run this script on the very first time
echo you want to use the game.
echo
echo You need root rights to install bass.so
echo 
echo Press Return to continue
echo 
read cmd
sudo cp libbass.so /usr/lib
sudo chmod o+r /usr/lib/libbass.so
echo 
echo Finished, have a nice day
echo 
