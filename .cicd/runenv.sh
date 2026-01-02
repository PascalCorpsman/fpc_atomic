#!/bin/bash
set -x

cd /source/fpc_atomic
git pull

rm -r /build/*
mkdir -p /build/DATA
cp /temp/bass.dll /build/

if [ "$ATOMIC_BUILD_LINUX" == "true" ]
then
  cd /source/fpc_atomic/ && lazbuildl64 fpc_atomic.lpi && mv fpc_atomic /build
fi

if [ "$ATOMIC_BUILD_WIN64" == "true" ]
then
  cd /source/fpc_atomic/ && lazbuildw64 fpc_atomic.lpi && mv fpc_atomic.exe /build
fi

if [ "$ATOMIC_BUILD_WIN32" == "true" ]
then
  cd /source/fpc_atomic/ && lazbuildw32 fpc_atomic.lpi && mv fpc_atomic.exe /build/fpc_atomic32.exe
fi

if [ -z "$(ls -A /AtomicGameRoot)" ] 
then
  echo "/AtomicGameRoot is empty"
else
  echo Found AtomicGameRoot DATA. Copying to build/DATA folder
  
  cd /source/fpc_atomic/cd_data_extractor && lazbuildl64 cd_data_extractor_nogui.lpi && lazbuildw64 cd_data_extractor_nogui.lpi
  # TODO: executing a CLI version of cd_data_extractor to convert data from /AtomicGameRoot/DATA/ to /build/DATA/

  cp cd_data_extractor_nogui /build
  cp cd_data_extractor_nogui.exe /build

  # not working due to gtk error on linux
  #./cd_data_extractor_nogui -cd /AtomicGameRoot -atomic /build
  # cp -r /AtomicGameRoot/DATA/* /build/DATA/

  if [ "$ATOMIC_EXTRA_ANI" == "true" ]
  then
    echo Download extra content
    wget https://www.oocities.org/timessquare/tower/4056/download/ani.zip --no-check-certificate -O /temp/ani.zip && unzip -jo /temp/ani.zip -d /build/DATA/ANI
  fi
fi
  
