#!/bin/bash

cd /fpc_atomic
git pull

cp /temp/bass.dll /build/

cd /source/fpc_atomic/ && lazbuildl64 fpc_atomic.lpi && mv fpc_atomic /build
cd /source/fpc_atomic/ && lazbuildw64 fpc_atomic.lpi && mv fpc_atomic.exe /build
cd /source/fpc_atomic/ && lazbuildw32 fpc_atomic.lpi && mv fpc_atomic.exe /build/fpc_atomic32.exe

cd /source/cd_data_extractor && lazbuildl64 cd_data_extractor.lpi && mv cd_data_extractor /build