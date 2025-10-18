#!/bin/bash
#docker run -it -v /${PWD}/AtomicGameRoot:/AtomicGameRoot -v /${PWD}/build:/build atomic bash
#docker run -it -v //d/Projekte/fpc_atomic/bomber/:/AtomicGameRoot -v /${PWD}/build:/build atomic bash
docker-compose -f docker-compose.yml up