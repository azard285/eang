#!/bin/sh

docker run -it --rm -p 2222:22 -p 5060:5060 -p 5061:5061 nksip-image /bin/sh
