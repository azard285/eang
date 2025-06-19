#!/bin/sh

docker run -it --rm -v `pwd`/nksip_samples/_build:/buildroot/nksip_samples/_build -p 2222:22 -p 5060:5060 -p 5061:5061 nksip-samples-image /bin/sh

