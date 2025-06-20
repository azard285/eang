#!/bin/sh

docker image rm nksip-samples-image

docker build -t nksip-samples-image .

docker images
