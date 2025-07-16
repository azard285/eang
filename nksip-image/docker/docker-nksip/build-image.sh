#!/bin/sh

docker image rm nksip-image

docker build -t nksip-image .

docker images
