#!/bin/sh

docker image rm sipstream

docker build -t sipstream .

docker images
