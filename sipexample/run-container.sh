#!/bin/sh

# docker run -it --rm -p 2222:22 -p 5060:5060 -p 5061:5061 sipstream /bin/sh

# docker run -it --rm \
#     -p 5060:5060/udp \
#     -p 5061:5061/udp \
#     -p 4000:4000/udp \
#     -p 5004:5004/udp \
#     -e SIP_USER=1000 \
#     -e AUDIO_URL="http://icecast.omroep.nl/radio1-bb-mp3" \
#     sipstream

HOST_IP=192.168.186.227
docker run -it --rm --network host \
    -e SIP_USER=1000 \
    -e AUDIO_URL="http://icecast.omroep.nl/radio1-bb-mp3" \
    -e HOST_IP="$HOST_IP" \
    sipstream


# service ssh start
# rebar3 shell
# c("src/sipstream.erl").
# sipstream:start().