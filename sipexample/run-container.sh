docker run -it --rm --network host \
    -e SIP_USER=1000 \
    -e AUDIO_URL="http://icecast.omroep.nl/radio1-bb-mp3" \
    -e HOST_IP="192.168.186.227" \
    sipstream



# rebar3 shell
# c("src/sipstream.erl").
# sipstream:start().