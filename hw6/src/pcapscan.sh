#!/bin/sh

erl -noshell -eval "pcapscan:file(\"${1}\")" -s init stop
