#!/usr/bin/env bash
# ./_build/default/lib/merlin_mockup.exe $1 &
./_build/default/ox/main.exe $1 &
PID=$!

sleep 1

echo -en "test/test1\npart 1" | nc -U $1 >/dev/null
echo -en "test/test1\npart 3" | nc -U $1 >/dev/null
echo -en "test/test1\npart 12" | nc -U $1 >/dev/null
echo -en "test/test1\npart 20" | nc -U $1 >/dev/null
echo -en "test/test1\npart 10" | nc -U $1 >/dev/null

kill -INT $PID
