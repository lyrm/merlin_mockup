#!/usr/bin/env bash

./_build/default/ox/main.exe& # Launch server
PID=$!

sleep 0.1

echo -en "test/test1\npart 1" | nc localhost 8080 >/dev/null
echo -en "test/test1\npart 3" | nc localhost 8080 >/dev/null
echo -en "test/test1\npart 12" | nc localhost 8080 >/dev/null
echo -en "test/test1\npart 20" | nc localhost 8080 >/dev/null
echo -en "test/test1\npart 10" | nc localhost 8080 >/dev/null

kill -INT $PID
