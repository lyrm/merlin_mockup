#!/usr/bin/env bash

_build/default/ox/main.exe& # Launch server
PID=$!

sleep 0.1

echo -en "test/test1\nall" | nc localhost 8453 >/dev/null
echo -en "test/test1\nall" | nc localhost 8453 >/dev/null
echo -en "test/test1\nall" | nc localhost 8453 >/dev/null
echo -en "test/test1\nall" | nc localhost 8453 >/dev/null
echo -en "test/test1\nall" | nc localhost 8453 >/dev/null

kill -INT $PID
