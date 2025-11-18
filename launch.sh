./_build/default/server/server.exe $1 &
PID=$!

sleep 1

echo -n 1 | nc -U $1 >/dev/null
echo -n 17 | nc -U $1 >/dev/null
echo -n 23 | nc -U $1 >/dev/null

kill -INT $PID
