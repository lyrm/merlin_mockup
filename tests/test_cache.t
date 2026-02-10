  $ merlin-mockup > /dev/null 2>&1 &
  $ PID=$!
  $ sleep 0.1

  $ echo -en "./defs/math\npart 1" | nc localhost 8453
  $ echo -en "./defs/math\npart 3" | nc localhost 8453
  $ echo -en "./defs/math\npart 12" | nc localhost 8453
  $ echo -en "./defs/math\npart 20" | nc localhost 8453
  $ echo -en "./defs/math\npart 10" | nc localhost 8453

  $ kill -KILL $PID
