  $ merlin-mockup > /dev/null 2>&1 &
  $ PID=$!
  $ sleep 0.1

  $ echo -en "./defs/math\npart 4" | nc localhost 8453

  $ echo -en "./defs/math\npart 15" | nc localhost 8453

  $ kill -KILL $PID
