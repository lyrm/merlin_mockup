#!/bin/bash

# Find the project root by locating dune-project
PROJECT_ROOT="$(cd "$(dirname "$0")" && while [ ! -f dune-project ] && [ "$PWD" != "/" ]; do cd ..; done && pwd)"

# Build first
dune build --root "$PROJECT_ROOT"

# Path to the built binary
BINARY="$PROJECT_ROOT/_build/default/src/merlin_mockup.exe"

# Start the server in background (logs go to server.log)
"$BINARY" > server.log 2>&1 &
PID=$!

# Cleanup function
cleanup() {
    kill -INT $PID 2>/dev/null
    sleep 0.1
    kill -9 $PID 2>/dev/null  # Force kill if still running
}

# Ensure cleanup on exit, interrupt, or termination
trap cleanup EXIT INT TERM

# Wait for server to start
sleep 0.2

# Send requests and print output
# Measure time to detect if we hit the timeout
# START=$(date +%s)
printf './tests/defs/math\npart 1' | nc localhost 8453
printf './tests/defs/math\npart 3' | nc localhost 8453
printf './tests/defs/math\npart 12' | nc localhost 8453
printf './tests/defs/math\npart 20' | nc localhost 8453
printf './tests/defs/math\npart 10' | nc localhost 8453
printf '.\nclose' | nc localhost 8453
# END=$(date +%s)
# ELAPSED=$((END - START))
# if [ $ELAPSED -ge 5 ]; then
#     echo "Warning: Connection closed by timeout (server did not close connection)"
# fi

# Cleanup happens automatically via trap
#  kill -KILL $PID