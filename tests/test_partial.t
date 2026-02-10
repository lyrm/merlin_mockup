  $ merlin-mockup > /dev/null 2>&1 &
  $ PID=$!
  $ sleep 0.1

  $ printf "./defs/math\npart 4" | nc localhost 8453
  Value of "alpha" is 4.
  Value of "beta" is 3.
  Value of "gamma" is 7.
  Value of "delta" is 21.
  Value of "epsilon" is 6.
  

  $ printf "./defs/math\npart 15" | nc localhost 8453
  Value of "alpha" is 4.
  Value of "beta" is 3.
  Value of "gamma" is 7.
  Value of "delta" is 21.
  Value of "epsilon" is 6.
  Value of "zeta" is 18.
  Value of "eta" is 12.
  Value of "theta" is 19.
  Value of "iota" is 9.
  Value of "kappa" is 23.
  Value of "lambda" is -250.
  Value of "mu" is 32.
  Value of "nu" is -750.
  Value of "xi" is -756.
  Value of "omicron" is 96.
  Value of "pi" is 115.
  

  $ printf '.\nclose' | nc localhost 8453
