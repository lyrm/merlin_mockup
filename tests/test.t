  $ merlin-mockup > /dev/null 2>&1 &
  $ PID=$!
  $ sleep 0.1

  $ echo -en "./defs/math\nall" | nc localhost 8453
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
  Value of "rho" is 28.
  Value of "sigma" is 49.
  Value of "tau" is 98.
  Value of "upsilon" is 92.
  Value of "phi" is 30.
  Value of "chi" is 48.
  Value of "psi" is 336.
  Value of "omega" is 327.
  Value of "final_value" is 333.
  
  $ kill -KILL $PID
