  $ merlin-mockup &
$ merlin-mockup > /dev/null 2>&1 &
  $ PID=$!
  $ sleep 0.1

  $ printf "./defs/math\npart 4" | nc localhost 8453
  Value of "alpha" is 4.
  Value of "beta" is 3.
  Value of "gamma" is 7.
  Value of "delta" is 21.
  Value of "epsilon" is 6.
  
  $ printf "./defs/fruits\nall" | nc localhost 8453
  Value of "apple" is 13.
  Value of "banana" is 12.
  Value of "cherry" is 45.
  Value of "dragonfruit" is 21.
  Value of "elderberry" is 116.
  Value of "fig" is 55.
  Value of "grape" is 620.
  Value of "hazelnut" is 316.
  Value of "iceberg" is 642.
  Value of "jackfruit" is 332.
  Value of "kiwi" is 115.
  Value of "lime" is 448.
  Value of "mango" is 228.
  Value of "nectarine" is 671.
  Value of "orange" is 363.
  Value of "papaya" is 832.
  Value of "quince" is 426.
  Value of "raspberry" is 1184.
  Value of "strawberry" is 3516.
  Value of "tangerine" is 1765.
  Value of "ugli" is 5290.
  Value of "vanilla" is 2873.
  Value of "watermelon" is 8504.
  Value of "xigua" is 4279.
  Value of "yellowfruit" is 12721.
  
  $ printf '.\nclose' | nc localhost 8453
