  $ DELATOR_LOG=trace DELATOR_TEST_CLOCK=1 ./lowering_demo.exe 2>&1 | sed -E 's/span#[0-9]+/span#ID/g'
  DEBUG Dune__exe__Lowering_demo::lower unit_name=Demo
    DEBUG Dune__exe__Lowering_demo: select pass pass=simplify
    DEBUG Dune__exe__Lowering_demo::leaf node=7
      INFO Dune__exe__Lowering_demo: rewrite node node=7
    span#ID close duration_ns=1000000
    WARN Dune__exe__Lowering_demo: fallback unit_name=Demo
  span#ID close duration_ns=3000000

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_TEST_CLOCK=1 ./lowering_demo.exe 2>&1 | sed -E 's/close=[0-9]+/close=ID/g'
  DEBUG Dune__exe__Lowering_demo span.new=lower unit_name=Demo
  DEBUG Dune__exe__Lowering_demo [lower]: select pass pass=simplify
  DEBUG Dune__exe__Lowering_demo span.new=leaf node=7
  INFO Dune__exe__Lowering_demo [lower>leaf]: rewrite node node=7
  SPAN lower>leaf close=ID duration_ns=1000000
  WARN Dune__exe__Lowering_demo [lower]: fallback unit_name=Demo
  SPAN lower close=ID duration_ns=3000000

  $ DELATOR_LOG=trace DELATOR_TEST_CLOCK=1 ./exception_demo.exe 2>&1 | grep -c 'uncaught exception'
  1

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_TEST_CLOCK=1 ./multidomain_demo.exe > multi.out 2>&1
  $ test "$(grep -c ': event ' multi.out)" = 100
  $ test "$(grep -c 'span.new=domain' multi.out)" = 2
  $ test "$(grep -c '^SPAN ' multi.out)" = 2
  $ test "$(grep -c 'domain-exit-tail' multi.out)" = 1
  $ test "$(grep -vcE '^(INFO|SPAN) ' multi.out)" = 0
  $ rm multi.out

  $ DELATOR_LOG=trace DELATOR_FORMAT=flat DELATOR_TEST_CLOCK=1 ./instrument_features.exe > features.out 2>&1
  $ grep -c 'span.new=' features.out
  6
  $ grep 'span.new=even' features.out | head -1
  DEBUG Dune__exe__Instrument_features span.new=even value=n2
  $ grep 'span.new=odd' features.out | head -1
  DEBUG Dune__exe__Instrument_features span.new=odd
  $ grep 'span.new=labelled' features.out
  DEBUG Dune__exe__Instrument_features span.new=labelled left=<opaque> right=<opaque> value=<opaque>
  $ grep 'span.new=classify' features.out
  DEBUG Dune__exe__Instrument_features span.new=classify
  $ grep 'span.new=identity' features.out
  DEBUG Dune__exe__Instrument_features span.new=identity
  $ rm features.out

  $ ./numeric_renderer.exe 2>&1
  span#0 close duration_ns=0
  span#4611686018427387903 close duration_ns=9223372036854775807
  span#-4611686018427387904 close duration_ns=-9223372036854775808

  $ DELATOR_LOG=trace DELATOR_COLOR=always DELATOR_TEST_CLOCK=1 ./lowering_demo.exe > color.out 2>&1
  $ grep -Fq "$(printf '\033[36mDEBUG\033[0m')" color.out
  $ grep -Fq "$(printf '\033[32mINFO\033[0m')" color.out
  $ grep -Fq "$(printf '\033[33mWARN\033[0m')" color.out
  $ grep -Fq "$(printf '\033[35mspan#\033[0m')" color.out
  $ rm color.out

  $ DELATOR_LOG=trace DELATOR_COLOR=never DELATOR_TEST_CLOCK=1 ./lowering_demo.exe > no-color.out 2>&1
  $ test "$(LC_ALL=C tr -cd '\033' < no-color.out | wc -c)" = 0
  $ rm no-color.out
