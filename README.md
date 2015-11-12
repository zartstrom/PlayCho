# Baduk Board  written in Scala.js

inspired by jgoboard

## Get started

## Run the tests

## The fully optimized version

For ultimate code size reduction, use `fullOptJS`. This will take several
seconds to execute, so typically you only use this for the final, production
version of your application. While `index-fastopt.html` refers to the
JavaScript emitted by `fastOptJS`, `index.html` refers to the optimized
JavaScript emitted by `fullOptJS`.

If Node.js is installed, the tests can also be run in their fully optimized
version with:

    > set scalaJSStage in Global := FullOptStage
    > test
