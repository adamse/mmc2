An AI agent for http://www.monkeymusicchallenge.com/ built in Haskell.

# To run

~~~sh
# Clone stuff
$ git clone ...

# Use a sandbox
$ cabal sandbox init

# Install dependencies
$ cabal install --only-dependencies 

# Build
$ cabal build

# Play
$ cabal repl
# Or run
$ cabal run mmc2 TEAMNAME APIKEY GAMEID
~~~
