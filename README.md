# Splendor

This is an implementation of the
[Splendor card game](https://www.amazon.com/dp/B00IZEUFIA) mostly to
give my a chance to play around with Scala and Clojure together.

## Notes

This is pretty rough at the moment. The goal is to eventually be able to
play a game of Splendor with an AI or any number of other players. Right
now there are a couple of hardcoded users that are both ready to play
the same game.

The game engine is ready to go and decently tested. Additional tests for
the API layer are pending.

## Usage

Build the clojurescript project first:

```bash
cd client
lein cljsbuild once
```

Then the backend will serve up those files for you when running under
sbt:

```bash
cd server
sbt re-start
```

Head over to
[http://localhost:9000/ui/index.html](http://localhost:9000/ui/index.html)
to start a game.

## License

Copyright © 2016 Adam Hinz

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
