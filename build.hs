-- stack runhaskell --package turtle
-- -- on *nix this has to be the first line #!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
import Turtle ((&), echo, empty, inproc, stdout)

main = do
  empty
    & inproc "stack" ["build"]
    & inproc "elm-make" ["client/src/Main.elm", "--output=assets/app-dev.js"]
    & stdout
  echo "Build successful, now run `stack exec elmanach-exe` to start the backend."
