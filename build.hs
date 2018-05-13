-- stack runhaskell --package turtle
-- -- on *nix this has to be the first line #!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
import Turtle ((&), echo, empty, inproc, stdout)

main = do
  empty
    & inproc "stack" ["build"] --, "-v"]
    & inproc "client/private/elm" ["make", "client/src/Main.elm", "--optimize", "--output=assets/js/app-dev.js"]
    & stdout
  echo "Build successful, now run `stack exec elmanac-exe` to start the backend."
