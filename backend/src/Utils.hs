module Utils ((|>), (<|)) where

-- | Shamelessly plucked from https://github.com/elm-lang/elm-compiler/blob/master/src/Elm/Utils.hs

{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 0 <|
infixl 0 |>
