module Polynomial exposing (Polynomial, eval)


type alias Polynomial =
    List Float


example : Polynomial
example =
    [ 1, 2, 1 ]


horner : Float -> Float -> Float -> Float
horner x a acc =
    acc * x + a


eval : Polynomial -> Float -> Float
eval p x =
    List.foldl (horner x) 0 p
