module Polynomial exposing (Polynomial, derivative, eval, fromCoefficients, toString, zero)

import Array exposing (Array)


type alias Polynomial =
    List Float


fromCoefficients : List Float -> Polynomial
fromCoefficients list =
    List.reverse list


horner : Float -> Float -> Float -> Float
horner x a acc =
    acc * x + a


eval : Polynomial -> Float -> Float
eval p x =
    List.foldr (horner x) 0 p


derivative : Polynomial -> Polynomial
derivative p =
    let
        multWith =
            List.map toFloat <| List.range 1 <| List.length p

        newCoeff =
            List.drop 1 p
    in
    List.map2 (*) newCoeff multWith


zero : Polynomial -> Float -> Float
zero p guess =
    newton p 1000 guess


newton : Polynomial -> Int -> Float -> Float
newton p depth guess =
    if depth <= 0 then
        0 / 0

    else if abs (eval p guess) <= 0.0000000001 then
        guess

    else
        let
            newguess =
                guess - eval p guess / eval (derivative p) guess
        in
        newton p (depth - 1) newguess


toString : Polynomial -> String
toString p =
    let
        coefficientList =
            List.map (\x -> "(" ++ String.fromFloat x ++ ")") p

        powersOfX =
            List.reverse <| List.map String.fromInt <| List.range 1 <| List.length p - 1

        powersOfXWithX =
            List.map (\x -> String.append "x^" x) powersOfX

        powersOfXWithConstTerm =
            powersOfXWithX ++ [ "" ]

        terms =
            List.map2 (++) (List.reverse coefficientList) powersOfXWithConstTerm
    in
    String.join "+" terms
