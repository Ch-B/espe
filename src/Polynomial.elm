module Polynomial exposing (Polynomial, derivative, eval, fromCoefficients, toString)

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
