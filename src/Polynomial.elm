module Polynomial exposing (Polynomial, averagePairs, derivative, eval, fromCoefficients, specialPoints, toString, zero)

import Array exposing (Array)


epsilon =
    0.0000000001


type alias Polynomial =
    List Float


fromCoefficients : List Float -> Polynomial
fromCoefficients list =
    List.reverse list


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


newton : Polynomial -> Int -> Float -> Float
newton p depth guess =
    if depth <= 0 then
        0 / 0

    else if abs (eval p guess) <= epsilon then
        guess

    else
        let
            newguess =
                guess - eval p guess / eval (derivative p) guess
        in
        newton p (depth - 1) newguess


zero : Polynomial -> Float -> Float
zero p guess =
    newton p 1000 guess


averagePairs : List Float -> List Float
averagePairs list =
    case list of
        [] ->
            []

        only :: [] ->
            []

        a :: b :: rest ->
            0.5 * (a + b) :: averagePairs (b :: rest)


type alias SpecialPoints =
    { zeroes : List Float, extrema : List Float, inflexions : List Float }


specialPoints : Polynomial -> SpecialPoints
specialPoints p =
    let
        findBetween : List Float -> List Float
        findBetween list =
            List.map (zero p) (averagePairs list)
    in
    case p of
        [] ->
            { zeroes = [], extrema = [], inflexions = [] }

        [ a ] ->
            { zeroes = [], extrema = [], inflexions = [] }

        [ a, b ] ->
            { zeroes = [ -a / b ], extrema = [], inflexions = [] }

        _ ->
            let
                s =
                    specialPoints (derivative p)
            in
            { zeroes = findBetween ([ -1.0e10 ] ++ s.zeroes ++ [ 1.0e10 ]), extrema = s.zeroes, inflexions = s.extrema }
