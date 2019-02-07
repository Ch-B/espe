module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Polynomial as P
import Svg exposing (Svg, circle, g, line, path, rect, svg)
import Svg.Attributes as SAttr


type alias Model =
    { count : Int
    , p : P.Polynomial
    }


initialModel : Model
initialModel =
    { count = 0
    , p = P.fromCoefficients [ 1.0 / 6, -1, 1, 0 ]
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


toScreenX : Float -> Float
toScreenX x =
    500 + 50 * x


toScreenY : Float -> Float
toScreenY y =
    250 - 50 * y


forceInterval : comparable -> comparable -> comparable -> comparable
forceInterval min max x =
    if x < max && x > min then
        x

    else if x > max then
        max

    else
        min


getPathString : P.Polynomial -> String
getPathString p =
    let
        domain =
            List.map toFloat <| List.range -10 10

        values =
            List.map (P.eval p) domain

        xs =
            List.map (\x -> String.fromFloat <| toScreenX x) domain

        ys =
            List.map (\x -> String.fromFloat <| toScreenY <| forceInterval -5 5 x) values

        joinWithSpace a b =
            a ++ " " ++ b
    in
    String.append "M " <| String.join "L " <| List.map2 joinWithSpace xs ys


view : Model -> Html Msg
view model =
    let
        s =
            P.specialPoints model.p

        zeroMark x =
            circle
                [ SAttr.cx <| String.fromFloat <| toScreenX x
                , SAttr.cy <| String.fromFloat <| toScreenY <| P.eval model.p x
                , SAttr.r "5"
                , SAttr.stroke "blue"
                , SAttr.strokeWidth "2"
                , SAttr.fill "transparent"
                ]
                []

        zeroMarks =
            g
                []
                (List.map zeroMark s.zeroes)

        extremumMark x =
            circle
                [ SAttr.cx <| String.fromFloat <| toScreenX x
                , SAttr.cy <| String.fromFloat <| toScreenY <| P.eval model.p x
                , SAttr.r "5"
                , SAttr.stroke "red"
                , SAttr.strokeWidth "2"
                , SAttr.fill "transparent"
                ]
                []

        extremaMarks =
            g
                []
                (List.map extremumMark s.extrema)

        inflexionMark x =
            circle
                [ SAttr.cx <| String.fromFloat <| toScreenX x
                , SAttr.cy <| String.fromFloat <| toScreenY <| P.eval model.p x
                , SAttr.r "5"
                , SAttr.stroke "purple"
                , SAttr.strokeWidth "2"
                , SAttr.fill "transparent"
                ]
                []

        inflexionMarks =
            g
                []
                (List.map inflexionMark s.inflexions)
    in
    div []
        [ button [ onClick Increment ] [ Html.text "+1" ]
        , div [] [ Html.text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ Html.text "-1" ]
        , div [] []
        , div [] [ Html.text <| "f(x) = " ++ P.toString model.p ]
        , div [] []
        , div [] [ Html.text <| "f'(x) = " ++ (P.toString <| P.derivative model.p) ]
        , div [] []
        , div [] [ Html.text <| "f(x) = 0 at x = " ++ (String.fromFloat <| P.zero model.p -5.0) ]
        , div [] []
        , div [] [ Html.text <| "AveragePairs p " ++ String.join ", " (List.map String.fromFloat (P.averagePairs model.p)) ]
        , div [] []
        , div [] [ Html.text <| "Zeroes at x = " ++ String.join ", " (List.map String.fromFloat s.zeroes) ]
        , div [] [ Html.text <| "Extrema at x = " ++ String.join ", " (List.map String.fromFloat s.extrema) ]
        , div [] [ Html.text <| "Inflexion points at x = " ++ String.join ", " (List.map String.fromFloat s.inflexions) ]
        , div [] []
        , svg [ SAttr.width "1000", SAttr.height "500", SAttr.viewBox "0 0 1000 500" ]
            [ rect
                [ SAttr.x "1"
                , SAttr.y "1"
                , SAttr.width "998"
                , SAttr.height
                    "498"
                , SAttr.stroke "black"
                , SAttr.strokeWidth "1"
                , SAttr.fill "None"
                ]
                []
            , g [ SAttr.stroke "black", SAttr.strokeWidth "2" ]
                [ line [ SAttr.x1 "0", SAttr.x2 "1000", SAttr.y1 "250", SAttr.y2 "250" ] []
                , line [ SAttr.x1 "500", SAttr.x2 "500", SAttr.y1 "0", SAttr.y2 "500" ] []
                ]
            , g [ SAttr.id "axisLabels" ]
                [ Svg.text_ [ SAttr.x "5", SAttr.y "265" ] [ Svg.text "-10" ]
                , Svg.text_ [ SAttr.x "505", SAttr.y "265" ] [ Svg.text "0" ]
                , Svg.text_ [ SAttr.x "980", SAttr.y "265" ] [ Svg.text "10" ]
                , Svg.text_ [ SAttr.x "505", SAttr.y "15" ] [ Svg.text "5" ]
                , Svg.text_ [ SAttr.x "505", SAttr.y "495" ] [ Svg.text "-5" ]
                ]
            , path
                [ SAttr.d <| getPathString model.p
                , SAttr.stroke "blue"
                , SAttr.strokeWidth "3"
                , SAttr.fill "None"
                ]
                []
            , path
                [ SAttr.d <| getPathString <| P.derivative model.p
                , SAttr.stroke "green"
                , SAttr.strokeWidth "3"
                , SAttr.fill "None"
                ]
                []
            , zeroMarks
            , extremaMarks
            , inflexionMarks
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
