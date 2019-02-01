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
    , p = P.fromCoefficients [ 0.2, -1, -3 ]
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


evalP : P.Polynomial -> String
evalP p =
    let
        domain =
            List.map toFloat <| List.range -10 10

        values =
            List.map (P.eval p) domain

        xs =
            List.map (\x -> String.fromFloat <| x * 50 + 500) domain

        forceInterval min max x =
            if x < max && x > min then
                x

            else if x > max then
                max

            else
                min

        ys =
            List.map (\x -> String.fromFloat <| -(forceInterval -5 5 x) * 50 + 250) values

        joinWithSpace a b =
            a ++ " " ++ b
    in
    String.append "M " <| String.join "L " <| List.map2 joinWithSpace xs ys


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ Html.text "+1" ]
        , div [] [ Html.text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ Html.text "-1" ]
        , div [] []
        , div [] [ Html.text <| "f(x) = " ++ P.toString model.p ]
        , div [] []
        , div [] [ Html.text <| "f'(x) = " ++ (P.toString <| P.derivative model.p) ]
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
            , Svg.text_ [ SAttr.x "5", SAttr.y "265" ] [ Svg.text "-10" ]
            , Svg.text_ [ SAttr.x "505", SAttr.y "265" ] [ Svg.text "0" ]
            , Svg.text_ [ SAttr.x "980", SAttr.y "265" ] [ Svg.text "10" ]
            , Svg.text_ [ SAttr.x "505", SAttr.y "15" ] [ Svg.text "5" ]
            , Svg.text_ [ SAttr.x "505", SAttr.y "495" ] [ Svg.text "-5" ]
            , path
                [ SAttr.d <| evalP model.p
                , SAttr.stroke "blue"
                , SAttr.strokeWidth "3"
                , SAttr.fill "None"
                ]
                []
            , path
                [ SAttr.d <| evalP <| P.derivative model.p
                , SAttr.stroke "green"
                , SAttr.strokeWidth "3"
                , SAttr.fill "None"
                ]
                []
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
