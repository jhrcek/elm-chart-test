module Main exposing (..)

import Chart exposing (vBar, title, updateStyles, toHtml)
import Html exposing (Html, div, text, button, input, label, h1)
import Html.Attributes as HA exposing (type', value, disabled)
import Html.Events exposing (onClick, onInput)
import Html.App exposing (program)
import List exposing (..)
import Markdown
import Platform.Sub as Sub
import Result exposing (withDefault)
import String exposing (toInt)
import Time exposing (millisecond)


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }



-- MODEL


type alias Model =
    { coins : Int
    , animationRunning : Bool
    }


maxCoins : Int
maxCoins =
    1000


coinsToAddOnAnimationTick : Int
coinsToAddOnAnimationTick =
    10


init : ( Model, Cmd Msg )
init =
    ( Model 0 False, Cmd.none )



-- UPDATE


type Msg
    = StartAnimation
    | StopAnimation
    | AddCoins Int
    | SetCoins Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg { coins, animationRunning } =
    let
        clampCoins =
            clamp 0 maxCoins

        continueAnimation newCoins =
            if newCoins == maxCoins then
                False
            else
                animationRunning
    in
        case msg of
            SetCoins c ->
                let
                    newCoins =
                        clampCoins c
                in
                    Model newCoins (continueAnimation newCoins)

            AddCoins c ->
                let
                    newCoins =
                        clampCoins (coins + c)
                in
                    Model newCoins (continueAnimation newCoins)

            StopAnimation ->
                Model coins False

            StartAnimation ->
                Model coins True



-- SUBS


subs : Model -> Sub Msg
subs { animationRunning } =
    if animationRunning then
        Time.every (100 * millisecond) (\_ -> AddCoins coinsToAddOnAnimationTick)
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ description
        , renderBarChart model.coins
        , controls model
        ]


controls : Model -> Html Msg
controls { coins, animationRunning } =
    div []
        [ div []
            [ label [] [ text "Animation " ]
            , button [ onClick StartAnimation, disabled animationRunning ] [ text "Start" ]
            , button [ onClick StopAnimation, disabled (not animationRunning) ] [ text "Stop" ]
            , button [ onClick (SetCoins 0), disabled animationRunning ] [ text "Restart" ]
            ]
        , div []
            [ label [] [ text "Manual controls " ]
            , button [ onClick (AddCoins (-10)) ] [ text "-10 coins" ]
            , button [ onClick (AddCoins (-1)) ] [ text "-1 coin" ]
            , input
                [ onInput (SetCoins << withDefault 0 << toInt)
                , type' "number"
                , value (toString coins)
                , HA.min "1"
                , HA.max (toString maxCoins)
                , disabled animationRunning
                ]
                []
            , button [ onClick (AddCoins 1) ] [ text "+1 coin" ]
            , button [ onClick (AddCoins 10) ] [ text "+10 coins" ]
            ]
        ]


description : Html Msg
description =
    Markdown.toHtml [] """
 # From Binomial to Normal distribution

Did you know that for sufficiently large n
[Binomial distribution](https://en.wikipedia.org/wiki/Binomial_distribution) `B(n,p)`
converges [Normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)?
This interactive animation might give you some intuitive feel for why this is true.

The chart below shows binomial distribution `B(n, p = 0.5)` of possible results of flipping fair coin `n` times.
The x-axis shows number of coins that come up heads and the y-axis shows total number of possible
outcomes that can end up with given number of coins. Notice that in the end the shape of the distribution resembles
the familiar Bell curve, which is typical for Normal distribution.
"""


renderBarChart : Int -> Html Msg
renderBarChart coins =
    let
        distribution =
            pascalRow coins

        xyData =
            map (\barHeight -> ( barHeight, "" {- no x-axis labels for bars -} )) distribution
    in
        vBar xyData
            |> title ("Number of coins flipped: " ++ toString coins)
            |> updateStyles "legend" [ ( "height", "0" ) ]
            |> updateStyles "chart-elements" [ ( "min-width", "4px" ), ( "width", "4px" ), ( "padding", "0px" ) ]
            |> toHtml


pascalRow : Int -> List Float
pascalRow n =
    scanl (\x a -> (a * (toFloat n - x + 1)) / x) 1 [1..toFloat n]
