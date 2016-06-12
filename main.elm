import Chart exposing (..)
import Html exposing (Html, div, text, button, input)
import Html.Attributes as HA exposing (type', value, disabled)
import Html.Events exposing (onClick)
import Html.App exposing (program)
import List exposing (..)
import Time exposing (millisecond)
import Platform.Sub as Sub

main : Program Never
main = program { init = init, view = view, update = update, subscriptions = subs }

-- MODEL

type alias Model = Int

init : (Model, Cmd Msg)
init = (0, Cmd.none)

-- UPDATE

type Msg
  = Tick
  | Restart

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> (model + 10, Cmd.none)
    Restart -> (0, Cmd.none)

-- SUBS

subs : Model -> Sub Msg
subs m = if m < 1000
  then Time.every (100 * millisecond) (\_ -> Tick)
  else Sub.none


view : Model -> Html Msg
view model = div []
    [ toBarChart <| pascalRow model
    , controls model
    , description
    ]


controls : Model -> Html Msg
controls model = div []
  [ input [type' "number", value (toString model), HA.min "1", HA.max "1000", disabled True] []
  , button [onClick Restart] [text "restart"]
  ]

description : Html Msg
description = div [] [text "How it works: "]

toBarChart : List Float -> Html Msg
toBarChart xs =
  let n = length xs - 1
  in
    vBar xs (List.repeat n "")
      |> title ("Number of coins flipped: " ++ toString n)
      |> updateStyles "legend" [("height", "0")]
      |> toHtml

pascalRow : Int -> List Float
pascalRow n = scanl (\x a -> (a * (toFloat n - x + 1)) / x) 1 (map toFloat [1..n])
