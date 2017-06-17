-- Draw a rainbow spiral made out of circles.
--
-- http://www.mathematische-basteleien.de/spiral.htm
-- https://krazydad.com/tutorials/makecolors.php


module Main exposing (..)

import Html exposing (Html, program)
import List
import String
import Svg exposing (circle, svg)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


candyFactor =
    200


rainbow t =
    let
        bias b =
            sin (candyFactor * t + b) * 127 + 128
    in
    rgb (bias -2) (bias 0) (bias 2)


xSpiral a t =
    a * t * cos t


ySpiral a t =
    a * t * sin t


nCircles i n ocx ocy time =
    if i >= n then
        []
    else
        let
            a =
                5

            t =
                i / (2 * pi)

            tcx =
                ocx + xSpiral a t

            tcy =
                ocy + ySpiral a t
        in
        circle
            [ cx (toString tcx)
            , cy (toString tcy)
            , r "45"
            , fill (rainbow (time + (i / n)))
            , stroke "black"
            ]
            []
            :: nCircles (i + 1) n ocx ocy time


view time =
    svg [ viewBox "00 00 300 300", width "600px" ]
        (nCircles 0 300 150 150 (time / (35 * Time.second)))


rgb r g b =
    let
        nums =
            List.map (floor >> toString) [ r, g, b ]
    in
    "rgb(" ++ String.join "," nums ++ ")"


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type alias Model =
    Time


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( newTime, Cmd.none )


subs : Model -> Sub Msg
subs model =
    Time.every (45 * Time.millisecond) Tick


main =
    program { init = init, view = view, update = update, subscriptions = subs }
