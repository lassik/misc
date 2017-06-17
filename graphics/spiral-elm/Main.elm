-- Draw a rainbow spiral made out of circles.
--
-- http://www.mathematische-basteleien.de/spiral.htm
-- https://krazydad.com/tutorials/makecolors.php


module Main exposing (..)

import Html exposing (Html)
import List
import String
import Svg exposing (circle, svg)
import Svg.Attributes exposing (..)


candyFactor =
    100


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


nCircles i n ocx ocy =
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
        circle [ cx (toString tcx), cy (toString tcy), r "45", fill (rainbow (i / n)), stroke "black" ] []
            :: nCircles (i + 1) n ocx ocy


main =
    svg [ viewBox "00 00 300 300", width "600px" ]
        (nCircles 0 300 150 150)


rgb r g b =
    let
        nums =
            List.map (floor >> toString) [ r, g, b ]
    in
    "rgb(" ++ String.join "," nums ++ ")"
