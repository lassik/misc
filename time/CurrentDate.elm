module Main exposing (Msg(..), init, main, monthNumber, subscriptions, update, view)

import Browser
import Debug
import Html
import Task
import Time exposing (..)


type Msg
    = NewTime Time.Posix


init () =
    ( Time.millisToPosix 0
    , Task.perform NewTime Time.now
    )


monthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


view model =
    let
        hour =
            String.fromInt (Time.toYear Time.utc model)

        minute =
            String.fromInt (monthNumber (Time.toMonth Time.utc model))

        second =
            String.fromInt (Time.toDay Time.utc model)
    in
    Html.text (hour ++ "-" ++ minute ++ "-" ++ second)


update msg model =
    case msg of
        NewTime t ->
            ( t, Cmd.none )


subscriptions number =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
