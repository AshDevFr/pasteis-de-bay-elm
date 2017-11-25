module Utils exposing (..)

import Random
import FormatNumber exposing (formatFloat, usLocale)
import Models exposing (..)


demandPercentage : Float -> String
demandPercentage demand =
    formatFloat usLocale (demand * 10)


randomFloat : Float -> Float -> Random.Seed -> ( Float, Random.Seed )
randomFloat float float2 seed =
    Random.step (Random.float float float2) seed


randomInt : Int -> Int -> Random.Seed -> ( Int, Random.Seed )
randomInt int int2 seed =
    Random.step (Random.int int int2) seed


randomMultipleFloat : Float -> Float -> Int -> Random.Seed -> ( List Float, Random.Seed )
randomMultipleFloat float float2 length seed =
    Random.step (Random.list length (Random.float float float2)) seed


randomMultipleInt : Int -> Int -> Int -> Random.Seed -> ( List Int, Random.Seed )
randomMultipleInt int int2 length seed =
    Random.step (Random.list length (Random.int int int2)) seed
