module Tests exposing (..)

import Test exposing (Test, describe)
import Units.UtilsTest exposing (utilsTests)


all : Test
all =
    utilsTests
        |> describe "Test Suite"
