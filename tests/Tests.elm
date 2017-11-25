module Tests exposing (..)

import Test exposing (Test, describe)
import Unit.UtilsTest exposing (utilsTests)


all : Test
all =
    utilsTests
        |> describe "Test Suite"
