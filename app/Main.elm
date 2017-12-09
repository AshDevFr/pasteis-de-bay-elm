port module Main exposing (..)

import Html exposing (..)
import Json.Encode
import Json.Decode as Decode
import Main.Msg as Main exposing (..)
import Main.Model exposing (..)
import Main.View as Main
import Main.Update as Main


main : Program (Maybe Decode.Value) Model Msg
main =
    programWithFlags
        { init = Main.init
        , update = Main.update savePort
        , subscriptions = Main.subscriptions cheats
        , view = Main.view
        }


port cheats : (Json.Encode.Value -> msg) -> Sub msg


port savePort : SaveModel -> Cmd msg
