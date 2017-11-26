module Projects.Model exposing (..)

import Dict exposing (Dict)
import Projects.Msg exposing (Msg)
import Main.Model exposing (Model)


type alias Project =
    { id : String
    , name : String
    , description : String
    , trigger : Model -> Bool
    , cost : ProjectCost
    , effect : List Msg
    }


type alias ProjectCost =
    { funds : Int
    , operations : Int
    , creativity : Int
    , trust : Int
    }
