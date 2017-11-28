module Projects.Model exposing (..)

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
    { funds : Float
    , operations : Float
    , creativity : Float
    , trust : Int
    }
