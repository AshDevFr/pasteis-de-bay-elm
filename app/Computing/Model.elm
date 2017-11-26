module Computing.Model exposing (..)


type alias ComputingModule =
    { trust : Int
    , processors : Int
    , memory : Int
    , memoryLimit : Int
    , operations : Float
    , creativity : Maybe Float
    }
