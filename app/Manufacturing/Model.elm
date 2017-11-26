module Manufacturing.Model exposing (..)


type alias ManufacturingModule =
    { dough : Int
    , doughSupply : Int
    , doughCost : Int
    , doughBasePrice : Float
    , pasteisModule : Maybe PasteisModule
    , megaPasteisModule : Maybe MegaPasteisModule
    , partialPasteis : Float
    , pasteisMakerRate : Float
    }


type alias PasteisModule =
    { cost : Float
    , boost : Float
    , level : Int
    , factor : Int
    }


type alias MegaPasteisModule =
    { cost : Float
    , boost : Float
    , level : Int
    , factor : Int
    }
