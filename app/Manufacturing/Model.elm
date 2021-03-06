module Manufacturing.Model exposing (..)


type alias ManufacturingModule =
    { dough : Int
    , doughSupply : Float
    , doughCost : Int
    , doughBasePrice : Float
    , doughAutoBuy : Maybe Bool
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
