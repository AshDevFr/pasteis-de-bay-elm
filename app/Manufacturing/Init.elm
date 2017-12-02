module Manufacturing.Init
    exposing
        ( init
        )

import Manufacturing.Model exposing (ManufacturingModule)


init : ManufacturingModule
init =
    { dough = 1000
    , doughSupply = 1000
    , doughCost = 15
    , doughBasePrice = 15
    , doughAutoBuy = Nothing
    , pasteisModule = Nothing
    , megaPasteisModule = Nothing
    , partialPasteis = 0
    , pasteisMakerRate = 0
    }
