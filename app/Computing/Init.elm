module Computing.Init
    exposing
        ( init
        )

import Computing.Model exposing (ComputingModule)


init : ComputingModule
init =
    { trust = 2
    , trustLevel = 2
    , processors = 1
    , memory = 1
    , memoryLimit = 1000
    , operations = 0
    , creativity = Nothing
    }
