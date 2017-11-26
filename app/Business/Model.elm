module Business.Model exposing (BusinessModule)


type alias BusinessModule =
    { funds : Float
    , inventory : Int
    , price : Float
    , demand : Float
    , demandBoost : Int
    , marketingCost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    }
