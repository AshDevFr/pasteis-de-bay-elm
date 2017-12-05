module Business.Model exposing (BusinessModule, StatsModule)


type alias BusinessModule =
    { funds : Float
    , inventory : Int
    , price : Float
    , demand : Float
    , demandBoost : Float
    , marketingCost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Float
    , statsModule : Maybe StatsModule
    }


type alias StatsModule =
    { revPerSec : Float
    , salesPerSec : Float
    , lastSales : List ( Int, Float )
    }
