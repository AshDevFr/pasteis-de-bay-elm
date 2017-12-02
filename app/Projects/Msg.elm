module Projects.Msg
    exposing
        ( Msg(..)
        )


type Msg
    = MapOperations (Float -> Float)
    | MapCreativity (Float -> Float)
    | MapPasteisBoost (Float -> Float)
    | MapMegaPasteisBoost (Float -> Float)
    | MapTrust (Int -> Int)
    | MapDoughSupply (Float -> Float)
    | MapMarketingEffectiveness (Float -> Float)
    | MapDemandBoost (Float -> Float)
    | MapFunds (Float -> Float)
    | MapDough (Int -> Int)
    | MapDoughAutoBuy
    | MapEnableMegaPasteis
    | MapEnableCreativity
    | NoEffect
    | Identity
