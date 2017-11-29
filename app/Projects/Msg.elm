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
    | MapWireSupply (Float -> Float)
    | MapMarketingEffectiveness (Float -> Float)
    | MapDemandBoost (Float -> Float)
    | MapFunds (Float -> Float)
    | MapDough (Int -> Int)
    | MapEnableCreativity
    | NoEffect
    | Identity
