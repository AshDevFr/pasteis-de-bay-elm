module Projects.Msg
    exposing
        ( Msg(..)
        )


type Msg
    = MapOperations (Float -> Float)
    | MapPasteisBoost (Float -> Float)
    | MapMegaPasteisBoost (Float -> Float)
    | MapTrust (Float -> Float)
    | MapWireSupply (Float -> Float)
    | MapMarketingEffectiveness (Float -> Float)
    | MapDemandBoost (Float -> Float)
    | Identity
