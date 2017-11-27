module Projects.Msg
    exposing
        ( Msg(..)
        )


type Msg
    = MapOperations (Float -> Float)
    | MapPasteisBoost (Float -> Float)
    | MapPasteisLevel (Float -> Float)
    | MapTrust (Float -> Float)
    | MapWire (Float -> Float)
    | Identity
