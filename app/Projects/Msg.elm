module Projects.Msg
    exposing
        ( Msg(..)
        )


type Msg
    = MapOperations (Float -> Float)
    | MapPasteisBoost (Float -> Float)
    | MapPasteisLevel (Int -> Int)
    | MapTrust (Int -> Int)
    | MapWire (Int -> Int)
