module Business.Msg
    exposing
        ( Msg(..)
        )


type Msg
    = LowerPrice
    | RaisePrice
    | BuyAds
    | RemoveFunds Float
    | UpdateDemand
    | SellPasteis Float
    | AddItems Int
