module Business.Update
    exposing
        ( init
        , update
        )

import Business.Msg as Business exposing (..)
import Business.Model as Business exposing (BusinessModule)
import Business.Internals exposing (lowerPrice, raisePrice, buyAds, removeFunds, updateDemand, sellPasteis, addItems)


init : BusinessModule
init =
    { funds = 0
    , inventory = 0
    , price = 0.25
    , demand = 3
    , demandBoost = 1
    , marketingCost = 100
    , marketingLvl = 1
    , marketingEffectiveness = 1
    }


update : Business.Msg -> BusinessModule -> ( BusinessModule, Cmd msg )
update msg businessModule =
    let
        fn =
            case msg of
                LowerPrice ->
                    lowerPrice

                RaisePrice ->
                    raisePrice

                BuyAds ->
                    buyAds

                RemoveFunds funds ->
                    flip removeFunds funds

                UpdateDemand ->
                    updateDemand

                SellPasteis cnt ->
                    flip sellPasteis cnt

                AddItems cnt ->
                    flip addItems cnt
    in
        ( fn businessModule, Cmd.none )
