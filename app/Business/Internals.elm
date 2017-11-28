module Business.Internals
    exposing
        ( addItems
        , addFunds
        , removeFunds
        , removeItems
        , updateDemand
        , sellPasteis
        , buyAds
        , lowerPrice
        , raisePrice
        )

import Business.Model exposing (BusinessModule)


updateDemand : BusinessModule -> BusinessModule
updateDemand model =
    let
        marketing =
            (1.1 ^ toFloat (model.marketingLvl - 1))

        demand =
            (((0.8 / model.price) * marketing * model.marketingEffectiveness) * model.demandBoost * 1.1)
    in
        { model
            | demand = demand
        }


sellPasteis : BusinessModule -> Float -> BusinessModule
sellPasteis model rand =
    let
        demand =
            floor (0.7 * (model.demand ^ 1.15))
    in
        if (rand > model.demand || model.inventory == 0) then
            model
        else if (demand > model.inventory) then
            { model
                | inventory = 0
                , funds = model.funds + (model.price * (toFloat model.inventory))
            }
        else
            { model
                | inventory = model.inventory - demand
                , funds = model.funds + (model.price * (toFloat demand))
            }


addFunds : BusinessModule -> Float -> BusinessModule
addFunds model income =
    { model
        | funds = model.funds + income
    }


removeFunds : BusinessModule -> Float -> BusinessModule
removeFunds model funds =
    { model
        | funds = model.funds - funds
    }


addItems : BusinessModule -> Int -> BusinessModule
addItems model income =
    { model
        | inventory = model.inventory + income
    }


removeItems : BusinessModule -> Int -> BusinessModule
removeItems model outcome =
    { model
        | inventory = model.inventory - outcome
    }


buyAds : BusinessModule -> BusinessModule
buyAds model =
    if (model.funds < toFloat model.marketingCost) then
        model
    else
        { model
            | funds = model.funds - toFloat model.marketingCost
            , marketingCost = model.marketingCost * 2
            , marketingLvl = model.marketingLvl + 1
        }


lowerPrice : BusinessModule -> BusinessModule
lowerPrice model =
    { model
        | price = (Basics.max (model.price - 0.01) 0.01)
    }


raisePrice : BusinessModule -> BusinessModule
raisePrice model =
    { model
        | price = model.price + 0.01
    }
