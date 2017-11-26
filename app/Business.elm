module Business
    exposing
        ( init
        , update
        , view
        , addItems
        , addFunds
        , removeFunds
        , removeItems
        , updateModel
        , sellPasteis
        )

import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Business.Msg as Business exposing (..)
import Business.Model as Business exposing (..)
import Main.Msg exposing (Msg(..))
import Main.Model exposing (Model, SaveModel)
import Utils exposing (demandPercentage)
import FormatNumber exposing (formatFloat, formatInt, usLocale)


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
    in
        ( fn businessModule, Cmd.none )


view : Model -> Html Main.Msg.Msg
view model =
    let
        businessModule =
            model.businessModule
    in
        div
            [ style
                [ ( "margin", "4px 8px" )
                , ( "width", "100%" )
                ]
            ]
            [ div
                [ style [ ( "flex-direction", "column" ) ] ]
                [ h2 [] [ text "Business" ]
                , h3 [] [ text ("Available Funds: $ " ++ (formatFloat usLocale businessModule.funds)) ]
                ]
            , div
                []
                [ text ("Unsold Inventory: " ++ (formatInt usLocale businessModule.inventory)) ]
            , div []
                [ div []
                    [ div
                        []
                        [ button
                            [ onClick (BusinessMessage Business.LowerPrice)
                            ]
                            [ text "Lower"
                            ]
                        , button
                            [ onClick (BusinessMessage Business.RaisePrice)
                            ]
                            [ text "Raise"
                            ]
                        , text
                            (" Price per Pastel: $ "
                                ++ (formatFloat usLocale businessModule.price)
                            )
                        ]
                    , div
                        []
                        [ span [] [ text ("Public demand: " ++ (demandPercentage businessModule.demand) ++ "%") ]
                        ]
                    ]
                ]
            , div [ style [ ( "margin-top", "10px" ) ] ]
                [ div []
                    [ div []
                        [ button
                            [ onClick (BusinessMessage Business.BuyAds)
                            , disabled (businessModule.funds < (toFloat businessModule.marketingCost))
                            ]
                            [ text "Marketing"
                            ]
                        , text (" Level: " ++ (toString businessModule.marketingLvl))
                        ]
                    , div
                        []
                        [ span [] [ text ("Cost: $ " ++ (formatInt usLocale businessModule.marketingCost)) ]
                        ]
                    ]
                ]
            ]


updateModel : BusinessModule -> BusinessModule
updateModel model =
    let
        marketing =
            (1.1 ^ toFloat (model.marketingLvl - 1))

        demand =
            (((0.8 / model.price) * marketing * toFloat model.marketingEffectiveness) * (toFloat model.demandBoost) * 1.1)
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
