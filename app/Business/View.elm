module Business.View
    exposing
        ( view
        )

import Main.Msg as Main exposing (Msg(..))
import Main.Model as Main exposing (Model)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Business.Msg as Business
import Utils exposing (demandPercentage)


view : Model -> Html Main.Msg
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
            , statsView model
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


statsView : Model -> Html Main.Msg
statsView model =
    model.businessModule.statsModule
        |> Maybe.map
            (\stats ->
                div []
                    [ div
                        []
                        [ text ("Avg. Rev. per sec: $ " ++ (formatFloat usLocale stats.revPerSec)) ]
                    , div
                        []
                        [ text ("Avg. Pasteis Sold per sec: " ++ (formatFloat usLocale stats.salesPerSec)) ]
                    ]
            )
        |> Maybe.withDefault (text "")
