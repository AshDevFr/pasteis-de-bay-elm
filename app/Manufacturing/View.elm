module Manufacturing.View
    exposing
        ( view
        )

import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Main.Msg as Main exposing (Msg(..))
import Main.Model as Main exposing (SaveModel, Model)
import Manufacturing.Msg as Manufacturing exposing (..)


view : Model -> Html Main.Msg
view model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        div
            [ style
                [ ( "margin", "4px 8px" )
                , ( "width", "100%" )
                ]
            ]
            [ div
                [ style [ ( "flex-direction", "column" ) ] ]
                [ h2 [] [ text "Manufacturing" ]
                , h3 [] [ text ("Pasteis per Second: " ++ (manufacturingModule.pasteisMakerRate |> round |> toString)) ]
                ]
            , div
                []
                [ div []
                    [ div []
                        [ button
                            [ onClick (ManufacturingMessage (Manufacturing.BuyDough businessModule.funds))
                            , disabled (businessModule.funds < (toFloat manufacturingModule.doughCost))
                            ]
                            [ text "Dough"
                            ]
                        , text (" " ++ (formatInt usLocale manufacturingModule.dough) ++ " coins")
                        ]
                    , div
                        []
                        [ text ("Cost: $ " ++ (toString manufacturingModule.doughCost))
                        ]
                    ]
                ]
            , pasteisView model
            , megaPasteisView model
            ]


pasteisView : Model -> Html Main.Msg
pasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        manufacturingModule.pasteisModule
            |> Maybe.map
                (\mod ->
                    div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ div []
                                [ button
                                    [ onClick BuyPasteis
                                    , disabled (businessModule.funds < mod.cost)
                                    ]
                                    [ text "AutoPasteis"
                                    ]
                                , text (" " ++ (toString mod.level))
                                ]
                            , div
                                []
                                [ span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                                ]
                            ]
                        ]
                )
            |> Maybe.withDefault (div [] [ text "" ])


megaPasteisView : Model -> Html Main.Msg
megaPasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        manufacturingModule.megaPasteisModule
            |> Maybe.map
                (\mod ->
                    div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ div []
                                [ button
                                    [ onClick BuyMegaPasteis
                                    , disabled (businessModule.funds < mod.cost)
                                    ]
                                    [ text "MegaPasteis"
                                    ]
                                , text (" " ++ (toString mod.level))
                                ]
                            , div
                                []
                                [ span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                                ]
                            ]
                        ]
                )
            |> Maybe.withDefault (div [] [ text "" ])
