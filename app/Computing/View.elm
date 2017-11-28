module Computing.View
    exposing
        ( view
        )

import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Computing exposing (nextTrust)
import Main.Model exposing (Model)
import Main.Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    model.computingModule
        |> Maybe.map
            (\mod ->
                div
                    [ style
                        [ ( "margin", "4px 8px" )
                        , ( "width", "100%" )
                        ]
                    ]
                    [ div
                        [ style [ ( "flex-direction", "column" ) ] ]
                        [ h2 [] [ text "Research" ]
                        , h3 [] [ text ("Trust : " ++ (formatInt usLocale mod.trust)) ]
                        ]
                    , div
                        []
                        [ div []
                            [ text ("Next trust " ++ (formatInt usLocale ((nextTrust mod.trust + 1) * 1000)))
                            ]
                        ]
                    , div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ button
                                [ onClick AddProcessor
                                , disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                                ]
                                [ text "Processors"
                                ]
                            , text (" " ++ (formatInt usLocale mod.processors))
                            ]
                        , div []
                            [ button
                                [ onClick AddMemory
                                , disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                                ]
                                [ text "Memory"
                                ]
                            , text (" " ++ (formatInt usLocale mod.memory))
                            ]
                        ]
                    , div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ text
                                ("Operations : "
                                    ++ (formatInt usLocale (floor mod.operations))
                                    ++ " / "
                                    ++ (formatInt usLocale mod.memoryLimit)
                                )
                            ]
                        , div
                            []
                            [ text ("Creativity : " ++ (formatInt usLocale (floor (mod.creativity |> Maybe.withDefault 0.0))))
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (text "")
