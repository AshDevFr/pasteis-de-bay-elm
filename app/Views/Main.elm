module Views.Main exposing (view)

import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Html exposing (Html, div, button, text, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Models exposing (..)
import Business as Business
import Manufacturing exposing (..)
import Manufacturing.Msg exposing (..)
import Computing as Computing
import Projects as Projects


header : Int -> Html msg
header pasteis =
    h1 []
        [ text ("Pasteis " ++ (formatInt usLocale pasteis))
        ]


makePasteisView : Model -> Html Models.Msg
makePasteisView model =
    div []
        [ div []
            [ button
                [ onClick (ManufacturingMessage (Manufacturing.Msg.BakePastel 1))
                , disabled (model.manufacturingModule.dough < 1)
                ]
                [ text "Make a Pastel" ]
            , button
                [ onClick Reset
                , style [ ( "margin-left", "10px" ) ]
                ]
                [ text "Reset" ]
            ]
        ]


businessAndMaufacturingView : Model -> Html Models.Msg
businessAndMaufacturingView model =
    div []
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-flow", "row wrap" )
                , ( "align-items", "flex-end" )
                , ( "margin-top", "20px" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-flow", "row wrap" )
                    , ( "justify-content", "space-between" )
                    , ( "align-items", "center" )
                    , ( "min-width", "256px" )
                    , ( "flex", "1 1 auto" )
                    ]
                ]
                [ Business.view model
                , Manufacturing.view model
                ]
            ]
        ]


computingView : Model -> Html Models.Msg
computingView model =
    div []
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-flow", "row wrap" )
                , ( "align-items", "flex-end" )
                , ( "margin-top", "20px" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-flow", "row wrap" )
                    , ( "justify-content", "space-between" )
                    , ( "align-items", "center" )
                    , ( "min-width", "256px" )
                    , ( "flex", "1 1 auto" )
                    ]
                ]
                [ Computing.view model
                ]
            ]
        ]


projectsView : Model -> Html Models.Msg
projectsView model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-flow", "row wrap" )
            , ( "align-items", "flex-end" )
            , ( "margin-top", "20px" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-flow", "row wrap" )
                , ( "justify-content", "space-between" )
                , ( "align-items", "center" )
                , ( "min-width", "256px" )
                , ( "flex", "1 1 auto" )
                ]
            ]
            []
        ]


view : Model -> Html Models.Msg
view model =
    div []
        [ header model.pasteis
        , div []
            [ makePasteisView model
            , businessAndMaufacturingView model
            , computingView model
            , projectsView model
            ]
        ]
