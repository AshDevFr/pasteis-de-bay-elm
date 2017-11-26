module Views.Main exposing (view)

import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Html exposing (Html, div, button, text, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Business as Business
import Manufacturing exposing (..)
import Manufacturing.Msg exposing (..)
import Main.Model exposing (Model)
import Main.Msg exposing (..)
import Computing as Computing
import Projects.View


header : Int -> Html msg
header pasteis =
    h1 []
        [ text ("Pasteis " ++ (formatInt usLocale pasteis))
        ]


makePasteisView : Model -> Html Main.Msg.Msg
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


businessAndMaufacturingView : Model -> Html Main.Msg.Msg
businessAndMaufacturingView model =
    div [ style [ ( "float", "left" ) ] ]
        [ Business.view model
        , Manufacturing.view model
        ]


computingAndProjectsView : Model -> Html Main.Msg.Msg
computingAndProjectsView model =
    div
        [ style
            [ ( "float", "left" )
            , ( "margin", "30px" )
            ]
        ]
        [ Computing.view model
        , Projects.View.view model
        ]


view : Model -> Html Main.Msg.Msg
view model =
    div []
        [ header model.pasteis
        , div []
            [ makePasteisView model
            , businessAndMaufacturingView model
            , computingAndProjectsView model
            ]
        ]
