module Projects.View
    exposing
        ( view
        )

import Models
import Models exposing (..)
import Html exposing (Html, text)
import Material.Card as Card
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Grid exposing (Cell, grid, cell, size, Device(..), Align(..), align)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Projects exposing (initList)


projectView : Int -> Model -> Project -> Cell Models.Msg
projectView index model project =
    cell [ size All 12 ]
        [ Button.render Mdl
            [ index, 1 ]
            model.mdl
            [ Button.colored
            , Button.ripple
            , Options.onClick (ActivateProject project)
            , Options.disabled (project.trigger model |> not)
            ]
            [ text project.name ]
        ]


view : Model -> Html Models.Msg
view model =
    case model.projectsModule of
        Nothing ->
            text ""

        Just mod ->
            Card.view
                [ Elevation.e2
                , css "margin" "4px 8px"
                , css "width" "100%"
                ]
                [ Card.title
                    [ css "flex-direction" "column" ]
                    [ Card.head [] [ text "Rewards" ]
                    , Card.subhead [] [ text "Select your reward" ]
                    ]
                , Card.actions
                    [ Color.text Color.black ]
                    [ grid []
                        (List.indexedMap (flip projectView model) Projects.initList)
                    ]
                ]
