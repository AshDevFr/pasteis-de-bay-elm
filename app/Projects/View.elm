module Projects.View
    exposing
        ( view
        )

import Html exposing (..)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Projects.Init exposing (initList)
import Main.Msg exposing (..)
import Main.Model exposing (Model)
import Projects.Model exposing (Project)


projectView : Int -> Model -> Project -> Html Main.Msg.Msg
projectView index model project =
    div []
        [ button
            [ onClick (ActivateProject project)
            , disabled (project.trigger model |> not)
            ]
            [ text project.name ]
        ]


view : Model -> Html Main.Msg.Msg
view model =
    model.projectsModule
        |> Maybe.map
            (\mod ->
                div
                    []
                    [ div
                        []
                        [ div [] [ text "Rewards" ]
                        , div [] [ text "Select your reward" ]
                        ]
                    , div
                        []
                        (List.indexedMap (flip projectView model) initList)
                    ]
            )
        |> Maybe.withDefault (text "")
