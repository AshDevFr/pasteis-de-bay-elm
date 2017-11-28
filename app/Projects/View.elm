module Projects.View
    exposing
        ( view
        )

import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Projects.Init exposing (initList)
import Main.Msg exposing (..)
import Main.Model exposing (Model)
import Projects.Model exposing (Project, ProjectCost)
import Projects.Update exposing (buy)
import Result.Extra exposing (isErr)


projectCostView : ProjectCost -> Html Main.Msg.Msg
projectCostView cost =
    let
        funds =
            costView "funds" (floor cost.funds)

        operations =
            costView "operations" (floor cost.operations)

        creativity =
            costView "creativity" (floor cost.creativity)

        trust =
            costView "trust" cost.trust

        costs =
            List.filter (\s -> not (String.isEmpty s)) [ funds, operations, creativity, trust ]
    in
        span [ style [ ( "font-size", "0.9em" ) ] ] [ text (" (" ++ (String.join ", " costs) ++ ")") ]


costView : String -> Int -> String
costView currency amount =
    case amount of
        0 ->
            ""

        _ ->
            case currency of
                "funds" ->
                    "$ " ++ (formatInt usLocale amount)

                _ ->
                    (formatInt usLocale amount) ++ " " ++ currency


projectView : Int -> Model -> Project -> Html Main.Msg.Msg
projectView index model project =
    div []
        [ button
            [ style
                [ ( "outline", "none" )
                , ( "background", "lightgray" )
                , ( "border", "1px solid" )
                , ( "margin", "0 0 15px" )
                , ( "padding", "10px" )
                , ( "text-align", "center" )
                , ( "width", "400px" )
                , ( "font-size", "1em" )
                ]
            , onClick (ActivateProject project)
            , disabled (buy model project.cost |> isErr)
            ]
            [ div []
                [ span
                    [ style
                        [ ( "font-size", "1em" )
                        , ( "font-weight", "bold" )
                        ]
                    ]
                    [ text project.name ]
                , projectCostView project.cost
                ]
            , div []
                [ span
                    [ style
                        [ ( "font-size", "0.9em" )
                        ]
                    ]
                    [ text project.description ]
                ]
            ]
        ]


view : Model -> Html Main.Msg.Msg
view model =
    model.projectsModule
        |> Maybe.map
            (\mod ->
                div
                    [ style
                        [ ( "margin-top", "10px" )
                        ]
                    ]
                    [ div
                        []
                        [ h2 [] [ text "Rewards" ]
                        , h3 [] [ text "Select your reward" ]
                        ]
                    , div
                        []
                        (List.indexedMap (flip projectView model) (initList mod))
                    ]
            )
        |> Maybe.withDefault (text "")
