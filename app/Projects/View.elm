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


projectCostView : ProjectCost -> Html Main.Msg.Msg
projectCostView cost =
    div []
        [ span [ style [ ( "margin-right", "5px" ) ] ] [ text ("$" ++ (formatInt usLocale cost.funds)) ]
        , b [] [ text ", " ]
        , span [ style [ ( "margin-right", "5px" ) ] ] [ text ((formatInt usLocale cost.operations) ++ " operations") ]
        , b [] [ text ", " ]
        , span [ style [ ( "margin-right", "5px" ) ] ] [ text ((formatInt usLocale cost.creativity) ++ " creativity") ]
        , b [] [ text ", " ]
        , span [ style [ ( "margin-right", "5px" ) ] ] [ text ((formatInt usLocale cost.trust) ++ " trust") ]
        ]


projectView : Int -> Model -> Project -> Html Main.Msg.Msg
projectView index model project =
    div
        [ style
            [ ( "border", "1px solid" )
            , ( "margin", "5px 0 " )
            ]
        ]
        [ h3 [] [ text project.name ]
        , h5 [] [ text project.description ]
        , projectCostView project.cost
        , button
            [ onClick (ActivateProject project)
            , disabled (canBeBought model project.cost)
            ]
            [ text "Buy" ]
        ]


view : Model -> Html Main.Msg.Msg
view model =
    model.projectsModule
        |> Maybe.map
            (\mod ->
                div
                    [ style [ ( "margin-top", "10px" ) ] ]
                    [ div
                        []
                        [ h2 [] [ text "Rewards" ]
                        , h3 [] [ text "Select your reward" ]
                        ]
                    , div
                        []
                        (List.indexedMap (flip projectView model) initList)
                    ]
            )
        |> Maybe.withDefault (text "")


canBeBought : Model -> ProjectCost -> Bool
canBeBought model cost =
    let
        enoughFunds =
            model.businessModule.funds >= (toFloat cost.funds)

        computingModule =
            Maybe.withDefault
                { trust = 0
                , processors = 0
                , memory = 0
                , memoryLimit = 0
                , operations = 0
                , creativity = Just 0
                }
                model.computingModule

        creativity =
            Maybe.withDefault 0 computingModule.creativity

        enoughOperations =
            computingModule.operations >= toFloat cost.operations

        enoughCreativity =
            creativity >= toFloat cost.creativity

        enoughTrust =
            computingModule.trust >= cost.trust
    in
        not (enoughFunds && enoughOperations && enoughCreativity && enoughTrust)
