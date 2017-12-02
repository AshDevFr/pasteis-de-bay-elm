module Projects.Data.Trust exposing (allProjects)

import Projects.Model exposing (Project, ProjectCost)
import Projects.Msg exposing (Msg(..))
import Projects.Data.Utils as Utils


allProjects : List Project
allProjects =
    [ trustBoost1
    , trustBoost2
    , trustBoost3
    , trustBoost4
    , trustBoost5
    , trustBoost6
    ]


trustBoost1 : Project
trustBoost1 =
    let
        projectCost =
            ProjectCost 0 0 10 0
    in
        { id = "trustBoost1"
        , name = "Limerick"
        , description = "Algorithmically-generated poem (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            case computingModule.creativity of
                                Nothing ->
                                    False

                                Just crea ->
                                    True
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }


trustBoost2 : Project
trustBoost2 =
    let
        projectCost =
            ProjectCost 0 0 50 0
    in
        { id = "trustBoost2"
        , name = "Lexical Processing"
        , description = "Gain ability to interpret and understand human language (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            computingModule.creativity
                                |> Maybe.map (\creativity -> creativity > 50)
                                |> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }


trustBoost3 : Project
trustBoost3 =
    let
        projectCost =
            ProjectCost 0 0 100 0
    in
        { id = "trustBoost3"
        , name = "Combinatory Harmonics"
        , description = "Daisy, Daisy, give me your answer do... (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            computingModule.creativity
                                |> Maybe.map (\creativity -> creativity > 100)
                                |> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }


trustBoost4 : Project
trustBoost4 =
    let
        projectCost =
            ProjectCost 0 0 150 0
    in
        { id = "trustBoost4"
        , name = "The Hadwiger Problem"
        , description = "Cubes within cubes within cubes... (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            computingModule.creativity
                                |> Maybe.map (\creativity -> creativity > 150)
                                |> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }


trustBoost5 : Project
trustBoost5 =
    let
        projectCost =
            ProjectCost 0 0 200 0
    in
        { id = "trustBoost5"
        , name = "The Tóth Sausage Conjecture"
        , description = "Tubes within tubes within tubes... (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            computingModule.creativity
                                |> Maybe.map (\creativity -> creativity > 200)
                                |> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }


trustBoost6 : Project
trustBoost6 =
    let
        projectCost =
            ProjectCost 0 0 250 0
    in
        { id = "trustBoost6"
        , name = "Donkey Space"
        , description = "I think you think I think you think I think you think I think... (+1 Trust)"
        , trigger =
            (\model ->
                model.computingModule
                    |> Maybe.map
                        (\computingModule ->
                            computingModule.creativity
                                |> Maybe.map (\creativity -> creativity > 250)
                                |> Maybe.withDefault False
                        )
                    |> Maybe.withDefault False
            )
        , effect =
            List.append (Utils.costEffectList projectCost)
                [ MapTrust (flip (+) 1)
                ]
        , cost = projectCost
        }
