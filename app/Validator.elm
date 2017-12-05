module Validator
    exposing
        ( decodeSaveModel
        , saveToModel
        , modelToSave
        )

import Dict exposing (toList, fromList)
import Projects.Module.ProjectsModule exposing (ProjectsModule, ProjectsModuleSave)
import Business.Model exposing (BusinessModule, StatsModule)
import Manufacturing.Model exposing (ManufacturingModule, PasteisModule, MegaPasteisModule)
import Computing.Model exposing (ComputingModule)
import Main.Model exposing (SaveModel, Model)
import Json.Decode
    exposing
        ( Decoder
        , map8
        , map7
        , map6
        , map5
        , map4
        , map3
        , map2
        , map
        , at
        , field
        , maybe
        , int
        , float
        , bool
        , dict
        , list
        , string
        , index
        , nullable
        )
import Json.Decode.Pipeline exposing (decode, required)


saveToModel : SaveModel -> Model
saveToModel saveModel =
    { lastTick = Nothing
    , pasteis = saveModel.pasteis
    , businessModule = saveModel.businessModule
    , manufacturingModule = saveModel.manufacturingModule
    , computingModule = saveModel.computingModule
    , projectsModule = saveToProjectModule saveModel.projectsModule
    }


modelToSave : Model -> SaveModel
modelToSave model =
    { pasteis = model.pasteis
    , businessModule = model.businessModule
    , manufacturingModule = model.manufacturingModule
    , computingModule = model.computingModule
    , projectsModule = projectModuleToSave model.projectsModule
    }


projectModuleToSave : Maybe ProjectsModule -> Maybe ProjectsModuleSave
projectModuleToSave =
    Maybe.map (\mod -> { projectsActivated = toList mod.projectsActivated })


saveToProjectModule : Maybe ProjectsModuleSave -> Maybe ProjectsModule
saveToProjectModule =
    Maybe.map
        (\mod ->
            { projectsActivated = fromList mod.projectsActivated
            , projectsEnabled = Dict.empty
            }
        )


decodeSaveModel : Decoder SaveModel
decodeSaveModel =
    map5 SaveModel
        (field "pasteis" int)
        (field "businessModule" decodeBusinessModule)
        (field "manufacturingModule" decodeManufacturingModule)
        (maybe (field "computingModule" decodeComputingModule))
        (maybe (field "projectsModule" decodeProjectsModule))


decodeBusinessModule : Decoder BusinessModule
decodeBusinessModule =
    decode BusinessModule
        |> required "funds" float
        |> required "inventory" int
        |> required "price" float
        |> required "demand" float
        |> required "demandBoost" float
        |> required "marketingCost" int
        |> required "marketingLvl" int
        |> required "marketingEffectiveness" float
        |> required "statsModule" (nullable decodeStatsModule)


decodeStatsModule : Decoder StatsModule
decodeStatsModule =
    decode StatsModule
        |> required "revPerSec" float
        |> required "salesPerSec" float
        |> required "lastSales" (list decoderLastSells)


decoderLastSells : Decoder ( Int, Float )
decoderLastSells =
    map2 (,) (index 0 int) (index 1 float)


decodeManufacturingModule : Decoder ManufacturingModule
decodeManufacturingModule =
    decode ManufacturingModule
        |> required "dough" int
        |> required "doughSupply" float
        |> required "doughCost" int
        |> required "doughBasePrice" float
        |> required "doughAutoBuy" (nullable bool)
        |> required "pasteisModule" (nullable decodePasteisModule)
        |> required "megaPasteisModule" (nullable decodeMegaPasteisModule)
        |> required "partialPasteis" float
        |> required "pasteisMakerRate" float


decodePasteisModule : Decoder PasteisModule
decodePasteisModule =
    map4 PasteisModule
        (field "cost" float)
        (field "boost" float)
        (field "level" int)
        (field "factor" int)


decodeMegaPasteisModule : Decoder MegaPasteisModule
decodeMegaPasteisModule =
    map4 MegaPasteisModule
        (field "cost" float)
        (field "boost" float)
        (field "level" int)
        (field "factor" int)


decodeComputingModule : Decoder ComputingModule
decodeComputingModule =
    map7 ComputingModule
        (field "trust" int)
        (field "trustLevel" int)
        (field "processors" int)
        (field "memory" int)
        (field "memoryLimit" int)
        (field "operations" float)
        (maybe (field "creativity" float))


decodeProjectsModule : Decoder ProjectsModuleSave
decodeProjectsModule =
    map ProjectsModuleSave
        (field "projectsActivated" (list decoderProject))


decoderProject : Decoder ( String, Bool )
decoderProject =
    map2 (,) (index 0 string) (index 1 bool)
