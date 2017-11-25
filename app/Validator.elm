module Validator
    exposing
        ( decodeSaveModel
        , saveToModel
        , modelToSave
        )

import Dict exposing (toList, fromList)
import Material
import Projects as Projects
import Json.Decode
    exposing
        ( Decoder
        , map8
        , map7
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
        )
import Models exposing (..)


saveToModel : SaveModel -> Model
saveToModel saveModel =
    { mdl =
        Material.model
    , lastTick = Nothing
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
projectModuleToSave projectsModule =
    case projectsModule of
        Nothing ->
            Nothing

        Just mod ->
            Just { projectsActivated = toList mod.projectsActivated }


saveToProjectModule : Maybe ProjectsModuleSave -> Maybe ProjectsModule
saveToProjectModule saveModel =
    case saveModel of
        Nothing ->
            Nothing

        Just mod ->
            Just
                { projectsActivated = fromList mod.projectsActivated
                , projects = Projects.initList
                }


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
    map8 BusinessModule
        (field "funds" float)
        (field "inventory" int)
        (field "price" float)
        (field "demand" float)
        (field "demandBoost" int)
        (field "marketingCost" int)
        (field "marketingLvl" int)
        (field "marketingEffectiveness" int)


decodeManufacturingModule : Decoder ManufacturingModule
decodeManufacturingModule =
    map8 ManufacturingModule
        (field "dough" int)
        (field "doughSupply" int)
        (field "doughCost" int)
        (field "doughBasePrice" float)
        (maybe (field "pasteisModule" decodePasteisModule))
        (maybe (field "megaPasteisModule" decodeMegaPasteisModule))
        (field "partialPasteis" float)
        (field "pasteisMakerRate" float)


decodePasteisModule : Decoder PasteisModule
decodePasteisModule =
    map4 PasteisModule
        (field "cost" float)
        (field "boost" int)
        (field "level" int)
        (field "factor" int)


decodeMegaPasteisModule : Decoder MegaPasteisModule
decodeMegaPasteisModule =
    map4 MegaPasteisModule
        (field "cost" float)
        (field "boost" int)
        (field "level" int)
        (field "factor" int)


decodeComputingModule : Decoder ComputingModule
decodeComputingModule =
    map7 ComputingModule
        (field "trust" int)
        (field "processors" int)
        (field "memory" int)
        (field "memoryLimit" int)
        (field "operations" float)
        (field "creativityEnable" bool)
        (field "creativity" float)


decodeProjectsModule : Decoder ProjectsModuleSave
decodeProjectsModule =
    map ProjectsModuleSave
        (field "projectsActivated" (list decoderProject))


decoderProject : Decoder ( String, Bool )
decoderProject =
    map2 (,) (index 0 string) (index 1 bool)
