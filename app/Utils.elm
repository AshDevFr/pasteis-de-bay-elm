module Utils exposing (..)

import Material
import Random
import Json.Decode exposing (Decoder, map8, map7, map4, map3, at, field, maybe, int, float, bool)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Models exposing (..)


demandPercentage : Float -> String
demandPercentage demand =
    formatFloat usLocale (demand * 10)


saveToModel : SaveModel -> Model
saveToModel saveModel =
    { mdl =
        Material.model
    , lastTick = Nothing
    , pasteis = saveModel.pasteis
    , businessModule = saveModel.businessModule
    , manufacturingModule = saveModel.manufacturingModule
    , computingModule = saveModel.computingModule
    }


modelToSave : Model -> SaveModel
modelToSave model =
    { pasteis = model.pasteis
    , businessModule = model.businessModule
    , manufacturingModule = model.manufacturingModule
    , computingModule = model.computingModule
    }


randomFloat : Float -> Float -> Random.Seed -> ( Float, Random.Seed )
randomFloat float float2 seed =
    Random.step (Random.float float float2) seed


randomInt : Int -> Int -> Random.Seed -> ( Int, Random.Seed )
randomInt int int2 seed =
    Random.step (Random.int int int2) seed


randomMultipleFloat : Float -> Float -> Int -> Random.Seed -> ( List Float, Random.Seed )
randomMultipleFloat float float2 length seed =
    Random.step (Random.list length (Random.float float float2)) seed


randomMultipleInt : Int -> Int -> Int -> Random.Seed -> ( List Int, Random.Seed )
randomMultipleInt int int2 length seed =
    Random.step (Random.list length (Random.int int int2)) seed


decodeSaveModel : Decoder SaveModel
decodeSaveModel =
    map4 SaveModel
        (field "pasteis" int)
        (field "businessModule" decodeBusinessModule)
        (field "manufacturingModule" decodeManufacturingModule)
        (maybe (field "computingModule" decodeComputingModule))


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
    map3 PasteisModule
        (field "cost" float)
        (field "boost" int)
        (field "level" int)


decodeMegaPasteisModule : Decoder MegaPasteisModule
decodeMegaPasteisModule =
    map3 MegaPasteisModule
        (field "cost" float)
        (field "boost" int)
        (field "level" int)


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
