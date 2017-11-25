module Models exposing (..)

import Business.Msg exposing (..)
import Manufacturing.Msg exposing (..)
import Projects.Msg exposing (..)
import Material
import Dict exposing (Dict)
import Time exposing (Time)


type alias BusinessModule =
    { funds : Float
    , inventory : Int
    , price : Float
    , demand : Float
    , demandBoost : Int
    , marketingCost : Int
    , marketingLvl : Int
    , marketingEffectiveness : Int
    }


type alias ManufacturingModule =
    { dough : Int
    , doughSupply : Int
    , doughCost : Int
    , doughBasePrice : Float
    , pasteisModule : Maybe PasteisModule
    , megaPasteisModule : Maybe MegaPasteisModule
    , partialPasteis : Float
    , pasteisMakerRate : Float
    }


type alias PasteisModule =
    { cost : Float
    , boost : Int
    , level : Int
    , factor : Int
    }


type alias MegaPasteisModule =
    { cost : Float
    , boost : Int
    , level : Int
    , factor : Int
    }


type alias ComputingModule =
    { trust : Int
    , processors : Int
    , memory : Int
    , memoryLimit : Int
    , operations : Float
    , creativityEnable : Bool
    , creativity : Float
    }


type alias ProjectsModule =
    { projectsActivated : Dict String Bool
    , projects : List Project
    }


type alias ProjectsModuleSave =
    { projectsActivated : List ( String, Bool )
    }


type alias Project =
    { id : String
    , name : String
    , description : String
    , cost : ProjectCost
    , trigger : Bool
    , effect : Msg
    }


type alias ProjectCost =
    { funds : Int
    , operations : Int
    , creativity : Int
    }


type alias SaveModel =
    { pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    , projectsModule : Maybe ProjectsModuleSave
    }


type alias Model =
    { mdl : Material.Model
    , lastTick : Maybe Time
    , pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    , projectsModule : Maybe ProjectsModule
    }


type Msg
    = Mdl (Material.Msg Msg)
    | BusinessMessage Business.Msg.Msg
    | ManufacturingMessage Manufacturing.Msg.Msg
    | ProjectsMessage Projects.Msg.Msg
    | Tick Time
    | UpdateModel
    | BuyPasteis
    | BuyMegaPasteis
    | AddProcessor
    | AddMemory
    | Reset
    | NewPasteisBaked Int
    | DoughtBought Float
