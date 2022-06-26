module Utils.Models exposing (..)


type alias ThingsThatIBuild =
    { imgUrl : String
    , title : String
    , desc : String
    , list : String
    , projectLink : String
    , repositoryUrl : Maybe String
    , italic : Maybe String
    }


defaultThingsThatIBuild : ThingsThatIBuild
defaultThingsThatIBuild =
    { imgUrl = ""
    , title = ""
    , desc = ""
    , list = ""
    , projectLink = ""
    , repositoryUrl = Nothing
    , italic = Nothing
    }
