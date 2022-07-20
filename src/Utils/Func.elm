module Utils.Func exposing
    ( aplR
    , getPattern
    , regex
    , regexValidate
    , run
    , Patterns(..)
    )

import Dict exposing (Dict)
import Regex
import Task


aplR : a -> (a -> b) -> b
aplR f x =
    x f


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


regex : String -> Regex.Regex
regex =
    Regex.fromString
        >> Maybe.withDefault Regex.never


regexValidate : String -> String -> Bool
regexValidate pattern_ =
    Regex.contains <| regex pattern_



-- patterns : Dict String String -> String


type Patterns
    = Email


patterns : Patterns -> String
patterns pattern =
    case pattern of
        Email ->
            "email"


dictPatterns : Dict String String
dictPatterns =
    List.map2
        (\n p ->
            ( n, p )
        )
        [ patterns Email ]
        [ "^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,8}$" ]
        |> Dict.fromList


getPattern : Patterns -> String
getPattern p =
    dictPatterns
        |> Dict.get (patterns p)
        |> Maybe.withDefault ""
