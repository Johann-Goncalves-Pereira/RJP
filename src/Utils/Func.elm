module Utils.Func exposing (aplR, regex, regexValidate)

import Regex


aplR : a -> (a -> b) -> b
aplR f x =
    x f


regex : String -> Regex.Regex
regex =
    Regex.fromString
        >> Maybe.withDefault Regex.never


regexValidate : String -> String -> Bool
regexValidate pattern_ =
    Regex.contains <| regex pattern_
