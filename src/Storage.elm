port module Storage exposing
    ( Scheme(..)
    , Storage
    , Theme
    , changeHue
    , fromJson
    , onChange
    )

import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E


port save : D.Value -> Cmd msg


port load : (D.Value -> msg) -> Sub msg


type alias Storage =
    { theme : Theme
    }


type alias Theme =
    { scheme : Scheme, hue : Int }


init : Storage
init =
    { theme = { scheme = Dark, hue = 0 }
    }


type Scheme
    = Dark
    | Light


schemeStr : Scheme -> String
schemeStr scheme =
    case scheme of
        Dark ->
            "dark"

        Light ->
            "light"



-- Converting to JSON


toJson : Storage -> D.Value
toJson storage =
    E.object
        [ ( "theme", encodeTheme storage.theme )
        ]


encodeTheme : Theme -> D.Value
encodeTheme theme =
    E.object
        [ ( "scheme", encodeScheme theme.scheme )
        , ( "hue", E.int theme.hue )
        ]


encodeScheme : Scheme -> E.Value
encodeScheme scheme =
    case scheme of
        Dark ->
            E.string <| schemeStr Dark

        Light ->
            E.string <| schemeStr Light



-- Converting from JSON


fromJson : D.Value -> Storage
fromJson value =
    value
        |> D.decodeValue decoderStorage
        |> Result.withDefault init


decoderStorage : D.Decoder Storage
decoderStorage =
    D.succeed Storage
        |> P.required "theme" decodeTheme


decodeTheme : D.Decoder Theme
decodeTheme =
    D.succeed Theme
        |> P.required "scheme" decodeScheme
        |> P.required "hue" D.int


decodeScheme : D.Decoder Scheme
decodeScheme =
    D.string
        |> D.andThen
            (\strScheme ->
                case strScheme of
                    "light" ->
                        D.succeed Light

                    "dark" ->
                        D.succeed Dark

                    _ ->
                        D.fail "This is a invalid theme"
            )



-- src/Storage.elm


changeHue : Storage -> Scheme -> Int -> Cmd msg
changeHue storage scheme hue =
    { storage | theme = { scheme = scheme, hue = hue } }
        |> toJson
        |> save


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)
