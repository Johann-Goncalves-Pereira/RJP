module Utils.View exposing (..)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (attribute, class, type_)
import Html.Attributes.Aria exposing (ariaHidden)


customProps : List { prop : String, value : String } -> Attribute msg
customProps listProps =
    List.foldl
        (\{ prop, value } result ->
            String.concat [ result, "--", prop, ":", value, ";" ]
        )
        ""
        listProps
        |> attribute "style"


customProp : String -> String -> Attribute msg
customProp prop value =
    customProps [ { prop = prop, value = value } ]


materialIcon : String -> String -> Html msg
materialIcon className iconName =
    span
        [ class <| "material-symbols-rounded " ++ className

        {- , ariaHidden True -}
        ]
        [ text iconName ]


button : List (Attribute msg) -> List (Html msg) -> Html msg
button attributes content =
    Html.button (type_ "button" :: attributes) content
