module BitGrid exposing (..)

import Ascii exposing (charToBinaryStrings, replaceChars)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List exposing (map)
import String exposing (toList)


main : Html a
main =
    toList "{- Hello elm, good to see you! -}"
        |> map letterToBitGrid
        |> div
            [ style
                [ (,) "display" "flex"
                , (,) "font-family" "monospace"
                ]
            ]


letterToBitGrid : Char -> Html a
letterToBitGrid letter =
    div
        [ style [ (,) "flex" "auto" ]
        ]
        (charToBinaryStrings letter
            |> replaceChars ( '.', '@' )
            |> map (\x -> div [] [ text x ])
        )
