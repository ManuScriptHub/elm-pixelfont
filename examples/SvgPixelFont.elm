module SvgPixelFont exposing (..)

import Ascii exposing (Grid, Row, Cell, stringToGrids, fromCode, toCode)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Color exposing (..)
import Math.Vector2 exposing (Vec2, vec2, add, scale, getX, getY)
import Color.Convert exposing (colorToHex)
import List.Extra as List
import Char exposing (fromCode)
import String exposing (fromChar)


type alias Pixel =
    { color : Color
    , pos : Vec2
    }


letterSize : Vec2
letterSize =
    vec2 80 130


pixelSize : Vec2
pixelSize =
    vec2 (getX letterSize / 8) (getY letterSize / 13)


main : Html a
main =
    let
        all =
            List.map (String.fromChar << Char.fromCode) <|
                List.range Ascii.fromCode Ascii.toCode
    in
        Svg.svg
            [ SA.width "1920"
            , SA.height "1080"
            , SA.viewBox "-20 -20 1920 1080"
            ]
            (List.greedyGroupsOf 13 all
                |> List.map (String.join "")
                |> List.indexedMap toSvg
            )


offX off =
    vec2 (getX letterSize * off) 0


offY off =
    vec2 0 (getY letterSize * off)


toSvg : Int -> String -> Svg a
toSvg offRow str =
    Ascii.stringToGrids str
        |> List.indexedMap viewGrid
        |> Svg.g [ translate <| offY (toFloat offRow) ]


viewGrid : Int -> Grid -> Svg a
viewGrid off rows =
    List.map viewRow rows
        |> List.concat
        |> Svg.g [ translate <| (offX <| 1.1 * toFloat off) ]


viewRow : Row -> List (Svg b)
viewRow cells =
    List.filterMap identity cells
        |> List.map (viewPixel << createPixel)


viewPixel : Pixel -> Svg a
viewPixel item =
    Svg.g [ translate item.pos ] [ Svg.rect (attrs <| rect item) [] ]



--Svg.g [ translate item.pos ] [ Svg.circle (attrs <| circle item) [] ]


createPixel : ( Int, Int ) -> Pixel
createPixel ( row, col ) =
    { color =
        List.getAt ((row * 8 + col) % List.length colors) colors
            |> Maybe.withDefault black
    , pos = vec2 (toFloat col * getX pixelSize) (toFloat row * getY pixelSize)
    }


translate : Vec2 -> Svg.Attribute b
translate pos =
    SA.transform <| "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


circle : Pixel -> List ( String -> Svg.Attribute b, String )
circle model =
    [ ( SA.cx, "0" )
    , ( SA.cy, "0" )
    , ( SA.r, toString <| getX pixelSize )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    , ( SA.strokeWidth, "0.5" )
    ]


rect : Pixel -> List ( String -> Svg.Attribute b, String )
rect model =
    [ ( SA.x, "0" )
    , ( SA.y, "0" )
    , ( SA.width, toString <| getX pixelSize )
    , ( SA.height, toString <| getY pixelSize )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    , ( SA.strokeWidth, "0.5" )
    ]


colors : List Color
colors =
    [ red
    , orange
    , yellow
    , green
    , blue
    , purple
    , brown
    , lightRed
    , lightOrange
    , lightYellow
    , lightGreen
    , lightBlue
    , lightPurple
    , lightBrown
    , darkRed
    , darkOrange
    , darkYellow
    , darkGreen
    , darkBlue
    , darkPurple
    , darkBrown
    , white
    , lightGrey
    , grey
    , darkGrey
    , lightCharcoal
    , charcoal
    , darkCharcoal
    , black
    ]
