module Main exposing (..)

import Ascii exposing (..)
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, vec2)
import Svg exposing (Svg)
import Svg.Attributes as SA


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Pixel =
    { pos : Vec2
    }


type alias Model =
    { input : String
    , grids : List Grid
    }


type Msg
    = Input String


letterSize : Vec2
letterSize =
    vec2 80 130


pixelSize : Vec2
pixelSize =
    vec2 (getX letterSize / Tuple.first letterDim) (getY letterSize / Tuple.second letterDim)


letterDim : ( Float, Float )
letterDim =
    ( 8, 13 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            ( { model
                | input = str
                , grids = Ascii.stringToGrids str
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ input [ HA.type_ "text", HE.onInput Input ] []
        , viewResult model

        --, text <| toString model
        ]


viewResult : Model -> Html Msg
viewResult model =
    let
        cursor =
            ( 0, 0 )
    in
    Svg.svg
        [ SA.width "1920"
        , SA.height "1080"
        , SA.viewBox "-20 -20 1920 1080"
        ]
        (List.foldl viewGrid ( cursor, [] ) model.grids |> Tuple.second)


viewGrid : Grid -> ( ( Float, Float ), List (Svg Msg) ) -> ( ( Float, Float ), List (Svg Msg) )
viewGrid grid ( ( x, y ), list ) =
    let
        gs =
            List.concat grid
                |> List.filterMap identity
                |> List.map (\( px, py ) -> Svg.g [ translate ( toFloat py * getX pixelSize, toFloat px * getY pixelSize ) ] [ Svg.circle (attrs circle) [] ])

        newPos =
            ( x + getX letterSize + 1, y )

        nextPos =
            if Tuple.first newPos < 900 then
                newPos
            else
                ( 0, y + getY letterSize )
    in
    ( nextPos, Svg.g [ translate ( x, y ) ] gs :: list )


rect : List ( String -> Svg.Attribute b, String )
rect =
    [ ( SA.x, "0" )
    , ( SA.y, "0" )
    , ( SA.width, toString <| getX pixelSize )
    , ( SA.height, toString <| getY pixelSize )
    , ( SA.fill, "black" )
    , ( SA.stroke, "black" )
    , ( SA.strokeWidth, "0.5" )
    ]


circle : List ( String -> Svg.Attribute b, String )
circle =
    [ ( SA.cx, "0" )
    , ( SA.cy, "0" )
    , ( SA.r, toString <| getX pixelSize / 2 )
    , ( SA.fill, "black" )
    , ( SA.stroke, "black" )
    , ( SA.strokeWidth, "0.5" )
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


translate : ( Float, Float ) -> Svg.Attribute b
translate ( x, y ) =
    SA.transform <| "translate (" ++ toString x ++ "," ++ toString y ++ ")"



-- viewGrid : Int -> Grid -> Svg a
-- viewGrid off rows =
--     List.map viewRow rows
--         |> List.concat
--         |> Svg.g [ translate <| (offX <| 1.1 * toFloat off) ]
--
--
-- viewRow : Row -> List (Svg b)
-- viewRow cells =
--     List.filterMap identity cells
--         |> List.map (viewPixel << createPixel)
--
--
-- viewPixel : Pixel -> Svg a
-- viewPixel item =
--     Svg.g [ translate item.pos ] [ Svg.rect (attrs <| rect item) [] ]
--
--
--
-- --Svg.g [ translate item.pos ] [ Svg.circle (attrs <| circle item) [] ]
--
--
-- createPixel : ( Int, Int ) -> Pixel
-- createPixel ( row, col ) =
--     { pos = vec2 (toFloat col * getX pixelSize) (toFloat row * getY pixelSize)
--     }
--
--
--
--
--
--
--
--
--
-- offX : Float -> Vec2
-- offX off =
--     vec2 (getX letterSize * off) 0
--
--
-- offY : Float -> Vec2
-- offY off =
--     vec2 0 (getY letterSize * off)
