module Draw exposing (pixel, pixelWithItems, px, pxSize, renderBoard, renderNext, renderOutline)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Piece exposing (..)
import State exposing (..)


renderOutline : List Block
renderOutline =
    let
        boardOutline =
            { x = 0
            , y = 0
            , width = 10 * blockSize
            , height = 20 * blockSize
            , color = "white"
            }

        nextPieceOutline =
            { x = (10 * blockSize) + 1
            , y = blockSize * 1
            , width = blockSize * 5
            , height = blockSize * 5
            , color = "white"
            }
    in
    [ boardOutline, nextPieceOutline ]


renderNext : Piece -> List Block
renderNext nextPiece =
    getBlocks nextPiece
        |> List.map
            (\( x, y ) ->
                { x = ((x + 11) * blockSize) + 1
                , y = (y + 1) * blockSize
                , width = blockSize
                , height = blockSize
                , color = getColor nextPiece
                }
            )


renderBoard : Piece -> ( Int, Int ) -> List ( Int, Int, String ) -> List Block
renderBoard currentPiece ( curX, curY ) fixatedBlocks =
    let
        currentBlock : List Block
        currentBlock =
            getBlocks currentPiece
                |> List.map
                    (\( x, y ) ->
                        { x = (x + curX) * blockSize
                        , y = (y + curY) * blockSize
                        , width = blockSize
                        , height = blockSize
                        , color = getColor currentPiece
                        }
                    )

        blocks : List Block
        blocks =
            fixatedBlocks
                |> List.map
                    (\( x, y, color ) ->
                        { x = x * blockSize
                        , y = y * blockSize
                        , width = blockSize
                        , height = blockSize
                        , color = color
                        }
                    )
    in
    currentBlock ++ blocks



-- helper functions


px : Int -> String
px i =
    String.fromInt i ++ "px"


pixel : Block -> Html msg
pixel block =
    pixelWithItems block []


pxSize : List Block -> Html msg
pxSize items =
    items
        |> List.map pixel
        |> div []


pixelWithItems : Block -> List (Html msg) -> Html msg
pixelWithItems { x, y, width, height, color } children =
    div
        [ style "width" (String.fromInt width ++ "px")
        , style "height" (String.fromInt height ++ "px")
        , style "left" (px (x + offsetX))
        , style "top" (px (y + offsetY))
        , style "position" "absolute"
        , style "background" color
        , style "outline-width" "1px"
        , style "outline-style" "solid"
        , style "outline-color" "black"
        ]
        children
