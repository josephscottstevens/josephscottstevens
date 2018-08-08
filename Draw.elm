module Draw exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import State exposing (..)
import Set exposing (Set)
import Piece exposing (..)


game : State -> Html msg
game state =
    div []
        [ renderOutline
            |> pxSize
        , renderBoard state.currentPiece state.currentPiecePosition (Set.toList state.fixatedBlocks)
            |> pxSize
        , renderNext state.nextPiece
            |> pxSize
        , pixelWithItems
            { x = (blockSize * 10) + 1
            , y = blockSize * 0
            , width = (blockSize * 5) + 1
            , height = blockSize * 1
            , color = "white"
            }
            [ div
                [ style
                    [ ( "margin-top", "5px" )
                    , ( "margin-left", px (blockSize * 1) )
                    ]
                ]
                [ text ("SCORE: " ++ (toString state.currentScore))
                ]
            ]
        ]


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
                , y = ((y + 1) * blockSize)
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
    toString i ++ "px"


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
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "left", px (x + offsetX) )
            , ( "top", px (y + offsetY) )
            , ( "position", "absolute" )
            , ( "background", color )
            , ( "outline-width", "1px" )
            , ( "outline-style", "solid" )
            , ( "outline-color", "black" )
            ]
        ]
        children
